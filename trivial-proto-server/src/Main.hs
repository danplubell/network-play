{-# LANGUAGE OverloadedStrings #-}
module Main where
{-Simple version that uses Bytestrings to send and receive messages
-}
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix      (fix)
import           Data.Binary
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as BL
import           Network.Socket
import           Protocol
import           System.Environment

main :: IO ()
main = do
  (port:_) <- getArgs
  let portNumber = read port::Int
  --create a socket
  sock  <- socket AF_INET Stream 0

  --create a channel
  chan <- newChan::IO (Chan PayLoadMsg)
  --make socket immediately resuable
  setSocketOption sock ReuseAddr 1

  -- listen on TCP port 4242
  bindSocket sock (SockAddrInet (fromIntegral portNumber)  iNADDR_ANY)

  -- allow as maximum of 2 outstanding connections
  listen sock 2

  _ <- forkIO $ fix $ \loop -> do
          _ <- readChan chan
          loop
         -- start main processing loop
  mainLoop sock chan 0

mainLoop::Socket -> Chan PayLoadMsg -> Int-> IO ()
mainLoop sock chan nr = do
    conn <- accept sock
    putStrLn $ "New connection: " ++ show conn
    _ <- forkIO (runConn conn chan nr)
    mainLoop sock chan $! nr+1

-- runConn duplicates the channel and reads from it
-- this runs for each connection to the server
-- When a connection is made the user is asked for a name
-- When an exception occurs the thread is killed
-- When the user enter "quit" the thread is killed
runConn:: (Socket,SockAddr)->Chan PayLoadMsg ->Int ->IO ()
runConn (sock, _) chan nr = do
            queue <- newTQueueIO
            --receiver thread reads from socket and writes to queue
            receiver <- startReceiver sock queue


            let broadcast msg  = writeChan chan (ChatMsg nr msg) --broadcast the message to the channel
            sendMsg sock (mkMsg GreetReq (GreetMsg "Hi what's your name?"))
            nameMsg <- recvMsgFromQueue queue
            case toEnum (msgType nameMsg) of
              GreetResp -> do
                 let name = grtMsg (decode $ BL.fromStrict (msgBody nameMsg)::PayLoadMsg)

                 putStrLn $ "New Connection: " ++ show nr ++ " Name: " ++ BS.unpack name
                 broadcast (BS.concat ["--> ",name," entered."])
                 sendMsg sock (mkMsg GreetReq (GreetMsg (BS.concat ["Welcome, ", name, "!"]) ))
                 --duplicated channel
                 -- new channel is empty
                 -- Writes to read from duplicated channels, but not removed from other channels
                 -- example of multicast
                 chan' <- dupChan chan
                 -- create reader thread, we'll kill it later
                 -- need thread id to kill it
                 reader' <- forkIO $ fix $ \loop -> do -- read message, but not our own messages
                   chatMsg <- readChan chan'
                   when (nr /= chtMsgId chatMsg) $ sendMsg sock (mkMsg Chat chatMsg)
                   loop
                -- handle an exception or quit when the user enters "quit"
                -- This loop reads chat message from queue
                 handle (\(SomeException _)-> return ()) $ fix $ \loop -> do

                     chatMsg' <- recvMsgFromQueue queue
                     case toEnum (msgType chatMsg') of
                       Shutdown  -> putStrLn "received shutdown msg..."
                       Chat      -> do
                         let lineMsg = decode (BL.fromStrict $ msgBody chatMsg')::PayLoadMsg
                         case BS.unpack (chtMsg lineMsg) of
                            "quit"  -> sendMsg sock (mkMsg GreetReq (GreetMsg (BS.pack "Bye!")))
                            []      -> return () -- zero length tcp messages means client closed their side
                            _       -> do
                                      broadcast (BS.concat [name, ": ", chtMsg lineMsg])
                                      loop
                       msgT@_        -> putStrLn $ "Unexpected Chat Message Type (connection will die): "
                                       ++ show msgT

                 putStrLn $ "Killing thread: " ++ show reader'
                 killThread reader'
                 broadcast (BS.concat ["<-- ",  name ,  " left."])
              msgT@_     -> putStrLn $  "Unexpected Greeting Message Type (Connection will die): " ++ show msgT

            putStrLn $ "Killing receiver thread: " ++ show receiver
            killThread receiver
            close  sock


