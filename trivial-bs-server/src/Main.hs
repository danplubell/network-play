{-# LANGUAGE OverloadedStrings #-}
module Main where
{-Simple version that uses Bytestrings to send and receive messages
In this version the messages are short and will fit in a single message
-}
{-Add concurrency to main loop-}
{-Add comminication between threads-}
{-Fixed in this new version:
  This version has a memory leak because the originla thread is not read
  Closing connection isn't handled well
  Need to add exception handling
-}
{-
Add ability to echo back to the user that initiated the message
Associate connection with a name
Add exception handling
Fixed memory leak
-}
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix         (fix)
import qualified Data.ByteString.Char8     as BS
import           Network.Socket
import qualified Network.Socket.ByteString as NB

type Msg = (Int,BS.ByteString)

main :: IO ()
main = do
  --create a socket
  sock  <- socket AF_INET Stream 0

  --create a channel
  chan <- newChan::IO (Chan Msg)
  --make socket immediately resuable
  setSocketOption sock ReuseAddr 1

  -- listen on TCP port 4242
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY)

  -- allow as maximum of 2 outstanding connections
  listen sock 2

  _ <- forkIO $ fix $ \loop -> do
          _ <- readChan chan
          loop
         -- start main processing loop
  mainLoop sock chan 0

mainLoop::Socket -> Chan Msg -> Int-> IO ()
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
runConn:: (Socket,SockAddr)->Chan Msg ->Int ->IO ()
runConn (sock, _) chan nr = do
            let broadcast msg  = writeChan chan (nr,msg) --broadcast the message to the channel
            NB.sendAll sock (BS.pack "Hi, what's your name?")
            name <- NB.recv sock 1024
            putStrLn $ "New Connection: " ++ show nr ++ " Name: " ++ BS.unpack name
            broadcast (BS.concat ["--> ",name," entered."])
            NB.sendAll sock (BS.concat ["Welcome, ", name, "!"])
            --duplicated channel
            -- new channel is empty
            -- Writes to read from duplicated channels, but not removed from other channels
            -- example of multicast
            chan' <- dupChan chan
            -- create reader thread, we'll kill it later
            -- need thread id to kill it
            reader <- forkIO $ fix $ \loop -> do -- read message, but not our own messages
              (nr',line) <- readChan chan'
              when (nr /= nr') $ NB.sendAll sock line
              loop
            -- handle an exception or quit when the user enters "quit"
            handle (\(SomeException _)-> return ()) $ fix $ \loop -> do

                line <- NB.recv sock 1024
                case line of
                   "quit" -> NB.sendAll sock  (BS.pack "Bye!")
                   _      -> do
                            broadcast (BS.concat [name, ": ", line])
                            loop
            putStrLn $ "Killing thread: " ++ show reader
            killThread reader
            broadcast (BS.concat ["<-- ",  name ,  " left."])
            close sock

