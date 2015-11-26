module Main where

{-
This client will read two bytes at a time.
It's for playing around with how messages are split
-}

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Data.Binary
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Typeable
import           Data.Word
import           Network.BSD
import           Network.Socket
--import qualified Network.Socket.ByteString as NB
import           Protocol
main :: IO ()
main = do
    protocol <- getProtocolNumber "TCP"
    let addrInfo = defaultHints { addrFlags = [AI_V4MAPPED] -- if not IPv6 then return IPv4 mapped to IPV6
                                , addrFamily = AF_INET6     -- try IPv6
                                , addrSocketType = Stream   -- Our choice in this case is streaming
                                , addrProtocol = protocol   -- use the desired protocol
                                }
    handle handleIt $ bracket (do
                                 sock <-socket AF_INET6 Stream protocol
                                 (addr:_)<- getAddrInfo (Just addrInfo) (Just "127.0.0.1") (Just "4242")
                                 connect sock (addrAddress addr)
                                 return sock
                              )
                               close
                              (\sock -> do
                                queue <- newTQueueIO
                                receiver <- startReceiver sock queue
                                msgRecv <- recvMsgFromQueue queue
                                case toEnum ( msgType msgRecv) of
                                  Chat -> print $ chtMsg ((decode $ BL.fromStrict (msgBody msgRecv))::PayLoadMsg)
                                  GreetReq -> do
                                    print $ grtMsg ((decode $ BL.fromStrict (msgBody msgRecv))::PayLoadMsg)
                                    name <- getLine
                                    sendMsg sock (mkMsg GreetResp (GreetMsg (BS.pack name)))
                                  GreetResp -> print $ grtMsg ((decode $ BL.fromStrict (msgBody msgRecv))::PayLoadMsg)
                                  Shutdown -> print "Received Shutdown"
                                reader <- runReader queue
                                mainLoop sock
                                shutdown sock ShutdownBoth
                                killThread reader
                                killThread receiver
                                )
runReader::TQueue Word8  -> IO ThreadId
runReader queue =
    forkIO $ fix $ \loop -> do
       msgRecv <- recvMsgFromQueue queue
       case toEnum ( msgType msgRecv) of
         Chat -> print $ chtMsg ((decode $ BL.fromStrict (msgBody msgRecv))::PayLoadMsg)
         GreetReq -> print $ grtMsg ((decode $ BL.fromStrict (msgBody msgRecv))::PayLoadMsg)

         GreetResp -> print $ grtMsg ((decode $ BL.fromStrict (msgBody msgRecv))::PayLoadMsg)
         Shutdown -> print "Received Shutdown"
       loop





mainLoop :: Socket -> IO ()
mainLoop sock = fix $ \loop -> do
     line <- getLine
     unless (line == "quit") (do
                               sendMsg sock (mkMsg Chat (ChatMsg 0 (BS.pack line)))
                               loop)

handleIt::SomeException -> IO ()
handleIt (SomeException e)  = putStrLn $  "An exception was thrown: " ++ "Type: " ++ show (typeOf e) ++ " " ++  show e
