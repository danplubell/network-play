module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Fix
import           Data.Binary
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as BL
import           Network.Simple.TCP
import           Protocol
import           System.Environment

main:: IO ()
main = do
     (ipAddr:port:_) <- getArgs
     connect ipAddr port $ \(sock, _) -> do
       queue <- newTQueueIO
       receiver <- startReceiver sock queue
       msgRecv <- recvMsgFromQueue queue
       case toEnum (msgType msgRecv) of
         Chat     -> print $ chtMsg ((decode $ BL.fromStrict (msgBody msgRecv))::PayLoadMsg)
         GreetReq -> do
           print $ grtMsg ((decode $ BL.fromStrict (msgBody msgRecv))::PayLoadMsg)
           name <- getLine
           sendMsg sock (mkMsg GreetResp (GreetMsg (BS.pack name)))
         GreetResp -> print $ grtMsg ((decode $ BL.fromStrict (msgBody msgRecv))::PayLoadMsg)
         Shutdown  -> print "Received Shutdown"
       reader <- runReader queue
       mainLoop sock
       killThread reader
       killThread receiver

runReader::TQueue Word8 -> IO ThreadId
runReader queue =
  forkIO $ fix $ \loop -> do
     msgRecv <- recvMsgFromQueue queue
     case toEnum (msgType msgRecv) of
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
