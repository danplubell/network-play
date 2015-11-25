{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Protocol where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Fix
import           Data.Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import           GHC.Generics              (Generic)
import           Network.Socket
import           Network.Socket.ByteString as NBS

-- | This is the total message
data Msg = Msg { msgType ::Int
               , msgLen  ::Int
               , msgBody ::BS.ByteString
               } deriving (Show,Eq,Generic)
instance Binary Msg

-- | These are the message types used in the envelope
data MsgType = Chat | GreetReq | GreetResp | Shutdown deriving (Show,Eq,Enum, Generic)
instance Binary MsgType

-- |This is the message that is exchanged
data PayLoadMsg = ChatMsg{ chtMsgId :: Int
                         , chtMsg   :: BS.ByteString
                         }
                | GreetMsg { grtMsg :: BS.ByteString}
                deriving (Show,Eq,Generic)
instance Binary PayLoadMsg

mkMsg:: MsgType -> PayLoadMsg -> Msg
mkMsg t p =
  let bs =  (BL.toStrict . encode ) p
  in Msg (fromEnum t) ((fromIntegral . BS.length) bs) bs

sendMsg :: Socket -> Msg -> IO ()
sendMsg sock msg = NBS.sendAll sock (BS.concat [ (BL.toStrict.encode) (msgType msg)
                                               , (BL.toStrict.encode) (msgLen msg)
                                               , msgBody msg])
addToQueue::TQueue Word8 -> Word8 -> IO ()
addToQueue queue w = atomically $ writeTQueue queue w

addBufferToQueue::TQueue Word8 -> BS.ByteString -> IO ()
addBufferToQueue q b = mapM_ (addToQueue q) (BS.unpack b)

recvMsgFromQueue::TQueue Word8 -> IO Msg
recvMsgFromQueue queue = do
    typ <- readSome 8
    len <- readSome 8
    body <- readSome len
    return $ Msg typ len body

    where readSome n = do
                          bytes <- atomically $ replicateM n (readTQueue queue)
                          return $ (decode.BL.pack) bytes

startReceiver::Socket -> TQueue Word8 -> IO ThreadId
startReceiver sock queue = forkIO $ fix $ \recvLoop -> do
              recvd <- NBS.recv sock 2
              if BS.null recvd

                 then addBufferToQueue queue (BL.toStrict $ encode (mkMsg Shutdown (GreetMsg BS.empty)))
                 else do addBufferToQueue queue recvd
                         recvLoop
