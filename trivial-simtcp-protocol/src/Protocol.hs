{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Protocol where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Fix
import           Data.Binary
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           GHC.Generics           (Generic)
import           Network.Simple.TCP

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
  in Msg (fromEnum t) (BS.length bs) bs

sendMsg :: Socket -> Msg -> IO ()
sendMsg sock msg =
  send sock (BS.concat [ (BL.toStrict.encode) (msgType msg)
  , (BL.toStrict.encode) (msgLen msg)
  , msgBody msg])

addToQueue::TQueue Word8 -> Word8 -> IO ()
addToQueue queue w = atomically $ writeTQueue queue w

addBufferToQueue::TQueue Word8 -> BS.ByteString -> IO ()
addBufferToQueue q b = mapM_ (addToQueue q) (BS.unpack b)

recvMsgFromQueue::TQueue Word8 -> IO Msg
recvMsgFromQueue queue = do
    typBytes <- readSome 8
    lenBytes <- readSome 8
    let len = (decode $ BL.pack lenBytes)::Int
    bodyBytes <- readSome len

    return $ Msg (decode $ BL.pack typBytes) len  (BS.pack bodyBytes)

    where readSome :: Int -> IO [Word8]
          readSome n = atomically $ replicateM n (readTQueue queue)


startReceiver::Socket -> TQueue Word8 -> IO ThreadId
startReceiver sock queue = forkIO $ fix $ \recvLoop -> do
              recvd <- recv sock 2
              case recvd of
                Nothing      -> addBufferToQueue queue (BL.toStrict $ encode (mkMsg Shutdown (GreetMsg BS.empty)))
                Just recvd'  -> do
                                 addBufferToQueue queue recvd'
                                 recvLoop

