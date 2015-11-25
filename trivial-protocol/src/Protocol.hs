{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Protocol where

import           Data.Binary
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import           GHC.Generics              (Generic)
import           Network.Socket
import           Network.Socket.ByteString as NBS
data Msg = Msg { msgType ::Int
               , msgLen  ::Int
               , msgBody ::BS.ByteString
               } deriving (Show,Eq,Generic)
instance Binary Msg

data MsgType = Chat | GreetReq | GreetResp | InvalidType deriving (Show,Eq,Enum, Generic)
instance Binary MsgType


data PayLoadMsg = ChatMsg{ chtMsgID :: Int
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

recvMsg::Socket -> IO Msg
recvMsg sock = undefined

