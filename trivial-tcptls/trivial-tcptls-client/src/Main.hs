{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.ByteString.Char8      as BS
import           Data.Default.Class
import qualified Data.Text.Encoding         as TEN
import qualified Data.Text.IO               as TIO
import           Data.Typeable
import           Data.X509.CertificateStore
import           Network.BSD
import           Network.Socket
import           Network.TLS
import           Network.TLS.Extra
import           Protocol
import           System.Environment
#ifdef darwin_HOST_OS
import           X509
#else
import           System.X509.Unix
#endif
main :: IO ()
main = do
    (ipAddr:port:_) <- getArgs
    store <- getSystemCertificateStore

    protocol <- getProtocolNumber "TCP"
    let addrInfo = defaultHints { addrFlags = [AI_V4MAPPED] -- if not IPv6 then return IPv4 mapped to IPV6
                                , addrFamily = AF_INET6     -- try IPv6
                                , addrSocketType = Stream   -- Our choice in this case is streaming
                                , addrProtocol = protocol   -- use the desired protocol
                                }
    handle handleIt $ bracket (do
                                 sock <-socket AF_INET6 Stream protocol
                                 (addr:_)<- getAddrInfo (Just addrInfo) (Just ipAddr) (Just port)
                                 connect sock (addrAddress addr)
                                 ctx <- contextNew sock (getDefaultParams ipAddr  store)
                                 handshake ctx
                                 return (sock,ctx)
                              )
                               (\(_,ctx)-> do
                                          contextFlush ctx
                                          contextClose ctx)
                              (\(_,ctx) -> do
                                queue <- newTQueueIO
                                receiver <- startReceiver ctx queue
                                msgRecv <- recvMsgFromQueue queue
                                case toEnum ( msgType msgRecv) of
                                  Chat -> print $ chtMsg (decodeMsgBody msgRecv)
                                  GreetReq -> do
                                    print $ grtMsg (decodeMsgBody msgRecv)
                                    name <- TIO.getLine
                                    sendMsg ctx (mkMsg GreetResp (GreetMsg (TEN.encodeUtf8 name)))
                                  GreetResp -> print $ grtMsg (decodeMsgBody msgRecv)
                                  Shutdown -> putStrLn "Received Shutdown"
                                reader <- runReader queue
                                mainLoop ctx

                                killThread reader
                                killThread receiver
                                )
runReader::TQueue Word8  -> IO ThreadId
runReader queue =
    forkIO $ fix $ \loop -> do
       msgRecv <- recvMsgFromQueue queue
       case toEnum ( msgType msgRecv) of
         Chat -> print $ chtMsg (decodeMsgBody msgRecv)
         GreetReq -> print $ grtMsg (decodeMsgBody msgRecv)

         GreetResp -> print $ grtMsg (decodeMsgBody msgRecv)
         Shutdown -> putStrLn "Received Shutdown"
       loop

decodeMsgBody::Msg -> PayLoadMsg
decodeMsgBody msgRecv = decode $ msgBody msgRecv


mainLoop :: Context -> IO ()
mainLoop ctx = fix $ \loop -> do
     line <- TIO.getLine
     unless (line == "quit") (do
                               sendMsg ctx (mkMsg Chat (ChatMsg 0 (TEN.encodeUtf8 line)))
                               loop)

handleIt::SomeException -> IO ()
handleIt (SomeException e)  = putStrLn $  "An exception was thrown: " ++ "Type: " ++ show (typeOf e) ++ " " ++  show e

getDefaultParams::HostName -> CertificateStore -> ClientParams
getDefaultParams host store =
  (defaultParamsClient host BS.empty )
  { clientSupported = def {supportedCiphers=ciphersuite_all}
  , clientShared = def { sharedCAStore = store
                       , sharedValidationCache = def
                       }
  }
