{-# LANGUAGE CPP #-}
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
import           Data.Typeable
import           Data.X509.CertificateStore
import           Network.BSD
import           Network.Socket
import           Network.TLS
import           Network.TLS.Extra
import           Protocol
import           System.Environment
import           X509
-- #ifdef darwin_HOST_OS
--import           System.X509.MacOS
-- #else
-- #  ifdef linux_HOST_OS
--import           System.X509.Linux
-- #  endif
-- #endif
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
                                    name <- getLine
                                    sendMsg ctx (mkMsg GreetResp (GreetMsg (BS.pack name)))
                                  GreetResp -> print $ grtMsg (decodeMsgBody msgRecv)
                                  Shutdown -> print "Received Shutdown"
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
         Shutdown -> print "Received Shutdown"
       loop

decodeMsgBody::Msg -> PayLoadMsg
decodeMsgBody msgRecv = decode $ msgBody msgRecv


mainLoop :: Context -> IO ()
mainLoop ctx = fix $ \loop -> do
     line <- getLine
     unless (line == "quit") (do
                               sendMsg ctx (mkMsg Chat (ChatMsg 0 (BS.pack line)))
                               loop)

handleIt::SomeException -> IO ()
handleIt (SomeException e)  = putStrLn $  "An exception was thrown: " ++ "Type: " ++ show (typeOf e) ++ " " ++  show e

getDefaultParams::HostName -> CertificateStore -> ClientParams
getDefaultParams host store =
  (defaultParamsClient host BS.empty )
  { clientSupported = def {supportedCiphers=ciphersuite_all}
  , clientShared = def { sharedCAStore = store
                       , sharedValidationCache = def --ValidationCache (\_ _ _ -> return ValidationCachePass)
                                                 --                  (\_ _ _ -> return ())
                       }
  }
