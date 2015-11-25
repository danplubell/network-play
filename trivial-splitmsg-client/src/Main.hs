module Main where

{-
This client will read two bytes at a time.
It's for playing around with how messages are split
-}

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import qualified Data.ByteString.Char8     as BS
import           Data.Typeable
import           Network.BSD
import           Network.Socket
import qualified Network.Socket.ByteString as NB
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
                                reader <- runRecv sock
                                mainLoop sock
                                shutdown sock ShutdownBoth
                                killThread reader
                              )
runRecv::Socket -> IO ThreadId
runRecv sock =
    forkIO $ fix $ \loop -> do
        line <- NB.recv sock 2
        unless  (BS.null line) (do putStrLn $ "msg: " ++ BS.unpack line; loop)


mainLoop :: Socket -> IO ()
mainLoop sock = fix $ \loop -> do
     line <- getLine
     unless (line == "quit") (do
                                s <- NB.send sock (BS.pack line)
                                putStrLn $ "Sent: " ++ show s
                                loop)

handleIt::SomeException -> IO ()
handleIt (SomeException e)  = putStrLn $  "An exception was thrown: " ++ "Type: " ++ show (typeOf e) ++ " " ++  show e
