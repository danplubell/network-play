module Main where

{-
Trivial client that uses a hardcoded ip address and port
This version brackets the acquire, execute, release cycle
Switch to using bytestrings instead of System.IO
-}

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Fix
import qualified Data.ByteString.Char8     as BS
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

                              (\sock -> shutdown sock ShutdownBoth >> close sock)
                              (\sock -> do
                                reader <- runRecv sock
                                mainLoop sock
                                killThread reader
                              )
runRecv::Socket -> IO ThreadId
runRecv sock =
    forkIO $ fix $ \loop -> do
        line <- NB.recv sock 1024
        putStrLn $ BS.unpack line
        loop

mainLoop :: Socket -> IO ()
mainLoop sock = fix $ \loop -> do
     line <- getLine
     case line of
         "quit" -> return ()
         _      -> do
                     NB.sendAll sock ( BS.pack line)
                     loop

handleIt::SomeException -> IO ()
handleIt e = putStrLn $  "An exception was thrown: " ++ show e
