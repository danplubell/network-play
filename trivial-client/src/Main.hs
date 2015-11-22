module Main where

{-
Trivial client that uses a hardcoded ip address and port
This version brackets the acquire, execute, release cycle
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
    handle handleIt $ bracket (socket AF_INET6 Stream protocol)
                              (\sock -> shutdown sock ShutdownBoth >> do putStrLn "Closing socket"; close sock)
                              (\sock -> do
                                 (addr:_)<- getAddrInfo (Just addrInfo) (Just "www.aqualatus.com") (Just "80")
                                 connect sock (addrAddress addr)
                                 mainLoop sock
                              )
runRecv::Socket -> IO ()
runRecv sock = do
        reader <- forkIO $ fix $ \loop -> do
                                      putStrLn "ready to receive"
                                      line <- NB.recv sock 1024
                                      putStrLn $ BS.unpack line
                                      loop
        return ()
mainLoop :: Socket -> IO ()
mainLoop sock = do putStrLn "Start mainLoop"
                   fix $ \loop -> do
                              line <- getLine
                              case line of
                                  "quit" -> return ()
                                  _      -> do _ <- NB.send sock (BS.pack line)
                                               loop

handleIt::SomeException -> IO ()
handleIt e = putStrLn $  "An exception was thrown: " ++ show e
