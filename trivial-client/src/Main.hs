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
import           System.IO
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
                                 (addr:_)<- getAddrInfo (Just addrInfo) (Just "127.0.0.1") (Just "4242")
                                 connect sock (addrAddress addr)
                                 hdl <- socketToHandle sock ReadWriteMode
                                 hSetBuffering hdl NoBuffering
                                 putStrLn "runRecv"
                                 runRecv hdl
                                 putStrLn "start main loop"
                                 mainLoop hdl
                              )
runRecv::Handle -> IO ()
runRecv hdl = do
        reader <- forkIO $ fix $ \loop -> do
                                      putStrLn "ready to receive"
                                      line <- hGetLine hdl
                                      putStrLn line
                                      loop
        return ()
mainLoop :: Handle -> IO ()
mainLoop hdl = do putStrLn "Start mainLoop"
                  fix $ \loop -> do
                             line <- getLine
                             case line of
                                 "quit" -> return ()
                                 _      -> do
                                             hPutStrLn hdl line
                                             putStrLn $ "Sent: " ++ line
                                             loop

handleIt::SomeException -> IO ()
handleIt e = putStrLn $  "An exception was thrown: " ++ show e
