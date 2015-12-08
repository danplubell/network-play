module Main where

{-
Trivial client that uses a hardcoded ip address and port
This version brackets the acquire, execute, release cycle
-}

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Fix
import           Network.BSD
import           Network.Socket
import           System.IO
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
                                 hdl <-socketToHandle sock ReadWriteMode
                                 hSetBuffering hdl NoBuffering
                                 return hdl
                              )

                              hClose
                              (\hdl -> do
                                reader <- runRecv hdl
                                mainLoop hdl
                                killThread reader
                              )
runRecv::Handle -> IO ThreadId
runRecv hdl =
    forkIO $ fix $ \loop -> do
        line <- hGetLine hdl
        putStrLn line
        loop

mainLoop :: Handle -> IO ()
mainLoop hdl = fix $ \loop -> do
     line <- getLine
     case line of
         "quit" -> return ()
         _      -> do
                     hPutStrLn hdl line
                     loop

handleIt::SomeException -> IO ()
handleIt e = putStrLn $  "An exception was thrown: " ++ show e
