module Main where
{-
This version is a very, very minimal implementtion
It handles one connection at a time
-}

import           Network.Socket

main :: IO ()
main = do
  --create a socket
  sock  <- socket AF_INET Stream 0

  --make socket immediately resuable
  setSocketOption sock ReuseAddr 1

  -- listen on TCP port 4242
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY)

  -- allow as maximum of 2 outstanding connections
  listen sock 2

  -- start main processing loop
  mainLoop sock


--this version of mainLoop handles one connection at a time
mainLoop :: Socket -> IO ()
mainLoop sock = do
         -- accept one connection and handle it
         conn <- accept sock
         runConn conn
         mainLoop sock  -- go back and do it again-}


-- this version is a simple send using Network.Socket
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    _ <- send sock "Hi!\n" -- send something simple
    sClose sock -- close the socket

