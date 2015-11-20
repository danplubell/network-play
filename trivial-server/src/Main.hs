module Main where
{-Simple version that uses System.IO-}

import           Network.Socket
import           System.IO
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
         mainLoop sock  -- go back and do it again

runConn:: (Socket,SockAddr)->IO ()
runConn (sock, _) = do
            hdl <- socketToHandle sock ReadWriteMode
            hSetBuffering hdl NoBuffering
            hPutStrLn hdl "Hi!"
            hClose hdl
