module Main where
{-Simple version that uses System.IO-}
{-Add concurrency to main loop-}
{-Add comminication between threads-}
{-This version has a memory leak because the originla thread is not read
  Closing connection isn't handled well
  Need to add exception handling
-}
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix  (fix)
import           Network.Socket
import           System.IO

type Msg = String

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

  --create a channel
  chan <- newChan
  -- start main processing loop
  mainLoop sock chan

mainLoop::Socket -> Chan Msg -> IO ()
mainLoop sock chan = do
    conn <- accept sock
    _ <- forkIO (runConn conn chan)
    mainLoop sock chan

-- runConn duplicates the channel and reads from it
runConn:: (Socket,SockAddr)->Chan Msg ->IO ()
runConn (sock, _) chan = do
            let broadcast  = writeChan chan
            hdl <- socketToHandle sock ReadWriteMode
            hSetBuffering hdl NoBuffering
            chan' <- dupChan chan
            --fork off thread for reading from duplicated channel
            _ <- forkIO $ fix $ \loop -> do
                line <- readChan chan'
                hPutStrLn hdl line
                loop
            fix $ \loop -> do
                line <- liftM init (hGetLine hdl)
                broadcast line
                loop

