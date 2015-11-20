module Main where
{-Simple version that uses System.IO-}
{-Add concurrency to main loop-}
{-Add comminication between threads-}
{-Fixed in this new version:
  This version has a memory leak because the originla thread is not read
  Closing connection isn't handled well
  Need to add exception handling
-}
{-
Add ability to echo back to the user that initiated the message
Associate connection with a name
Add exception handling
Fix memory leak
-}
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix  (fix)
import           Network.Socket
import           System.IO

type Msg = (Int,String)

main :: IO ()
main = do
  --create a socket
  sock  <- socket AF_INET Stream 0

  --create a channel
  chan <- newChan::IO (Chan Msg)
  --make socket immediately resuable
  setSocketOption sock ReuseAddr 1

  -- listen on TCP port 4242
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY)

  -- allow as maximum of 2 outstanding connections
  listen sock 2

  _ <- forkIO $ fix $ \loop -> do
          _ <- readChan chan
          loop
         -- start main processing loop
  mainLoop sock chan 0

mainLoop::Socket -> Chan Msg -> Int-> IO ()
mainLoop sock chan nr = do
    conn <- accept sock
    _ <- forkIO (runConn conn chan nr)
    mainLoop sock chan $! nr+1

-- runConn duplicates the channel and reads from it
-- this runs for each connection to the server
-- When a connection is made the user is asked for a name
-- When an exception occurs the thread is killed
-- When the user enter "quit" the thread is killed
runConn:: (Socket,SockAddr)->Chan Msg ->Int ->IO ()
runConn (sock, _) chan nr = do
            let broadcast msg  = writeChan chan (nr,msg) --broadcast the message to the channel
            hdl <- socketToHandle sock ReadWriteMode
            hSetBuffering hdl NoBuffering
            hPutStrLn hdl "Hi, what's your name?"
            name <- liftM init (hGetLine hdl) -- everything but the last newline character, lift init into IO
            broadcast ("--> " ++ name ++ " entered.")
            hPutStrLn hdl ("Welcome, " ++ name ++ "!")
            --duplicated channel
            -- new channel is empty
            -- Writes to read from duplicated channels, but not removed from other channels
            -- example of multicast
            chan' <- dupChan chan
            -- create reader thread, we'll kill it later
            -- need thread id to kill it
            reader <- forkIO $ fix $ \loop -> do -- read message, but not our own messages
              (nr',line) <- readChan chan'
              when (nr /= nr') $ hPutStrLn hdl line
              loop
            -- handle an exception or quit when the user enters "quit"
            handle (\(SomeException _)-> return ()) $ fix $ \loop -> do

                line <- liftM init (hGetLine hdl)
                case line of
                   "quit" -> hPutStrLn hdl "Bye!"
                   _      -> do
                            broadcast (name ++ ": " ++ line)
                            loop
            killThread reader
            broadcast ("<-- " ++ name ++ " left.")
            hClose hdl

