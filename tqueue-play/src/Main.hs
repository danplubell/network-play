{-# LANGUAGE DeriveGeneric #-}
module Main where


import           Control.Concurrent
import           Control.Concurrent.STM
--import           Control.Concurrent.STM.TQueue
import           Control.Monad
import           Control.Monad.Fix
import           Data.Binary
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
--import           Data.Typeable
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BC
import           Debug.Trace
import           GHC.Generics           (Generic)
data Msg = Msg Word8 Word8 BS.ByteString
           deriving (Generic)

instance Binary Msg

theMessage :: [Word8]
theMessage = BS.unpack $ BC.pack "this is the message"

binMsg:: [Word8]
binMsg = [5,fromIntegral (length theMessage)] ++ theMessage

main :: IO ()
main = do
  print $  binMsg

  queue <- newTQueueIO
  _ <- forkIO $ fix $  \loop -> do
             _ <- replicateM 1 (mapM_ (addToQueue queue) binMsg)
             loop
  mainLoop queue
--    _ <- forkIO $ fix $ \loop -> do
--                               c <- readFromQueue queue
--                               putStrLn [c]
--                               loop
  return ()

mainLoop ::TQueue Word8 -> IO ()
mainLoop q = do
         c <- readMsgFromQueue q
         putStrLn $ BC.unpack $  BS.pack c
         mainLoop q


addToQueue:: TQueue Word8 -> Word8 -> IO ()
addToQueue q c  = atomically $  writeTQueue q c

readFromQueue::TQueue Word8 -> IO Word8
readFromQueue q = atomically $ readTQueue q

readMsgFromQueue::TQueue Word8 -> IO [Word8]
readMsgFromQueue q = atomically $ do
   --read 5 bytes
   tupe <- readTQueue q
--   traceShowM $ "type: " ++ show tupe
   len <- readTQueue q
--   traceShowM  $  "len: " ++ show len

   replicateM (fromIntegral len)  (readTQueue q)
--   traceShowM $ "body: " ++ show body

