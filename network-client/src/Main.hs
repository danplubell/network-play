module Main where

--import           Data.ByteString
import           Network.Connection

main :: IO ()
main = do
  ctx  <- initConnectionContext
  conn <- connectTo ctx  ConnectionParams { connectionHostname = "192.168.1.95"
                                         , connectionPort = 4445
                                         , connectionUseSecure = Nothing
                                         , connectionUseSocks = Nothing
                                           }
  r <- connectionGet conn 1096
  print r
  connectionClose conn

