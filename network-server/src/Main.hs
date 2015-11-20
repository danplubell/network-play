{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Network.Connection
main :: IO ()
main = do
  ctx<-initConnectionContext
  conn <- connectTo ctx ConnectionParams {
                                          connectionHostname = "127.0.0.1"
                                        , connectionPort = 4445
                                        , connectionUseSecure = Nothing
                                        , connectionUseSocks = Nothing }
  connectionPut conn "HELLO\n"
  connectionClose conn
