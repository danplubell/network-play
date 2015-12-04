{-# LANGUAGE OverloadedStrings #-}
module Main where
--import qualified Data.ByteString.Char8 as BS8
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Fix
import           Data.Default.Class
import           Data.X509.CertificateStore
import           Network.Socket
import           Network.TLS
import           Network.TLS.Extra
import           System.Environment
import           System.X509.MacOS


main :: IO ()
main = do
  -- set up the socket
  (port: _ ) <- getArgs
  let portNumber = read port::Int
  sock <- socket AF_INET Stream 0


  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet (fromIntegral portNumber)iNADDR_ANY)
  listen sock 2

  -- setup the broadcast channel
  chan <- newChan::IO (Chan PayLoadMsg)

  void $ forkIO $ fix $ \loop -> do
             void $ readChan chan
             loop
  --setup the default server TLS parameters
  store <- getSystemCertificateStore


  --getCredentials from a PEM file
  credeither <- credentialLoadX509 "/Users/cerdep/test.pem" "/Users/cerdep/test.pem"

  let cred (Right c) = c
      cred (Left s) = error $ "credential error: " ++ s
      serverParams = getDefaultParams store (cred credeither)
  mainLoop sock chan serverParams 0


mainLoop::Socket -> Chan PayLoadMsg -> ServerParams -> Int -> IO ()
mainLoop sock chan sparams nr = do
  conn <- accept sock
  putStrLn $ "New connection: " ++ show conn
  void $ forkIO (runTLSConn conn chan sparams nr)
  mainLoop sock chan sparams $! nr + 1 -- loopback and do it again

runTLSConn:: (Socket,SockAddr)-> Chan PayLoadMsg -> ServerParams -> Int -> IO ()
runTLSConn (sock,_) chan sparams nr = do
    ctx <- contextNew sock sparams
    handshake ctx
    --do the whole read broadcast thing here
    bye ctx
    close sock
getDefaultParams::CertificateStore->Credential->ServerParams
getDefaultParams store cred =
    ServerParams { serverWantClientCert = False
                 , serverCACertificates = []
                 , serverDHEParams = Nothing
                 , serverShared = def { sharedSessionManager = noSessionManager
                                      , sharedCAStore = store
                                      , sharedValidationCache = def
                                      , sharedCredentials = Credentials [cred]
                                      }
                , serverHooks = def
                , serverSupported = def {supportedCiphers = ciphersuite_all}
                }
