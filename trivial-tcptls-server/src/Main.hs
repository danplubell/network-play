{-# LANGUAGE OverloadedStrings #-}
module Main where
--import qualified Data.ByteString.Char8 as BS8
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Data.Binary
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BC
import           Data.Default.Class
import           Data.X509.CertificateStore
import           Network.BSD
import           Network.Socket
import           Network.TLS
import           Network.TLS.Extra
import           Protocol
import           System.Environment
import           System.X509.MacOS
main :: IO ()
main = do
  -- set up the socket
  (addr:port:crt:key:_ ) <- getArgs
  let portNumber = read port::Int
  sock <- socket AF_INET Stream 0
  host <- getHostByName addr


  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet (fromIntegral portNumber) (head $ hostAddresses host))
  listen sock 2

  -- setup the broadcast channel
  chan <- newChan::IO (Chan PayLoadMsg)

  void $ forkIO $ fix $ \loop -> do
             void $ readChan chan
             loop
  --setup the default server TLS parameters
  store <- getSystemCertificateStore


  --getCredentials from a PEM file
  credeither <- credentialLoadX509 crt key
--  credeither <- credentialLoadX509 "/Users/cerdep/localhostserver.crt" "/Users/cerdep/localhostserver.key"


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

    queue <- newTQueueIO
    receiver <- startReceiver ctx queue
    let broadcast msg = writeChan chan (ChatMsg nr msg)
    sendMsg ctx (mkMsg GreetReq (GreetMsg "Hi what's your name?"))
    nameMsg <- recvMsgFromQueue queue
    case toEnum (msgType nameMsg) of
      GreetResp -> do
        let name = grtMsg (decode (msgBody nameMsg)::PayLoadMsg)

        putStrLn $ "New connection: " ++ show nr ++ " Name: " ++ BC.unpack name
        broadcast (BC.concat ["--> ",name," entered."])
        sendMsg ctx (mkMsg GreetReq (GreetMsg (BS.concat ["Welcome, ", name, "!"])))

        chan' <- dupChan chan
        reader' <- forkIO $ fix $ \loop -> do
                  chatMsg <- readChan chan'
                  when (nr /= chtMsgId chatMsg) $ sendMsg ctx (mkMsg Chat chatMsg)
                  loop
        handle (\(SomeException _ )-> return ()) $ fix $ \loop -> do
               chatMsg' <- recvMsgFromQueue queue
               case toEnum (msgType chatMsg') of
                 Shutdown -> putStrLn "received shutdown msg..."
                 Chat     -> do
                     let lineMsg = decode ( msgBody chatMsg')::PayLoadMsg
                     case BC.unpack (chtMsg lineMsg) of
                          "quit" -> sendMsg ctx (mkMsg GreetReq (GreetMsg $ BC.pack "Bye!"))
                          []     -> return ()
                          _      -> do
                                   broadcast (BC.concat [name, ": ", chtMsg lineMsg])
                                   loop
                 msgT@_  -> putStrLn $ "Unexpected Chat Messge Type (connection will die): "
                           ++ show msgT
        putStrLn $ "Killing reader thread: " ++ show reader'
        killThread reader'
        broadcast (BC.concat ["<-- ", name , " left."])
      msgT@_  -> putStrLn $ "Unexpected Greeting Message Type (connection will die): " ++ show msgT

    putStrLn $  "Killing receiver thread: " ++ show receiver
    killThread receiver
    contextFlush ctx
    contextClose ctx


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
