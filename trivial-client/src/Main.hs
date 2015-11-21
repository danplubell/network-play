module Main where

{-
Trivial client that uses a hardcoded ip address and port
This version does synchronous send and receive
-}

import           Control.Exception
import           Network.BSD
import           Network.Socket

main :: IO ()
main = do
  --create socket
  --socket::Family -> Socket_Type -> ProtocolNumber -> IO Socket
  -- if AF_INET6 then IPV6Only socket option is set to 0
  protocol <- getProtocolNumber "TCP"
  sock <- socket AF_INET6 Stream protocol
  putStrLn $ "Opened socket: " ++ show sock

  --set up addrInfo
  --
  let addrInfo = defaultHints { addrFlags = [AI_V4MAPPED] -- if not IPv6 then return IPv4 mapped to IPV6
                              , addrFamily = AF_INET6     -- try IPv6
                              , addrSocketType = Stream   -- Out choice in this case is streaming
                              , addrProtocol = protocol   -- use the desired protocol
                              }



  --get address info.  Used to resolve hostname to HostAddress
  --getAddrInfo Maybe AddrInfo Maybe HostName Maybe ServiceName -> IO [AddrInfo]
  --this could fail if the host doesn't exist or something else happens. We'll add exception handing later
  (addr:_)<- getAddrInfo (Just addrInfo) (Just "www.aqualatus.com") (Just "80")
  putStrLn $ "Got Address: " ++ show addr
  --create connection

  --connect::Socket -> SockAddr -> IO()
  conn <- connect sock (addrAddress addr)
  putStrLn $ "Create Connection: " ++ show conn

  close sock
  putStrLn "Closed Socket"
