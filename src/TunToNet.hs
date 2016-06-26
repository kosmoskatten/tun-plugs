{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Data.Word (Word8)
import Data.String.Conv (toS)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (allocaBytes)
import Network.Discovery.Info ( Info (..)
                              , Interface (..)
                              , Payload (..)
                              , Protocol (..)
                              )
import Network.Nats ( Connection
                    , JsonMsg (..)
                    , NatsMsg (..)
                    , Timeout (..)
                    , Topic
                    , defaultSettings
                    , pub'
                    , pubJson'
                    , requestJSON
                    , runNatsClient
                    , subAsync'
                    )
import Network.Socket ( PortNumber
                      , SockAddr (..)
                      , Family (..)
                      , SocketType (..)
                      , Socket
                      , bind
                      , defaultProtocol
                      , socket
                      , inet_addr
                      , recvBufFrom
                      , sendBufTo
                      )
import System.Environment (getArgs)
import System.IO.Tun (openTun)
import System.Posix.IO (fdReadBuf, fdWriteBuf)
import System.Posix.Types (Fd)

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["aside", ownIp, natsUri, device] ->
            tunPlug "aside-tunnel" ownIp natsUri 
                    ( "service.register.aside-tunnel"
                    , "service.discover.bside-tunnel"
                    )
                    device

        ["bside", ownIp, natsUri, device] ->
            tunPlug "bside-tunnel" ownIp natsUri
                    ( "service.register.bside-tunnel"
                    , "service.discover.aside-tunnel"
                    )
                    device
        _                                  ->
            putStrLn "Usage: tun-plugs [aside|bside] <ip> <natsUri> <tun>"

tunPlug :: Topic -> String -> String -> (Topic, Topic) -> String -> IO ()
tunPlug ownName ownIp natsUri (ownSide, otherSide) device =
    runNatsClient defaultSettings (BS.pack natsUri) $ \conn -> do

        -- Publish myselves.
        pubJson' conn ownSide $ mkConfig ownIp

        -- Subscribe to pings.
        void $ subAsync' conn (ownName `BS.append` ".ping") $
            \(NatsMsg _ _ (Just replyTo) p) -> pub' conn replyTo p 

        -- Waiting for peer discovery.
        mPeer <- waitForPeer conn otherSide
        maybe (putStrLn "Error: Cannot decode peer ServiceConfig")
              (setupPlugs ownIp device)
              mPeer

setupPlugs :: String -> String -> Info -> IO ()
setupPlugs ownIp device info = do
    let ifc = head $ interfaces info
    sock <- serverSocket ownIp $ fromIntegral udpSocket
    peer <- sockAddr (toS $ address ifc) (fromIntegral $ port ifc)
    mFd  <- openTun device
    case mFd of
        Just fd -> do
            void $ forkIO $ tunToSock fd (sock, peer)
            sockToTun sock fd

        Nothing -> putStrLn "setupPlugs: cannot open tun device"

tunToSock :: Fd -> (Socket, SockAddr) -> IO ()
tunToSock fd (sock, peer) = allocaBytes 1500 go
    where
      go :: Ptr Word8 -> IO ()
      go ptr = do
          bytesGot  <- fromIntegral <$> fdReadBuf fd ptr 1500
          bytesSent <- sendBufTo sock ptr bytesGot peer
          when (bytesGot /= bytesSent) $ putStrLn "tunToSock: size warning"
          go ptr

sockToTun :: Socket -> Fd -> IO ()
sockToTun sock fd = allocaBytes 1500 go
    where
      go :: Ptr Word8 -> IO ()
      go ptr = do
          (bytesGot, _) <- recvBufFrom sock ptr 1500
          let bytesGot' = fromIntegral bytesGot
          bytesSent <- fdWriteBuf fd ptr bytesGot'
          when (bytesGot' /= bytesSent) $ putStrLn "sockToTun: size warning"
          go ptr

mkConfig :: String -> Info
mkConfig ip =
    Info { service    = "TunToNet. TUN to UDP transport tunnel."
         , version    = "0.0.1.0"
         , interfaces =
            [ Interface { role     = "Tunnel"
                        , protocol = UDP
                        , address  = toS ip
                        , port     = udpSocket
                        , payload  = Payload "IP packets"
                        }
            ]
         }

waitForPeer :: Connection -> Topic -> IO (Maybe Info)
waitForPeer conn topic = do
    putStrLn "Wait for peer discovery ..."
    go
    where
      go = do
          mResp <- requestJSON conn topic ([] :: [Int]) $ Sec 10
          maybe (putStrLn "Still waiting ..." >> go)
                (\(JsonMsg _ _ _ msg) -> return msg)
                mResp

udpSocket :: Int
udpSocket = 8000

serverSocket :: String -> PortNumber -> IO Socket
serverSocket host port' = do
    sock <- socket AF_INET Datagram defaultProtocol
    bind sock =<< sockAddr host port'
    return sock

sockAddr :: String -> PortNumber -> IO SockAddr
sockAddr host port' = SockAddrInet port' <$> inet_addr host
