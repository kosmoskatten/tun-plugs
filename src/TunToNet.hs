{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (allocaBytes)
import Network.Nats
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

import Types (ServiceConfig (..), Interface (..))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["client", ownIp, natsUri, device] ->
            tunPlug ownIp natsUri 
                    ("service.register.client", "service.request.server")
                    device

        ["server", ownIp, natsUri, device] ->
            tunPlug ownIp natsUri
                    ("service.register.server", "service.request.client")
                    device
        _                                  ->
            putStrLn "Usage: tun-plugs [client|server] <ip> <natsUri> <tun>"

tunPlug :: String -> String -> (Topic, Topic) -> String -> IO ()
tunPlug ownIp natsUri (ownSide, otherSide) device = do
    let settings = defaultSettings { loggerSpec = StdoutLogger }
    runNatsClient settings (BS.pack natsUri) $ \conn -> do

        -- Publish myselves.
        pubJson' conn ownSide $ mkConfig ownIp

        -- Waiting for peer discovery.
        mPeer <- waitForPeer conn otherSide
        maybe (putStrLn "Error: Cannot decode peer ServiceConfig")
              (setupPlugs ownIp device)
              mPeer

setupPlugs :: String -> String -> ServiceConfig -> IO ()
setupPlugs ownIp device conf = do
    let ifc = head $ interfaces conf
    sock <- serverSocket ownIp $ fromIntegral udpSocket
    peer <- sockAddr (address ifc) (fromIntegral $ port ifc)
    mFd  <- openTun device
    case mFd of
        Just fd -> do
            forkIO $ tunToSock fd (sock, peer)
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

mkConfig :: String -> ServiceConfig
mkConfig host = 
    ServiceConfig
        { interfaces =
            [ Interface
                { role     = "Tunnel"
                , address  = host
                , port     = udpSocket
                , protocol = "UDP"
                }
            ]
        }

waitForPeer :: Connection -> Topic -> IO (Maybe ServiceConfig)
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
