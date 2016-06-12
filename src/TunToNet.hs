{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Conduit
import Control.Concurrent.Async (async, wait)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize
import Data.String (fromString)
import System.Environment (getArgs)
import System.IO.Tun (openTun)
import System.Posix.Types (Fd)
import System.Posix.IO.ByteString (fdRead, fdWrite)

import qualified Data.ByteString.Char8 as BS

import Data.Message.Frame

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["client", device, host, port]       -> 
            client device (BS.pack $ host) (read port)
        ["server", device, preference, port] ->
            server device (fromString preference) (read port)
        _ -> putStrLn "Unknown config"

client :: String -> ByteString -> Int -> IO ()
client device host port = do
    let settings = clientSettings port host
    runTCPClient settings $ \app -> do
        mFd <- openTun device
        case mFd of
            Just fd -> do
                p1 <- async $ tunSrcPipe fd $ appSink app
                p2 <- async $ netSrcPipe (appSource app) fd
                wait p1
                wait p2
            Nothing -> putStrLn "Cannot open tun device"

server :: String -> HostPreference -> Int -> IO ()
server device preference port = do
    let settings = serverSettings port preference
    runTCPServer settings $ \app -> do
        mFd <- openTun device
        case mFd of
            Just fd -> do
                p1 <- async $ netSrcPipe (appSource app) fd
                p2 <- async $ tunSrcPipe fd (appSink app)
                wait p1
                wait p2
            Nothing -> putStrLn "Cannot open tun device"

tunSrcPipe :: Fd -> Sink ByteString IO ()-> IO ()
tunSrcPipe fd sink = tunSource fd 
                 =$= toFrame
                 =$= conduitPut put
                 $$ sink

netSrcPipe :: Source IO ByteString -> Fd -> IO ()
netSrcPipe src fd = src
                =$= conduitGet2 get
                =$= fromFrame
                $$ tunSink fd

tunSource :: Fd -> Source IO ByteString
tunSource fd = forever $ do
    pkt <- liftIO $ fdRead fd 2000
    yield pkt

tunSink :: Fd -> Sink ByteString IO ()
tunSink fd = awaitForever $ \pkt -> do
    let l = BS.length pkt
    ll <- liftIO $ fromIntegral <$> fdWrite fd pkt
    if l == ll then return ()
               else (liftIO $ putStrLn $ "Err: Written: " ++ show ll)

{-inspectBs :: Conduit ByteString IO ByteString
inspectBs = awaitForever $ \pkt -> do
    liftIO $ putStrLn $ "Size: " ++ show (BS.length pkt)
    liftIO $ BS.putStrLn pkt
    yield pkt
    -}

toFrame :: Conduit ByteString IO Frame
toFrame = awaitForever $ yield . Frame

fromFrame :: Conduit Frame IO ByteString
fromFrame = awaitForever $ \(Frame pkt) -> yield pkt
