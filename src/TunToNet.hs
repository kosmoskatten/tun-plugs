{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Conduit
import Control.Concurrent.Async (async, wait)
import Data.ByteString (ByteString)
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize
import Data.String (fromString)
import System.Environment (getArgs)
import System.IO.Tun (openTun)

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
        mTunH <- openTun device
        case mTunH of
            Just tunH -> do
                p1 <- async $ tunToNetPipe (sourceHandle tunH) (appSink app)
                p2 <- async $ netToTunPipe (appSource app) (sinkHandle tunH)
                wait p1
                wait p2
            Nothing   -> putStrLn "Cannot open tun device"

server :: String -> HostPreference -> Int -> IO ()
server device preference port = do
    let settings = serverSettings port preference
    runTCPServer settings $ \app -> do
        mTunH <- openTun device
        case mTunH of
            Just tunH -> do
                p1 <- async $ netToTunPipe (appSource app) (sinkHandle tunH)
                p2 <- async $ tunToNetPipe (sourceHandle tunH) (appSink app)
                wait p1
                wait p2
            Nothing   -> putStrLn "Cannot open tun device"

tunToNetPipe :: Source IO ByteString -> Sink ByteString IO () -> IO ()
tunToNetPipe src sink = src =$= (db "from tun => ") =$= toFrame =$= conduitPut put =$= (db "to net => ") $$ sink

netToTunPipe :: Source IO ByteString -> Sink ByteString IO () -> IO ()
netToTunPipe src sink = src =$= (db "from net => ") =$= conduitGet2 get =$= toByteString =$= (db "to tun => ") $$ sink

sizeSink :: Sink ByteString IO ()
sizeSink = awaitForever $ \pkg -> do
    liftIO $ putStrLn $ show (BS.length pkg) ++ "  bytes received."
    return ()

-- | Conduit to convert from 'ByteString' to 'Frame'.
toFrame :: Conduit ByteString IO Frame
toFrame = awaitForever $ yield . Frame

-- | Conduit to conver from 'Frame' to 'ByteString'.
toByteString :: Conduit Frame IO ByteString
toByteString = awaitForever $ \(Frame bs) -> yield bs

db :: String -> Conduit ByteString IO ByteString
db label = awaitForever $ \bs -> do
    liftIO $ putStrLn $ label ++ (show $ BS.length bs)
    yield bs
