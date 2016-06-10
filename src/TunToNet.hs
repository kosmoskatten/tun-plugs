module Main
    ( main
    ) where

import Data.ByteString (ByteString)
import Conduit
import Data.Conduit.Network
import System.IO.Tun (openTun)

import qualified Data.ByteString as BS

main :: IO ()
main = do
    mTunH <- openTun "tun0"
    case mTunH of
        Just tunH -> sourceHandle tunH $$ sizeTeller
        Nothing   -> putStrLn "Cannot open tun device."

sizeTeller :: Sink ByteString IO ()
sizeTeller = awaitForever $ \pkg -> do
    liftIO $ putStrLn $ show (BS.length pkg) ++ " bytes received."
    return ()
