module Data.Message.Frame
    ( Frame (..)
    ) where

import Data.ByteString (ByteString)
import Data.Serialize
import Data.Word (Word32)

import qualified Data.ByteString as BS

-- | A frame for transporting packages captured from Tun over TCP. Its
-- payload data preceeded with the size of the payload. The size is
-- a signed, four byte, big endian integer.
data Frame = Frame !ByteString
    deriving (Eq, Show)

instance Serialize Frame where
    put (Frame bs) = do
        let l = BS.length bs
        putWord32be $ toWord32 l
        putByteString bs

    get = do
        l <- toInt <$> getWord32be
        Frame <$> getByteString l

toWord32 :: Int -> Word32
toWord32 = fromIntegral

toInt :: Word32 -> Int
toInt = fromIntegral
