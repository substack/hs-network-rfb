module System.IO.Word where

import System.IO
import Data.Word
import Control.Applicative ((<$>))
import Control.Arrow ((&&&), (***))
import Control.Monad (replicateM)

hGetWord8 :: Handle -> IO Word8
hGetWord8 = hGetN (8 `div` 8)

hGetWord16 :: Handle -> IO Word16
hGetWord16 = hGetN (16 `div` 8)

hGetWord32 :: Handle -> IO Word32
hGetWord32 = hGetN (32 `div` 8)

hGetWord64 :: Handle -> IO Word64
hGetWord64 = hGetN (64 `div` 8)

hGetN :: Integral a => Int -> Handle -> IO a
hGetN n h = foldl1 (+)
    <$> zipWith (*) (iterate (`div` 256) (fromIntegral $ 256 ^ (n - 1)))
    <$> replicateM n (fromIntegral . fromEnum <$> hGetChar h)

hTake :: Handle -> Int -> IO String
hTake h n = replicateM n (hGetChar h)

hTakeBytes :: Handle -> Int -> IO [Int]
hTakeBytes h n = replicateM n (fromIntegral <$> hGetWord8 h)

hGetByte :: Handle -> IO Int
hGetByte = (fromIntegral <$>) . hGetWord8

hGetShort :: Handle -> IO Int
hGetShort = (fromIntegral <$>) . hGetWord16

hGetInt :: Handle -> IO Int
hGetInt = (fromIntegral <$>) . hGetWord32

hGetLong :: Handle -> IO Int
hGetLong = (fromIntegral <$>) . hGetWord64
