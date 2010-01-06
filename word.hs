module System.IO.Word (
    WordIO, hGetWords, hGetWord, hPutWords, hPutWord,
    hGetByte, hGetShort, hGetInt, hGetLong,
    hGetBytes, hGetShorts, hGetInts, hGetLongs,
    hPutByte, hPutShort, hPutInt, hPutLong,
    hPutBytes, hPutShorts, hPutInts, hPutLongs
) where

import System.IO (Handle, hGetBuf, hPutBuf)
import Foreign (Storable, sizeOf, mallocArray, newArray, peekArray)
import Control.Applicative ((<$>))
import Data.Word (Word8, Word16, Word32, Word64)

class (Num a, Storable a) => WordIO a where
    hGetWords :: Handle -> Int -> IO [a]
    hGetWords = hGetWords' 0
    
    hGetWord :: Handle -> IO a
    hGetWord h = head <$> hGetWords h 1
    
    hPutWords :: Handle -> [a] -> IO ()
    hPutWords = hPutWords' 0
    
    hPutWord :: Handle -> a -> IO ()
    hPutWord h x = hPutWords h [x]

instance WordIO Word8
instance WordIO Word16
instance WordIO Word32
instance WordIO Word64

hGetWords' :: Storable a => a -> Handle -> Int -> IO [a]
hGetWords' x h n = do
    ptr <- mallocArray n
    hGetBuf h ptr (n * sizeOf x)
    peekArray n ptr

hPutWords' :: Storable a => a -> Handle -> [a] -> IO ()
hPutWords' x h xs = do
    ptr <- newArray xs
    hPutBuf h ptr $ (sizeOf x) * length xs

hGetByte :: Integral a => Handle -> IO a
hGetByte = (fromIntegral <$>) . (hGetWord :: Handle -> IO Word8)

hGetBytes :: Integral a => Handle -> Int -> IO [a]
hGetBytes h n = map fromIntegral <$> (hGetWords h n :: IO [Word8])

hGetShort :: Integral a => Handle -> IO a
hGetShort = (fromIntegral <$>) . (hGetWord :: Handle -> IO Word16)

hGetShorts :: Integral a => Handle -> Int -> IO [a]
hGetShorts h n = map fromIntegral <$> (hGetWords h n :: IO [Word16])

hGetInt :: Integral a => Handle -> IO a
hGetInt = (fromIntegral <$>) . (hGetWord :: Handle -> IO Word32)

hGetInts :: Integral a => Handle -> Int -> IO [a]
hGetInts h n = map fromIntegral <$> (hGetWords h n :: IO [Word32])

hGetLong :: Integral a => Handle -> IO a
hGetLong = (fromIntegral <$>) . (hGetWord :: Handle -> IO Word64)

hGetLongs :: Integral a => Handle -> Int -> IO [a]
hGetLongs h n = map fromIntegral <$> (hGetWords h n :: IO [Word64])

hPutByte :: Handle -> Word8 -> IO ()
hPutByte = hPutWord

hPutBytes :: Handle -> [Word8] -> IO ()
hPutBytes = hPutWords

hPutShort :: Handle -> Word16 -> IO ()
hPutShort = hPutWord

hPutShorts :: Handle -> [Word16] -> IO ()
hPutShorts = hPutWords

hPutInt :: Handle -> Word32 -> IO ()
hPutInt = hPutWord

hPutInts :: Handle -> [Word32] -> IO ()
hPutInts = hPutWords

hPutLong :: Handle -> Word64 -> IO ()
hPutLong = hPutWord

hPutLongs :: Handle -> [Word64] -> IO ()
hPutLongs = hPutWords
