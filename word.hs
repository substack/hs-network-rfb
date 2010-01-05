module System.IO.Word where

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
