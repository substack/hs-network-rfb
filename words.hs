module Data.Word.Convert (
    Words, flipEndian, isBigEndian, toBigEndian, toLittleEndian
) where

import Data.Word (Word8, Word16, Word32, Word64)
import Foreign (castPtr, peek, poke, newArray, peekArray, Ptr)
import Foreign.Storable (Storable, sizeOf)
import System.IO.Unsafe (unsafePerformIO)

class Storable a => Words a where
    fromWords :: Storable b => [b] -> [a]

instance Words Word8 where fromWords = fromWords' 8
instance Words Word16 where fromWords = fromWords' 16
instance Words Word32 where fromWords = fromWords' 32
instance Words Word64 where fromWords = fromWords' 64

instance Words Char where
    fromWords = fromWords' (8 * sizeOf ('z' :: Char))
instance Words Int where
    fromWords = fromWords' (8 * sizeOf (0 :: Int))

fromWords' :: (Storable a, Storable b) => Int -> [a] -> [b]
fromWords' _ [] = []
fromWords' n xs = unsafePerformIO $ do
    ptr <- newArray xs
    let x = head xs
    let ptr' = castPtr ptr
    peekArray (length xs * sizeOf x `div` (n `div` 8)) ptr'

-- Flip the endianness of a single word.
flipEndian :: Words a => a -> a
flipEndian x = head $ fromWords $ reverse $ (fromWords [x] :: [Word8])

-- True if system integers are big-endian.
isBigEndian :: Bool
isBigEndian = (== 1) $ head $ (fromWords ([1] :: [Int]) :: [Word8])

-- Convert a word from the system endianness to big-endian. If the system is
-- already big-endian, nothing changes.
toBigEndian :: Words a => a -> a
toBigEndian x = if isBigEndian then x else flipEndian x

-- Convert a word from the system endianness to little-endian. If the system is
-- already little-endian, nothing changes.
toLittleEndian :: Words a => a -> a
toLittleEndian x = if isBigEndian then flipEndian x else x
