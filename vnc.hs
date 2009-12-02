-- 
module Main where

import Network (connectTo, PortID(PortNumber), HostName)
import System.IO
import Control.Arrow ((***))
import Control.Applicative ((<$>))
import Control.Monad (when, unless, join, replicateM, foldM)
import Data.Char (ord, chr)
import Foreign (malloc, mallocArray, peek, peekArray, Ptr)
import qualified Data.Map as M

data SecurityType = None
    deriving (Eq, Ord, Show)
securityTypes :: M.Map SecurityType Int
securityTypes = M.fromList [(None,1)]

data PixelFormat = PixelFormat {
    pfBitsPerPixel :: Int, 
    pfDepth :: Int,
    pfBigEndian :: Bool,
    pfTrueColor :: Bool,
    pfRedMax :: Int,
    pfGreenMax :: Int,
    pfBlueMax :: Int,
    pfRedShift :: Int,
    pfGreenShift :: Int,
    pfBlueShift :: Int
} deriving (Eq, Ord, Show)

data FrameBuffer = FrameBuffer {
    fbWidth :: Int,
    fbHeight :: Int,
    fbPixelFormat :: PixelFormat,
    fbName :: String
} deriving (Eq, Ord, Show)

data RFB = RFB {
    rfbHandle :: Handle,
    rfbVersion :: (Int,Int), -- (major, minor)
    rfbSecurityType :: SecurityType,
    rfbFB :: FrameBuffer,
    rfbHost :: HostName,
    rfbPort :: PortID,
    rfbShared :: Bool
}

newRFB = RFB {
    rfbHandle = undefined,
    rfbVersion = (3,8),
    rfbSecurityType = None,
    rfbFB = undefined,
    rfbHost = undefined,
    rfbPort = undefined,
    rfbShared = True
}

hGetInt :: Handle -> IO Int
hGetInt h = fromIntegral <$> hGetInteger h 4

hGetShort :: Handle -> IO Int
hGetShort h = fromIntegral <$> hGetInteger h 2

hGetInteger :: Handle -> Int -> IO Integer
hGetInteger h n = do
    let n' = ceiling $ fromIntegral n / 4
    let max' = maxBound :: Int
    ptr <- mallocArray n' :: IO (Ptr Int)
    hGetBuf h ptr n
    sum <$> zipWith (*) (iterate (* (fromIntegral max')) 1)
        <$> map fromIntegral <$> peekArray n' ptr

hTake :: Handle -> Int -> IO String
hTake h n = replicateM n $ hGetChar h

connect :: RFB -> HostName -> PortID -> IO RFB
connect rfb host port = do
    sock <- connectTo host port
    hPutStrLn sock "RFB 003.008" >> hFlush sock
    foldM (\rfb handshake -> handshake sock rfb) rfb
        [ versionHandshake, securityHandshake, initHandshake ]

type Handshake = Handle -> RFB -> IO RFB

versionHandshake :: Handshake
versionHandshake sock rfb = do
    version <- join (***) (read . tail) . splitAt 4 . drop 3
        <$> hGetLine sock :: IO (Int,Int)
    when (version < rfbVersion rfb) $ putStrLn $ error
        $ "Maximum supported version "
            ++ show version ++ " at remote end is < (3,8)"
    return rfb

securityHandshake :: Handshake
securityHandshake sock rfb = do
    secLen <- ord <$> hGetChar sock
    secTypes <- map ord <$> hTake sock secLen
    
    when (secLen == 0) $ do
        msg <- hTake sock =<< hGetInt sock
        putStrLn $ error "Connection failed with message: " ++ msg
    
    let secNum = securityTypes M.! rfbSecurityType rfb
    
    -- TODO: other security types with auth type
    unless (secNum `elem` secTypes)
        $ putStrLn $ error "Authentication mode not supported on remote"
    
    hPutChar sock (chr secNum) >> hFlush sock
    
    secRes <- hGetInt sock -- note: < (3,8) doesn't send this for None
    when (secRes /= 0) $ do
        msg <- hTake sock =<< hGetInt sock
        putStrLn $ error "Security handshake failed with message: " ++ msg
    
    return rfb

initHandshake :: Handshake
initHandshake sock rfb = do
    hPutChar sock (chr $ fromEnum $ rfbShared rfb) >> hFlush sock
    
    return rfb
