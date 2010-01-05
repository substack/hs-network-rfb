-- 
module Main where

import Network (connectTo, PortID(PortNumber), HostName)
import System.IO
import Control.Arrow ((***))
import Control.Applicative ((<$>))
import Control.Monad (when, unless, join, replicateM, replicateM_, foldM)
import Data.Char (ord, chr)
import qualified Data.Map as M

import System.IO.Word

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

connect :: RFB -> HostName -> PortID -> IO RFB
connect rfb host port = do
    sock <- connectTo host port
    hPutStrLn sock "RFB 003.008" >> hFlush sock
    foldM (flip ($)) rfb { rfbHandle = sock }
        [ versionHandshake, securityHandshake, initHandshake ]

type Handshake = RFB -> IO RFB

versionHandshake :: Handshake
versionHandshake rfb = do
    let sock = rfbHandle rfb
    version <- join (***) (read . tail) . splitAt 4 . drop 3
        <$> hGetLine sock :: IO (Int,Int)
    when (version < rfbVersion rfb) $ do
        fail $ "Maximum supported version "
            ++ show version ++ " at remote end is < (3,8)"
    return rfb

securityHandshake :: Handshake
securityHandshake rfb = do
    let sock = rfbHandle rfb
    secLen <- hGetByte sock
    secTypes <- hTakeBytes sock secLen
    
    when (secLen == 0) $ do
        msg <- hTake sock =<< hGetInt sock
        fail $ "Connection failed with message: " ++ msg
    
    let secNum = securityTypes M.! rfbSecurityType rfb
    
    -- TODO: other security types with auth type
    unless (secNum `elem` secTypes) $ do
        fail "Authentication mode not supported on remote"
    
    hPutChar sock (chr secNum) >> hFlush sock
    
    secRes <- hGetInt sock -- note: < (3,8) doesn't send this for None
    when (secRes /= 0) $ do
        msg <- hTake sock =<< hGetInt sock
        fail $ "Security handshake failed with message: " ++ msg
    
    return rfb

initHandshake :: Handshake
initHandshake rfb = do
    let sock = rfbHandle rfb
    -- client init sends whether or not to share the desktop
    hPutChar sock (chr $ fromEnum $ rfbShared rfb) >> hFlush sock
    
    -- server init
    fb <- hGetFrameBuffer sock
    return $ rfb { rfbFB = fb }
    
hGetPixelFormat :: Handle -> IO PixelFormat
hGetPixelFormat sock = do
    [ bitsPerPixel, depth, bigEndian, trueColor ]
        <- replicateM 4 $ hGetByte sock
    
    [ redMax, greenMax, blueMax ] <- replicateM 3 $ hGetShort sock
    
    [ redShift, greenShift, blueShift ]
        <- replicateM 3 $ hGetByte sock
    
    hTake sock 3 -- padding
    
    return $ PixelFormat {
        pfBitsPerPixel = bitsPerPixel, 
        pfDepth = depth,
        pfBigEndian = bigEndian /= 0,
        pfTrueColor = trueColor /= 0,
        pfRedMax = redMax,
        pfGreenMax = greenMax,
        pfBlueMax = blueMax,
        pfRedShift = redShift,
        pfGreenShift = greenShift,
        pfBlueShift = blueShift
    }

hGetFrameBuffer :: Handle -> IO FrameBuffer
hGetFrameBuffer sock = do
    [ width, height ] <- replicateM 2 $ hGetShort sock
    pixelFormat <- hGetPixelFormat sock
    name <- hTake sock =<< hGetInt sock
    
    return $ FrameBuffer {
        fbWidth = width,
        fbHeight = height,
        fbPixelFormat = pixelFormat,
        fbName = name
    }
