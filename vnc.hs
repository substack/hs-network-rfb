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
import Data.Word

data SecurityType = None
    deriving (Eq, Ord, Show)
securityTypes :: M.Map SecurityType Word8
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

connect' :: HostName -> PortID -> IO RFB
connect' = connect newRFB

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
    secTypes <- hGetBytes sock secLen
    
    when (secLen == 0) $ do
        msg <- map (toEnum . fromEnum)
            <$> (hGetBytes sock =<< hGetByte sock)
        fail $ "Connection failed with message: " ++ msg
    
    let secNum = securityTypes M.! rfbSecurityType rfb
    
    -- TODO: other security types with auth type
    unless (secNum `elem` secTypes) $ do
        fail "Authentication mode not supported on remote"
    
    hPutWord sock secNum >> hFlush sock
    
    -- note: < (3,8) doesn't send this for None
    secRes <- hGetInt sock
    when (secRes /= 0) $ do
        msg <- map (toEnum . fromEnum) <$> (hGetBytes sock =<< hGetByte sock)
        fail $ "Security handshake failed with message: " ++ msg
    
    return rfb

initHandshake :: Handshake
initHandshake rfb = do
    let sock = rfbHandle rfb
    -- client init sends whether or not to share the desktop
    hPutByte sock (toEnum $ fromEnum $ rfbShared rfb) >> hFlush sock
    
    -- server init
    fb <- hGetFrameBuffer rfb
    return $ rfb { rfbFB = fb }
 
hGetPixelFormat :: RFB -> IO PixelFormat
hGetPixelFormat rfb = do
    let sock = rfbHandle rfb
    [ bitsPerPixel, depth, bigEndian, trueColor ] <- hGetBytes sock 4
    [ redMax, greenMax, blueMax ] <- hGetBytes sock 4
    [ redShift, greenShift, blueShift ] <- hGetBytes sock 4
    
    hGetBytes sock 3 -- padding
    
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

hGetFrameBuffer :: RFB -> IO FrameBuffer
hGetFrameBuffer rfb = do
    let sock = rfbHandle rfb
    [ width, height ] <- hGetShorts sock 2
    pixelFormat <- hGetPixelFormat rfb
    name <- map toEnum <$> (hGetBytes sock =<< hGetInt sock)
    
    return $ FrameBuffer {
        fbWidth = width,
        fbHeight = height,
        fbPixelFormat = pixelFormat,
        fbName = name
    }
