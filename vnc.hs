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
    secLen <- hGetByte sock
    secTypes <- hTakeBytes sock secLen
    
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
    -- client init sends whether or not to share the desktop
    hPutChar sock (chr $ fromEnum $ rfbShared rfb) >> hFlush sock
    
    -- server init
    [ width, height ] <- replicateM 2 $ hGetShort sock
    
    [ bitsPerPixel, depth, bigEndian, trueColor ]
        <- replicateM 4 $ hGetByte sock
    
    [ redMax, greenMax, blueMax ] <- replicateM 3 $ hGetShort sock
    
    [ redShift, greenShift, blueShift ]
        <- replicateM 3 $ hGetByte sock
    
    name <- hTake sock =<< hGetInt sock
    
    hTake sock 3 -- padding
    
    let
        pixelFormat = PixelFormat {
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
        
        fb = FrameBuffer {
            fbWidth = width,
            fbHeight = height,
            fbPixelFormat = pixelFormat,
            fbName = name
        }
    
    return $ rfb { rfbFB = fb }
