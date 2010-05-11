module Network.RFB (
    SecurityType(..), PixelFormat(..), FrameBuffer(..), RFB(..), Update(..),
    Config(..), Rectangle(..), Encoding(..),
    connect, getUpdate,
    sendKeyEvent, sendKeyPress, sendPointer, sendClipboard, setEncodings,
) where

import Network (connectTo, PortID(..), HostName)
import Control.Arrow ((***),(&&&))
import Control.Applicative ((<$>))
import Control.Monad (when, unless, join, replicateM, foldM)

import Data.Char (ord, chr)
import qualified Data.Map as M

import System.IO (Handle(..), hGetLine, hPutStrLn, hFlush)
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString.Lazy as BS
import Data.Word (Word8,Word16,Word32,Word64)

type Point = (Int,Int)
type Size = (Int,Int)

data GetBytes a = GetBytes Int (Get a)
runGetBytes :: Handle -> GetBytes a -> IO a
runGetBytes fh (GetBytes size f) = runGet f <$> BS.hGet fh size

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
} deriving (Read, Show, Eq)

data FrameBuffer = FrameBuffer {
    fbSize :: (Int,Int),
    fbPixelFormat :: PixelFormat,
    fbImage :: BS.ByteString,
    fbName :: BS.ByteString
}

data RFB = RFB {
    rfbHandle :: Handle,
    rfbVersion :: (Int,Int), -- (major, minor)
    rfbSecurityType :: SecurityType,
    rfbFB :: FrameBuffer,
    rfbShared :: Bool
}

data Update
    = FrameBufferUpdate { rectangles :: [Rectangle] }
    | ColorMapUpdate
    | BellUpdate
    | ClipboardUpdate BS.ByteString

data Rectangle = Rectangle {
    rectPos :: Point,
    rectSize :: Size,
    rectEncoding :: Encoding
}

data Encoding
    = RawEncoding { rawImage :: BS.ByteString }
    | CopyRectEncoding { copyRectPos :: Point }

data Config = Config {
   shared :: Bool,
   securityType :: SecurityType
}

defaultConfig = Config { shared = True, securityType = None }

connect :: Config -> HostName -> PortID -> IO RFB
connect config host port = do
    sock <- connectTo host port
    hPutStrLn sock "RFB 003.008" >> hFlush sock
    let
        rfb = RFB sock (3,8) cSec undefined cShared
        cSec = securityType config
        cShared = shared config
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
securityHandshake rfb@RFB{ rfbHandle = sock } = do
    secLen <- fromIntegral . runGet getWord8 <$> BS.hGet sock 1
    secTypes <- BS.unpack <$> BS.hGet sock secLen
    
    when (secLen == 0) $ do
        msgLen <- fromIntegral . runGet getWord8 <$> BS.hGet sock 1
        msg <- (map (toEnum . fromEnum) <$> BS.unpack) <$> BS.hGet sock msgLen
        fail $ "Connection failed with message: " ++ msg
    
    let secNum = securityTypes M.! rfbSecurityType rfb
    
    -- TODO: other security types with auth type
    unless (secNum `elem` secTypes) $ do
        fail "Authentication mode not supported on remote"
    
    BS.hPut sock $ runPut (putWord8 secNum)
    hFlush sock
    
    -- note: < (3,8) doesn't send this for None
    secRes <- runGet getWord32be <$> BS.hGet sock 4
    when (secRes /= 0) $ do
        msgLen <- fromIntegral . runGet getWord8 <$> BS.hGet sock 1
        msg <- (map (toEnum . fromEnum) <$> BS.unpack) <$> BS.hGet sock msgLen
        fail $ "Security handshake failed with message: " ++ msg
    return rfb

initHandshake :: Handshake
initHandshake rfb = do
    let sock = rfbHandle rfb
    -- client init sends whether or not to share the desktop
    BS.hPut sock (BS.pack [toEnum $ fromEnum $ rfbShared rfb]) >> hFlush sock
    
    -- server init
    fb <- getFrameBuffer rfb
    return $ rfb { rfbFB = fb }
 
parsePixelFormat :: BS.ByteString -> PixelFormat
parsePixelFormat = runGet $ do
    [ bitsPerPixel, depth, bigEndian, trueColor ]
        <- replicateM 4 (fromIntegral <$> getWord8)
    [ redMax, greenMax, blueMax ]
        <- replicateM 3 (fromIntegral <$> getWord16be)
    [ redShift, greenShift, blueShift ]
        <- replicateM 3 (fromIntegral <$> getWord8)
    skip 3
    
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

getFrameBuffer :: RFB -> IO FrameBuffer
getFrameBuffer RFB{ rfbHandle = sock } = do
    [width, height] <- (<$> BS.hGet sock 4)
        $ runGet $ replicateM 2 (fromIntegral <$> getWord16be)
    pf <- parsePixelFormat <$> BS.hGet sock 16
    size <- fromIntegral . runGet getWord32be <$> BS.hGet sock 4
    name <- BS.hGet sock size
    return $ FrameBuffer {
        fbSize = (width,height),
        fbPixelFormat = pf,
        fbImage = BS.empty,
        fbName = name
    }

getUpdate :: RFB -> IO Update
getUpdate rfb@RFB{ rfbFB = fb, rfbHandle = sock } = do
    BS.hPut sock $ runPut $ do
        mapM_ putWord8 [3,1]
        let (width,height) = fbSize fb
        mapM_ putWord16be $ map fromIntegral [ 0, 0, width, height ]
    hFlush sock
    
    msgType <- fromIntegral . runGet getWord8 <$> BS.hGet sock 1
    case msgType of
        0 -> do
            rectSize <- (<$> BS.hGet sock 3)
                $ fromIntegral . runGet (skip 1 >> getWord16be)
            FrameBufferUpdate <$> replicateM rectSize (getRectangle rfb)
        
        1 -> do -- color map update
            fail "color map not implemented"
            return ColorMapUpdate
        
        2 -> do -- bell update
            fail "bell not implemented"
            return BellUpdate
        
        3 -> do -- clipboard update
            BS.hGet sock 3
            clip <- BS.hGet sock =<<
                (fromIntegral . runGet getWord32be <$> BS.hGet sock 4)
            return $ ClipboardUpdate clip
        
        _ -> fail $ "Unknown update message type: " ++ show msgType

-- | You can import Graphics.X11.Types and have access to all the xK_ Word32
-- constants.
sendKeyEvent :: RFB -> Bool -> Word32 -> IO ()
sendKeyEvent RFB{ rfbHandle = sock } keyDown key = do
    BS.hPut sock $ runPut $ do
        mapM putWord8 [ 4, toEnum $ fromEnum keyDown, 0, 0 ]
        putWord32be key
    hFlush sock

sendKeyPress :: RFB -> Word32 -> IO ()
sendKeyPress rfb key =
    sendKeyEvent rfb True key >> sendKeyEvent rfb False key

sendPointer :: RFB -> Word8 -> Word16 -> Word16 -> IO ()
sendPointer RFB{ rfbHandle = sock } buttonMask x y = do
    BS.hPut sock $ runPut $ do
        mapM_ putWord8 [5,buttonMask]
        mapM_ putWord16be [x,y]
    hFlush sock

sendClipboard :: RFB -> BS.ByteString -> IO ()
sendClipboard RFB{ rfbHandle = sock } clip = do
    BS.hPut sock $ runPut $ do
        mapM putWord8 [6,0,0,0]
        putWord64be $ fromIntegral $ BS.length clip
    BS.hPut sock clip
    hFlush sock

setEncodings :: RFB -> [Int] -> IO ()
setEncodings RFB{ rfbHandle = sock } encodings = do
    BS.hPut sock $ runPut $ do
        mapM putWord8 [2,0]
        putWord16be $ fromIntegral $ length encodings
        putWord32be $ fromIntegral $ sum $ map ((2 ^ 32) -) encodings
    hFlush sock

getRectangle :: RFB -> IO Rectangle
getRectangle rfb@RFB{ rfbHandle = sock } = do
    let pf = fbPixelFormat $ rfbFB rfb
        bits = pfBitsPerPixel pf
    
    rect <- (<$> BS.hGet sock 8) $ runGet $ do
        [ dstX, dstY, w, h ] <- map fromIntegral <$> replicateM 4 getWord16be
        return $ Rectangle {
            rectPos = (dstX,dstY),
            rectSize = (w,h),
            rectEncoding = undefined
        }
    
    msgType <- (<$> BS.hGet sock 4) $ runGet $ getWord32be
    ((\x -> rect { rectEncoding = x }) <$>) $ case msgType of
        0 -> case bits of
            32 -> RawEncoding <$> BS.hGet sock size
                where size = fromIntegral $ 4 * (uncurry (*) $ rectSize rect)
            _ -> fail $ "unsupported bits per pixel: " ++ show bits
        
        1 -> (<$> BS.hGet sock 4) $ runGet $ do
            [x,y] <- map fromIntegral <$> replicateM 2 getWord16be
            return $ CopyRectEncoding (x,y)
        
        _ -> fail "unsupported encoding"
