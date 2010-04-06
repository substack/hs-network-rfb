module Network.RFB (
    SecurityType(..), PixelFormat(..), FrameBuffer(..), RFB(..), Update(..),
    Rectangle(..), Encoding(..), PortID(..),
    connect, connect', getUpdate, getImage, renderImage, renderImage', newRFB,
    sendKeyEvent, sendKeyPress, sendPointer, sendClipboard, setEncodings,
) where

import Network (connectTo, PortID(..), HostName)
import Control.Arrow ((***),(&&&))
import Control.Applicative ((<$>))
import Control.Monad (when, unless, join, replicateM, foldM, msum, liftM2)

import Data.Char (ord, chr)
import qualified Data.Map as M
import Data.List.Split (splitEvery)

import System.IO (Handle(..), hGetLine, hPutStrLn, hFlush)
import Data.Binary.Get (
    Get(..),runGet,skip,getBytes,getWord8,getWord16be,getWord32be)
import Data.Binary.Put (
    runPut,putWord8,putWord16be,putWord32be,putWord64be,putByteString)

import Data.ByteString.Lazy (ByteString,hGet,hPut)
import Data.Word (Word8,Word16,Word32,Word64)

import qualified Graphics.GD as GD
import Foreign.C.Types (CInt)

import Control.Concurrent.STM.TMVar
import Control.Monad.STM (atomically)

data GetBytes a = GetBytes Int (Get a)
runGetBytes :: Handle -> GetBytes a -> IO a
runGetBytes fh (GetBytes size f) = runGet f <$> hGet fh size

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
    fbWidth :: Int,
    fbHeight :: Int,
    fbPixelFormat :: PixelFormat,
    fbImage :: TMVar GD.Image,
    fbName :: String
}

data RFB = RFB {
    rfbHandle :: Handle,
    rfbVersion :: (Int,Int), -- (major, minor)
    rfbSecurityType :: SecurityType,
    rfbFB :: FrameBuffer,
    rfbHost :: HostName,
    rfbPort :: PortID,
    rfbShared :: Bool
}

data Update =
    FrameBufferUpdate { rectangles :: [Rectangle] } |
    ColorMapUpdate |
    BellUpdate |
    ClipboardUpdate [Word8]

data Rectangle = Rectangle {
    rectPos :: GD.Point,
    rectSize :: GD.Size,
    rectEncoding :: Encoding
}

data Encoding =
    RawEncoding { rawImage :: ByteString } |
    CopyRectEncoding { copyRectPos :: GD.Point }

fromRGBA :: GD.Size -> [Word32] -> IO GD.Image
fromRGBA size@(w,h) pixels = do
    let
        xy = [ (x,y) | y <- [ 0 .. h - 1 ], x <- [ 0 .. w - 1 ] ]
        px = map fromIntegral pixels :: [CInt]
    im <- GD.newImage size
    sequence_ [ GD.setPixel (x,y) p im | ((x,y),p) <- zip xy px ]
    return im

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
securityHandshake rfb@RFB{ rfbHandle = sock } = do
    secLen <- hGet sock 1
    secTypes <- hGet sock secLen
    
    when (secLen == 0) $ do
        msg <- map (toEnum . fromEnum)
            <$> (hGet sock =<< hGet sock 1)
        fail $ "Connection failed with message: " ++ msg
    
    let secNum = securityTypes M.! rfbSecurityType rfb
    
    -- TODO: other security types with auth type
    unless (secNum `elem` secTypes) $ do
        fail "Authentication mode not supported on remote"
    
    runPut (putWord8 secNum) >> hFlush sock
    
    -- note: < (3,8) doesn't send this for None
    secRes <- runGet getWord32be =<< hGet sock 4
    when (secRes /= 0) $ do
        msg <- map (toEnum . fromEnum) <$> (hGet sock =<< hGet sock 1)
        fail $ "Security handshake failed with message: " ++ msg
    return rfb

initHandshake :: Handshake
initHandshake rfb = do
    let sock = rfbHandle rfb
    -- client init sends whether or not to share the desktop
    hPut sock (toEnum $ fromEnum $ rfbShared rfb) >> hFlush sock
    
    -- server init
    fb <- runGetBytes =<< (parseFrameBuffer <$> hGet sock 24)
    im <- atomically =<< newTMVar <$> GD.newImage (fbWidth &&& fbHeight $ fb)
    return $ rfb { rfbFB = fb { fbImage = im } }
 
parsePixelFormat :: ByteString -> PixelFormat
parsePixelFormat = runGet $ do
    [ bitsPerPixel, depth, bigEndian, trueColor ] <- replicateM 4 getWord8
    [ redMax, greenMax, blueMax ] <- replicateM 3 getWord16be
    [ redShift, greenShift, blueShift ] <- replicateM 3 getWord8
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

parseFrameBuffer :: ByteString -> GetBytes FrameBuffer
parseFrameBuffer = runGet $ do
    [width, height] <- replicateM 2 getWord16be
    pf <- parsePixelFormat =<< getBytes 16
    size <- getWord32be
    return $ GetBytes size $ do
        name <- getBytes size
        return $ FrameBuffer {
            fbWidth = width,
            fbHeight = height,
            fbPixelFormat = pf,
            fbImage = undefined,
            fbName = name
        }

renderImage :: RFB -> Rectangle -> IO ()
renderImage rfb rect = do
    let fb = rfbFB rfb
    case rectEncoding rect of
        RawEncoding { rawImage = srcIm } -> do
            let tm = fbImage fb
            dstIm <- atomically $ takeTMVar tm
            GD.copyRegion
                (0,0) (rectSize rect) srcIm
                (rectPos rect) dstIm
            atomically $ putTMVar tm dstIm

renderImage' :: RFB -> [Rectangle] -> IO ()
renderImage' rfb = mapM_ (renderImage rfb)

getImage :: RFB -> IO GD.Image
getImage rfb = atomically $ readTMVar $ fbImage $ rfbFB rfb

getUpdate :: RFB -> IO Update
getUpdate rfb@RFB{ rfbFB = fb, rfbHandle = sock } = do
    hPut sock $ runPut $ do
        mapM_ putWord8 [3,1]
        mapM_ putWord16be $ map fromIntegral [ 0, 0, fbWidth fb, fbHeight fb ]
    hFlush sock
    
    (=<< hGet sock 1) $ \msgType -> case msgType of
        0 -> do
            rectSize <- (hGet sock 3 <$>) $ runGet $ skip 1 >> getWord16be
            FrameBufferUpdate <$> replicateM rectSize (getRectangle rfb)
        
        1 -> do -- color map update
            fail "color map not implemented"
            return ColorMapUpdate
        
        2 -> do -- bell update
            fail "bell not implemented"
            return BellUpdate
        
        3 -> do -- clipboard update
            hGet sock 3
            clip <- hGet sock =<< (runGet getWord32be <$> hGet sock 4)
            return $ ClipboardUpdate clip
        
        _ -> fail $ "Unknown update message type: " ++ show msgType

-- | You can import Graphics.X11.Types and have access to all the xK_ Word32
-- constants.
sendKeyEvent :: RFB -> Bool -> Word32 -> IO ()
sendKeyEvent RFB{ rfbHandle = sock } keyDown key = do
    hPut sock =<< runPut $ do
        mapM putWord8 [ 4, toEnum $ fromEnum keyDown, 0, 0 ]
        putWord32be key
    hFlush sock

sendKeyPress :: RFB -> Word32 -> IO ()
sendKeyPress rfb key =
    sendKeyEvent rfb True key >> sendKeyEvent rfb False key

sendPointer :: RFB -> Word8 -> Word16 -> Word16 -> IO ()
sendPointer RFB{ rfbHandle = sock } buttonMask x y = do
    hPut sock $ runPut $ do
        mapM putWord8 [5,buttonMask]
        mapM putWord16be [x,y]
    hFlush sock

sendClipboard :: RFB -> ByteString -> IO ()
sendClipboard RFB{ rfbHandle = sock } clip = do
    hPut sock $ runPut $ do
        mapM putWord8 [6,0,0,0]
        putWord64be $ fromIntegral $ length clip
        putByteString clip
    hFlush sock

setEncodings :: RFB -> [Int] -> IO ()
setEncodings RFB{ rfbHandle = sock } encodings = do
    hPut sock $ runPut $ do
        mapM putWord8 [2,0]
        putWord16be $ length encodings
        putWord32be $ map ((2 ^ 32) -) encodings
    hFlush sock

getRectangle :: RFB -> IO Rectangle
getRectangle rfb@RFB{ rfbHandle = sock } = do
    let pf = fbPixelFormat $ rfbFB rfb
        bits = pfBitsPerPixel pf
    
    rect <- (<$> hGet sock 8) $ runGet $ do
        [ dstX, dstY, w, h ] <- map fromIntegral <$> replicateM 4 getWord16be
        return $ Rectangle {
            rectPos = (dstX,dstY),
            rectSize = (w,h),
            rectEncoding = undefined
        }
    
    msgType <- (<$> hGet sock 4) $ runGet $ getWord32be
    ((\x -> rect { rectEncoding = x }) <$>) $ case msgType of
        0 -> case bits of
            32 -> RawEncoding <$> hGet sock size
                where size = fromIntegral $ 4 * (uncurry (*) $ rectSize rect)
            _ -> fail $ "unsupported bits per pixel: " ++ show bits
        
        1 -> (<$> hGet sock 4) $ runGet $ do
            [x,y] <- map fromIntegral <$> replicateM 2 getWord16be
            return $ CopyRectEncoding (x,y)
        
        _ -> fail "unsupported encoding"
