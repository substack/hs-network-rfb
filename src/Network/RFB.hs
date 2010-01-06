-- 
module Main where

import Network (connectTo, PortID(PortNumber), HostName)
import System.IO
import Control.Arrow ((***))
import Control.Applicative ((<$>))
import Control.Monad (when, unless, join, replicateM, replicateM_, foldM)
import Data.Char (ord, chr)
import qualified Data.Map as M

import System.WordIO
import Data.Word
import Data.Word.Convert

import qualified Data.Array as A

endian :: (Integral a, Words a, Num b) => Bool -> a -> b
endian isBig = fromIntegral
    . (if isBig then fromBigEndian else fromLittleEndian)
    
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

type PixelData = A.Array Int RGB
type RGB = (Word8, Word8, Word8)

data FrameBuffer = FrameBuffer {
    fbWidth :: Int,
    fbHeight :: Int,
    fbPixelFormat :: PixelFormat,
    fbPixelData :: PixelData,
    fbIncrement :: Word8,
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
    [ redMax, greenMax, blueMax ] <- hGetShorts sock 3 :: IO [Word16]
    [ redShift, greenShift, blueShift ] <- hGetBytes sock 3
    hGetBytes sock 3 -- padding
    
    return $ PixelFormat {
        pfBitsPerPixel = bitsPerPixel, 
        pfDepth = depth,
        pfBigEndian = bigEndian /= 0,
        pfTrueColor = trueColor /= 0,
        pfRedMax = endian (bigEndian /= 0) redMax,
        pfGreenMax = endian (bigEndian /= 0) greenMax,
        pfBlueMax = endian (bigEndian /= 0) blueMax,
        pfRedShift = redShift,
        pfGreenShift = greenShift,
        pfBlueShift = blueShift
    }

hGetFrameBuffer :: RFB -> IO FrameBuffer
hGetFrameBuffer rfb = do
    let sock = rfbHandle rfb
    [ width', height' ] <- (hGetWords sock 2 :: IO [Word16])
    pf <- hGetPixelFormat rfb
    let
        bigEndian = pfBigEndian pf
        [ width, height ] = map (endian bigEndian) [ width', height' ]
    
    nameLen <- endian bigEndian <$> (hGetWord sock :: IO Word32)
    name <- map toEnum <$> hGetBytes sock nameLen
    
    return $ FrameBuffer {
        fbWidth = width,
        fbHeight = height,
        fbPixelFormat = pf,
        fbIncrement = 0,
        fbPixelData = A.listArray (0, width * height - 1) $ repeat (0,0,0),
        fbName = name
    }

data Update =
    FrameBufferUpdate [Rectangle] |
    ColorMapUpdate |
    BellUpdate |
    ClipboardUpdate [Word8]

data Rectangle = Rectangle {
    rectX :: Int,
    rectY :: Int,
    rectWidth :: Int,
    rectHeight :: Int,
    rectEncoding :: Encoding
}

data Encoding =
    RawEncoding PixelData

render :: RFB -> Rectangle -> RFB
render rfb rect = undefined

getUpdate :: RFB -> IO Update
getUpdate rfb = do
    let
        fb = rfbFB rfb
        sock = rfbHandle rfb
        pf = fbPixelFormat $ rfbFB rfb
        bigEndian = pfBigEndian pf
    
    hPutBytes sock [ 3, fbIncrement fb ]
    hPutShorts sock $ map fromIntegral [ 0, 0, fbWidth fb, fbHeight fb ]
    hFlush sock
    msgType <- hGetByte sock
    case msgType of
        0 -> do -- framebuffer update
            hGetByte sock -- padding
            
            -- number of rectangles in the queue
            rectLen <- endian bigEndian <$> hGetWord16 sock
            
            putStrLn $ "rectLen = " ++ show rectLen
            FrameBufferUpdate <$> replicateM rectLen (hGetRectangle rfb)
        1 -> do -- color map update
            fail "color map not implemented"
            return ColorMapUpdate
        2 -> do -- bell update
            fail "bell not implemented"
            return BellUpdate
        3 -> do -- clipboard update
            hGetBytes sock 3 -- padding
            clip <- hGetBytes sock =<< hGetInt sock
            return $ ClipboardUpdate clip

sendKey :: RFB -> Bool -> Word32 -> IO ()
sendKey rfb keyDown key = do
    let sock = rfbHandle rfb
    hPutBytes sock [ 4, toEnum $ fromEnum keyDown, 0, 0 ]
    hPutWord sock key

sendPointer :: RFB -> Word8 -> Word16 -> Word16 -> IO ()
sendPointer rfb buttonMask x y = do
    let sock = rfbHandle rfb
    hPutWords sock [ 5, buttonMask ]
    hPutWords sock [ x, y ]

sendClipboard :: RFB -> [Word8] -> IO ()
sendClipboard rfb clip = do
    let sock = rfbHandle rfb
    hPutBytes sock [ 6, 0, 0, 0 ]
    hPutLong sock $ fromIntegral $ length clip
    hPutWords sock clip

setEncodings :: RFB -> [Word32] -> IO ()
setEncodings rfb encodings = do
    let sock = rfbHandle rfb
    hPutBytes sock [ 2, 0 ]
    hPutShort sock $ fromIntegral $ length encodings
    hPutWords sock encodings
    hFlush sock

hGetRectangle :: RFB -> IO Rectangle
hGetRectangle rfb = do
    let
        sock = rfbHandle rfb
        pf = fbPixelFormat $ rfbFB rfb
        bigEndian = pfBigEndian pf
    
    [ x, y, w, h ] <- map (endian bigEndian)
        <$> (hGetWords sock 4 :: IO [Word16])
    
    encType <- hGetInt sock
    encoding <- case encType of
        0 -> do -- raw encoding
            let
                bits = pfBitsPerPixel pf
                pixM = do
                    pixel <- hGetBytes sock $ bits `div` 8
                    case bits of 
                        24 -> return (r,g,b) where [r,g,b] = pixel
                        32 -> return (r,g,b) where [r,g,b,_] = pixel
                        _ -> fail $ show bits ++ " bits?"
            RawEncoding . A.listArray (0, w * h - 1)
                <$> replicateM (w * h) pixM
        _ -> fail "unsupported encoding"
    
    print (w,h)
    return $ Rectangle {
        rectX = x,
        rectY = y,
        rectWidth = w,
        rectHeight = h,
        rectEncoding = encoding
    }
