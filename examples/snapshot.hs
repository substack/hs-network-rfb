-- snapshot.hs - connect to a vnc server and render the screen to a png
import qualified Network.RFB as RFB
import Control.Applicative ((<$>))
import qualified Graphics.Transform.Magick.Images as Mgk
import qualified Graphics.Transform.Magick.Types as Mgk
import System.Environment (getArgs)
import Data.Word (Word16,Word32)
import Data.Maybe (catMaybes,fromJust)
import Data.List.Split (splitEvery)
import Foreign (peekArray)

main :: IO ()
main = do
    (host:port:file:_) <- getArgs
    snapshot host (read port) file

rgba = Mgk.PixMap [Mgk.R,Mgk.G,Mgk.B,Mgk.A]

snapshot :: String -> Word16 -> FilePath -> IO ()
snapshot host port file = do
    rfb <- RFB.simpleConnect host port
    let (w,h) = RFB.fbSize $ RFB.rfbFB rfb
    RFB.requestFrameBuffer rfb 0 0 (fromIntegral w) (fromIntegral h)
    Mgk.writeImage file =<< getImage rfb

getImage :: RFB.RFB -> IO Mgk.HImage
getImage rfb = do
    update <- RFB.getUpdate rfb
    case update of
        RFB.FrameBufferUpdate rects -> do
            print $ length rects
            fst . fromJust <$> asMagick (head rects)
            -- Mgk.mosaic . catMaybes <$> mapM asMagick rects
        _ -> getImage rfb

asMagick :: RFB.Rectangle -> IO (Maybe (Mgk.HImage,Mgk.Rectangle))
asMagick (RFB.Rectangle (x,y) (w,h) (RFB.RawEncoding ptr bytes)) = do
    print (x,y)
    print (w,h)
    im <- Mgk.constituteImage rgba . splitEvery (w * 4)
        <$> peekArray (w * h * 4) ptr
    return $ Just (im,Mgk.Rectangle (fromIntegral w) (fromIntegral h) x y)

asMagick _ = return Nothing
