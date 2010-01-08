import Network.RFB
import Control.Monad
import qualified Graphics.GD as GD
import Network (PortID(PortNumber))

main :: IO ()
main = do
    rfb <- connect' "localhost" $ PortNumber 5900
    update <- getUpdate rfb
    mapM_ (render rfb) $ fbuRectangles update
    GD.savePngFile "fb.png" $ fbImage $ rfbFB rfb
