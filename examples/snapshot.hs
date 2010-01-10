-- snapshot.hs - connect to a vnc server and render the screen to a png
import Network.RFB
import Control.Monad
import qualified Graphics.GD as GD
import System.Environment (getArgs)

main :: IO ()
main = do
    (host:port:file:_) <- getArgs
    rfb <- connect' host (PortNumber $ read port)
    update <- getUpdate rfb
    mapM_ (render rfb) $ fbuRectangles update
    GD.savePngFile file $ fbImage $ rfbFB rfb
    putStrLn $ "Saved image to " ++ file
