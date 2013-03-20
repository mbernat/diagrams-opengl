import Diagrams.Prelude hiding (doRender)
import Diagrams.Backend.OpenGL
import Diagrams.Backend.OpenGL.CmdLine

main :: IO ()
main = do
  putStrLn $ show $ renderPath p
  defaultMain d

v1 = r2 (0,1)
v2 = r2 (0.5,-0.5)
p = fromOffsets [v1,v2] :: Path R2
d = stroke p :: Diagram OpenGL R2
