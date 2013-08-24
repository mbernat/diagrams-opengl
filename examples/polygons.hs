import Diagrams.Prelude hiding (doRender)
import Diagrams.Backend.OpenGL
import Diagrams.Backend.OpenGL.CmdLine

main :: IO ()
main = do
  defaultMain d

v1 :: R2
v1 = r2 (0,1)

v2 :: R2
v2 = r2 (0.5,-0.5)

p :: Path R2
p = pathFromTrail . closeTrail $ fromOffsets [v1,v2]

p2_ :: Path R2
p2_ = pathFromTrail . closeTrail $ fromOffsets [v2, v1]

d :: Diagram OpenGL R2
d = stroke p  # fc green <>
    (stroke p2_ # lc blue # fc cyan # translate (r2 (0,0.5)))
