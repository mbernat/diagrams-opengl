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
p = fromOffsets [v1,v2, v1, v2,v1,-v2,v1]

p2_, p2_closed :: Path R2
[p2_, p2_closed] = map pathFromTrail [open, closed]
 where open = fromOffsets [v2, v1]
       closed = closeTrail open

d :: Diagram OpenGL R2
d = stroke p # lc red <>
    stroke p2_ # lc blue <>
    (stroke $ fromOffsets [v2, v2, v1]) <>
    stroke (p2_closed) # lc green # translate (v1+v2)
