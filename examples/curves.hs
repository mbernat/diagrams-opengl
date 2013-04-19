import Diagrams.Prelude
import Diagrams.Backend.OpenGL.TwoD
import Diagrams.Backend.OpenGL.TwoD.CmdLine

main :: IO ()
main = defaultMain d

d :: Diagram OpenGL R2
d = circle 1 # fc green # lc blue
