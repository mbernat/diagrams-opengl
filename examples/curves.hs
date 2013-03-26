import Diagrams.Prelude
import Diagrams.Backend.OpenGL
import Diagrams.Backend.OpenGL.CmdLine

main :: IO ()
main = defaultMain d

d :: Diagram OpenGL R2
d = circle 1 # fc green # lc blue
