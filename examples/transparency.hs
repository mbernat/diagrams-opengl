import Diagrams.Prelude
import Diagrams.Backend.OpenGL.TwoD
import Diagrams.Backend.OpenGL.TwoD.CmdLine

colors  = map (blue `withOpacity`) [0.1, 0.2 .. 1.0]
example = hcat' with { catMethod = Distrib, sep = 1 }
          (zipWith fcA colors (repeat (circle 1)))

main = defaultMain example
