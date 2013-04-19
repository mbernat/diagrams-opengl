import Diagrams.Prelude
import Diagrams.Backend.OpenGL.TwoD
import Diagrams.Backend.OpenGL.TwoD.CmdLine
import Diagrams.Core.Points

loopyStar :: Diagram OpenGL R2
loopyStar = mconcat . map (cubicSpline True)
          . pathVertices
          . star (StarSkip 3)
          $ rp

rp :: [P2]
rp = regPoly 7 1

example :: Diagram OpenGL R2
example = loopyStar # fillRule EvenOdd # fc red
      ||| strutX 1
      ||| loopyStar # fillRule Winding # fc green
      ||| strutX 1
      ||| loopyStar
      ||| strutX 1
      ||| stroke ring # fc purple # fillRule EvenOdd

ring :: Path R2
ring = circle 1 <> circle 0.75

main :: IO ()
main = defaultMain example
