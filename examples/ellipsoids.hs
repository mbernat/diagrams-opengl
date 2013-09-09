import Diagrams.Prelude hiding (doRender)
import Diagrams.Backend.OpenGL.ThreeD
import Diagrams.Backend.OpenGL.ThreeD.CmdLine
import Diagrams.ThreeD.Types

import Diagrams.ThreeD.Shapes

collection :: Diagram OpenGL R3
collection = mconcat $ map around [2*pi/8.0, 4*pi/8.0 .. 2*pi]

around :: Double -> Diagram OpenGL R3
around θ = translate r . scale 0.1 $ sphere where
  r = r3 (0.8 * sin θ, 0.8 * cos θ, 0)

main :: IO ()
main = do
  --print . toDoc $ polarSphere 5 5
  --fileOutputDemo "ellipsoids.png" $ example
  defaultMain collection
