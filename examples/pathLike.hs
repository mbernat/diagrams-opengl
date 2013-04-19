{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.OpenGL.TwoD
import Diagrams.Backend.OpenGL.TwoD.CmdLine
import Diagrams.Core.Points

ts = mconcat . take 3 . iterate (rotateBy (1/9)) $ eqTriangle 1

example :: Diagram OpenGL R2
example = (ts ||| stroke ts ||| strokeT ts ||| fromVertices ts) # fc red

main = defaultMain example
