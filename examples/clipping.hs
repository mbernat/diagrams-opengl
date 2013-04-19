{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude
import Diagrams.Backend.OpenGL.TwoD.CmdLine

main :: IO ()
main = defaultMain example

example = square 3
        # rotateBy (1/100000) -- If I comment this line I see strange bugs.
                              -- Some triangles are not clipped
        # fc green
        # lw 0.05
        # clipBy (square 3.2 # rotateBy (1/10))
