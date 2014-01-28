{-# LANGUAGE ViewPatterns               #-}

module Diagrams.Backend.OpenGL.TwoD.Tesselate
       ( tessRegion, TessWinding(..) ) where

import           System.IO.Unsafe
import Diagrams.Prelude
import Graphics.Rendering.OpenGL
import Graphics.Rendering.Util

tessRegion :: TessWinding -> [[P2]] -> [[P2]]
tessRegion fr trs = renderTriangulation $ unsafePerformIO $
  triangulate fr 0.0001 (Normal3 0 0 0)
    (\_ _ -> 0) $
    ComplexPolygon [ComplexContour (map createVertex trail) | trail <- trs]
 where createVertex (unp2 -> (x,y)) =
          AnnotatedVertex (Vertex3 (r2f x) (r2f y) 0) (0::Int)
       renderTriangulation (Triangulation ts) = map renderTriangle ts
       renderTriangle (Triangle a b c) = map deAnnotate [a, b, c]
       deAnnotate (AnnotatedVertex (Vertex3 x y _) _) = p2 (r2f x, r2f y)
