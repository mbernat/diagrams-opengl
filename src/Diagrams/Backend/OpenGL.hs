{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Diagrams.Backend.OpenGL where

-- General Haskell
import Data.Semigroup
import           Data.Typeable
import qualified Data.Vector.Storable as V

-- Graphics
import Data.Colour.Names
import Graphics.Rendering.OpenGL hiding (RGB, Linear, Render)

-- From Diagrams
import Diagrams.Prelude hiding (Attribute, close, e, (<>))
import Graphics.Rendering.Util

renderPath :: Path R2 -> Render OpenGL R2
renderPath (Path trs) = GlRen box $ fmap renderTrail trs where
  box = mconcat . map (boundingBox . snd) $ trs

renderTrail :: (P2, Trail R2) -> GlPrim
renderTrail (p0, t) = GlPrim mode white vertices where
  mode = case isClosed t of
    True  -> LineLoop
    False -> LineStrip
  vertices = V.fromList $ concatMap flatP2 $ trailVertices p0 t

flatP2 :: (Fractional a, Num a) => P2 -> [a]
flatP2 (unp2 -> (x,y)) = [r2f x, r2f y]

r2f :: (Real r, Fractional f) => r -> f
r2f x = realToFrac x

r2fPr :: (Real r, Fractional f) => (r,r) -> (f,f)
r2fPr (a,b) = (r2f a, r2f b)

data OpenGL = OpenGL
            deriving (Show, Typeable)

instance Backend OpenGL R2 where
  data Render OpenGL R2 = GlRen (BoundingBox R2) [GlPrim]
                        deriving Show
  type Result OpenGL R2 = IO ()
  data Options OpenGL R2 = GlOptions
                           { size :: SizeSpec2D   -- ^ The requested size.
                           }
                         deriving Show

  withStyle _ _ _ r = r

-- | The OpenGL backend expects doRender to be called in a loop.
--   Ideally, most of the work would be done on the first rendering,
--   and subsequent renderings should require very little CPU computation
  doRender _ _ = displayPrims

displayPrims :: Render OpenGL R2 -> IO ()
displayPrims (GlRen b ps) = do
    clearColor $= glColor (black :: Colour GLfloat)
    clear [ColorBuffer]
    matrixMode $= Modelview 0
    loadIdentity
    inclusiveOrtho b
    mapM_ (drawOGL 2) ps
    flush

instance Monoid (Render OpenGL R2) where
  mempty = GlRen emptyBox mempty
  (GlRen b0 r0) `mappend` (GlRen b1 r1) = GlRen (b0 <> b1) (r0 <> r1)

instance Renderable (Path R2) OpenGL where
  render _ = renderPath

instance Renderable (Trail R2) OpenGL where
  render c t = render c $ Path [(p2 (0,0), t)]

instance Renderable (Segment R2) OpenGL where
  render c = render c . flip Trail False . (:[])

dimensions :: QDiagram b R2 m -> (Double, Double)
dimensions = unr2 . boxExtents . boundingBox

aspectRatio :: QDiagram b R2 m -> Double
aspectRatio = uncurry (/) . dimensions

inclusiveOrtho :: BoundingBox R2 -> IO ()
inclusiveOrtho b = ortho x0 x1 y0 y1 z0 z1 where
  defaultBounds = (p2 (-1,-1), p2 (1,1))
  ext      = unr2 $ boxExtents b
  (ll, ur) = maybe defaultBounds id $ getCorners b
  (x0, y0) = r2fPr $ unp2 ll - 0.05 * ext
  (x1, y1) = r2fPr $ unp2 ur + 0.05 * ext
  z0 = 0
  z1 = 1
