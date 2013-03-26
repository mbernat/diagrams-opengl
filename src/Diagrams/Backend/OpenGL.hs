{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Diagrams.Backend.OpenGL where

-- General Haskell
import Data.Semigroup
import           Data.Typeable
import qualified Data.Vector.Storable as V

-- Graphics
import Data.Colour.SRGB
import Graphics.Rendering.OpenGL hiding (RGB, Linear, Render)

-- From Diagrams
import Diagrams.Prelude hiding (Attribute, close, e, (<>))
import Graphics.Rendering.Util

renderPath :: Path R2 -> Render OpenGL R2
renderPath p@(Path trs) = GlRen box $ concatMap renderTrail trs where
  box = boundingBox p

renderTrail :: (P2, Trail R2) -> [GlPrim]
renderTrail (p0, t) = case isClosed t of
  True  -> [GlPrim LineStrip defaultLineColor vertices,
            GlPrim TriangleFan defaultFillColor vertices]
  False -> [GlPrim LineStrip defaultLineColor vertices]
  where
    vertices = V.fromList $ concatMap flatP2 . concat $
               zipWith segVertices (trailVertices p0 t)
                                   (trailSegments t ++ [straight (r2 (0,0))])

segVertices :: P2 -> Segment R2 -> [P2]
segVertices p (Linear _) = [p]
segVertices p cubic = map ((p .+^) . atParam cubic) [0,i..1-i] where
  i = 1/30

flatP2 :: (Fractional a, Num a) => P2 -> [a]
flatP2 (unp2 -> (x,y)) = [r2f x, r2f y]

styleLine :: forall v. Style v -> GlPrim -> GlPrim
styleLine s p = case colorToSRGBA <$> getLineColor <$> getAttr s of
                     Just (r,g,b,a) -> p { primColor = withOpacity (sRGB r g b) a }
                     Nothing        -> p

styleFill :: forall v. Style v -> GlPrim -> GlPrim
styleFill s p = case colorToSRGBA <$> getFillColor <$> getAttr s of
                     Just (r,g,b,a) -> p { primColor = withOpacity (sRGB r g b) a }
                     Nothing        -> p

stylePrim :: forall v. Style v -> GlPrim -> GlPrim
stylePrim s p@(GlPrim Lines _ _) = styleLine s p
stylePrim s p@(GlPrim LineLoop  _ _) = styleLine s p
stylePrim s p@(GlPrim LineStrip _ _) = styleLine s p
stylePrim s p@(GlPrim Triangles _ _) = styleFill s p
stylePrim s p@(GlPrim TriangleStrip _ _) = styleFill s p
stylePrim s p@(GlPrim TriangleFan _ _) = styleFill s p
stylePrim s p@(GlPrim Quads _ _) = styleFill s p
stylePrim s p@(GlPrim QuadStrip _ _) = styleFill s p
stylePrim s p@(GlPrim Polygon _ _) = styleFill s p
stylePrim _ p = p

data OpenGL = OpenGL
            deriving (Show, Typeable)

instance Backend OpenGL R2 where
  data Render OpenGL R2 = GlRen (BoundingBox R2) [GlPrim]
                        deriving Show
  type Result OpenGL R2 = IO ()
  data Options OpenGL R2 = GlOptions
                           { bgColor :: AlphaColour Double -- ^ The clear color for the window
                           }
                         deriving Show

  withStyle _ s _ (GlRen b ps) = GlRen b $ map (stylePrim s) ps

-- | The OpenGL backend expects doRender to be called in a loop.
--   Ideally, most of the work would be done on the first rendering,
--   and subsequent renderings should require very little CPU computation
  doRender _ o (GlRen b ps) = do
    clearColor $= (glColor $ bgColor o)
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
