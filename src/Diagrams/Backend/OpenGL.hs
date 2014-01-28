{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Diagrams.Backend.OpenGL (aspectRatio, OpenGL(..), Options(..) ) where

-- General  Haskell
import           Control.Monad.State
import           Data.Typeable
import qualified Data.Vector.Storable as V

-- Graphics
import           Graphics.Rendering.OpenGL as GL

-- From Diagrams
import           Diagrams.BoundingBox
import           Diagrams.Prelude as D hiding (Attribute)

-- In this package
import           Graphics.Rendering.Util
import           Diagrams.Backend.OpenGL.TwoD.Attributes
import           Diagrams.Backend.OpenGL.TwoD.Outlines
import           Diagrams.Backend.OpenGL.TwoD.Tesselate

renderPath :: Path R2 -> Render OpenGL R2
renderPath p@(Path trs) =
  GlRen box $ do
    _fc <- gets currentFillColor
    _lc <- gets currentLineColor
    o <- gets currentOpacity
    fr <- gets currentFillRule
    _lw <- gets currentLineWidth
    lcap <- gets currentLineCap
    lj <- gets currentLineJoin
    darr <- gets currentDashArray
    clip <- gets currentClip
    put initialGLRenderState
    return $
      map (renderPolygon _fc o) (clippedPolygons (simplePolygons fr) clip) <>
      map (renderPolygon _lc o) (clippedPolygons (linePolygons darr _lw lcap lj) clip)
 where trails                  = map trlVertices trs
       simplePolygons fr       = tessRegion fr trails

       linePolygons :: [Double] -> Double -> LineCap -> LineJoin -> [[P2]]
       linePolygons darr _lw lcap lj = concatMap (calcLines darr _lw lcap lj) trails

       clippedPolygons vis [] = vis
       clippedPolygons vis clip = concatMap (tessRegion TessWindingAbsGeqTwo . (: clip)) vis
       box = boundingBox p

renderPolygon :: AlphaColour Double -> Double -> [P2] -> GlPrim
renderPolygon c o ps = GlPrim TriangleFan (dissolve o c) vertices
  where vertices = V.fromList $ concatMap flatP2 ps

flatP2 :: (Fractional a, Num a) => P2 -> [a]
flatP2 (unp2 -> (x,y)) = [r2f x, r2f y]

data OpenGL = OpenGL
            deriving (Show, Typeable)

initialGLRenderState :: GLRenderState
initialGLRenderState = GLRenderState
                            (opaque black)
                            transparent
                            1
                            0.01
                            LineCapButt
                            LineJoinMiter
                            TessWindingNonzero
                            []
                            0
                            []

instance Backend OpenGL R2 where
  data Render OpenGL R2 = GlRen (BoundingBox R2) (GLRenderM [GlPrim])
  type Result OpenGL R2 = IO ()
  data Options OpenGL R2 = GlOptions
                           { bgColor :: AlphaColour Double -- ^ The clear color for the window
                           }
                         deriving Show

  withStyle _ s _ (GlRen b p) =
      GlRen b $ do
        mapM_ ($ s)
          [ changeLineColor
          , changeFillColor
          , changeOpacity
          , changeLineWidth
          , changeLineCap
          , changeLineJoin
          , changeFillRule
          , changeDashing
          , changeClip
          ]
        p

--   The OpenGL backend expects doRender to be called in a loop.
--   Ideally, most of the work would be done on the first rendering,
--   and subsequent renderings should require very little CPU computation
  doRender _ o (GlRen b p) = do
    clearColor $= glColor (bgColor o)
    clear [ColorBuffer]
    matrixMode $= Modelview 0
    loadIdentity
    inclusiveOrtho b
    let ps = evalState p initialGLRenderState
    -- GL.polygonMode $= (GL.Line, GL.Line)
    GL.blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    mapM_ (drawOGL 2) ps
    flush

instance Monoid (Render OpenGL R2) where
  mempty = GlRen emptyBox $ return mempty
  (GlRen b1 p01) `mappend` (GlRen b2 p02) =
    GlRen (b1 <> b2) $ liftA2 (<>) p01 p02

instance Renderable (Path R2) OpenGL where
  render _ = renderPath

instance Renderable (Trail R2) OpenGL where
  render c t = render c $ Path [(t `at` origin)]

instance Renderable (Segment Closed R2) OpenGL where
  render c = render c . trailFromSegments . (:[])

dimensions :: Monoid' m => QDiagram b R2 m -> (Double, Double)
dimensions = unr2 . boxExtents . boundingBox

aspectRatio :: Monoid' m => QDiagram b R2 m -> Double
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
