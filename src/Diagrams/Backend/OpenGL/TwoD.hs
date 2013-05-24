{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Diagrams.Backend.OpenGL.TwoD (aspectRatio, OpenGL(..), Options(..) ) where

-- General  Haskell
import Data.Semigroup
import Control.Monad.State
import System.IO.Unsafe
import qualified Data.Vector.Storable as V
import Data.Tuple

-- Graphics
import Data.Colour.SRGB as C
import Graphics.Rendering.OpenGL as GL

-- From Diagrams
import Diagrams.Backend.OpenGL.Types
import Diagrams.Prelude as D hiding (Attribute, close, e, (<>))
import Diagrams.TwoD.Arc
import Diagrams.TwoD.Path
import Graphics.Rendering.Util


{- calculate drawn outlines of styled lines -}

calcLines :: [Double]  -- ^ Dashing pattern, first element of Dashing type
             -> Double -- ^ Line Width
             -> LineCap
             -> LineJoin
             -> [P2]  -- ^ Points from a single Trail, with curves already linearized
             -> [[P2]] -- ^ Each inner list is the outline of a (convex) polygon
calcLines darr lwf lcap lj ps@(_:_:_) =
  case darr of
    []    -> map (calcLine lwf) strokedLines <>
             map (calcJoin lj lwf) joins
    (_:_) -> calcDashedLines (cycle darr) False lwf lcap lj strokedLines
  <> if dist < 0.0001
     then [calcJoin lj lwf (pup, fp, sp)]
     else [calcCap lwf lcap $ swap $ head strokedLines] <>
          [calcCap lwf lcap $ last strokedLines]
 where strokedLines = zip  ps (tail ps)
       joins = zip3 ps (tail ps) (tail $ tail ps)
       pup   = ps !! (length ps - 2)
       lp    = last ps
       fp    = head ps
       sp    = ps !! 1
       dist = magnitude $ lp .-. fp
calcLines _ _ _ _ _ = mempty

calcDashedLines :: [Double] -- ^ Dashing pattern, first element of Dashing type
                   -> Bool -- ^ Currently in a gap between dashes
                   -> Double -- ^ Line Width
                   -> LineCap
                   -> LineJoin
                   -> [(P2, P2)] -- ^ Line segments, defined by their endpoints
                   -> [[P2]] -- ^ Each inner list is the outline of a (convex) polygon
calcDashedLines (d:ds) hole lwf lcap lj ((p0, p1):ps) =
  if hole
  then if len >= d
       then calcDashedLines ds           (not hole) lwf lcap lj ((p0 .+^ vec, p1):ps)
       else calcDashedLines (d - len:ds) (    hole) lwf lcap lj ps
  else if len >= d
       then calcLine lwf (p0, p0 .+^ vec):
            calcDashedLines ds           (not hole) lwf lcap lj ((p0 .+^ vec, p1):ps)
       else calcLine lwf (p0, p1):
            case ps of
              ((_, p3):_) -> calcJoin lj lwf (p0, p1, p3):
                             calcDashedLines (d - len:ds) hole lwf lcap lj ps
              []     -> mempty
 where len = magnitude (p1 .-. p0)
       vec = normalized (p1 .-. p0) ^* d
calcDashedLines _ _ _ _ _ _ = mempty

calcCap :: Double -- ^ Line Width
           -> LineCap
           -> (P2, P2) -- ^ Endpoints of final line segment
           -> [P2] -- ^ The outline of the line's end cap
calcCap lwf lcap (p0, p1) =
  case lcap of
    LineCapButt   -> mempty
    LineCapRound  ->
      trlVertices (p1 .+^ c, arcT (Rad $ -tau/4) (Rad $ tau/4)
                             # D.scale (r2f lwf/2) # D.rotate angle)
    LineCapSquare -> [ p1 .+^ c
                     , p1 .-^ c
                     , p1 .+^ (norm - c)
                     , p1 .+^ (norm + c)
                     ]
 where vec   = p1 .-. p0
       norm  = normalized vec ^* (lwf/2)
       c = D.rotate (-tau/4 :: Rad) norm
       angle :: Rad
       angle = direction vec

calcJoin :: LineJoin
            -> Double -- ^ Line Width
            -> (P2, P2, P2) -- ^ Two line segments meeting at the middle point
            -> [P2]
calcJoin lj lwf (p0, p1, p3) =
  case lj of
    LineJoinMiter -> if abs spikeLength > 10 * lwf
                       then bevel
                       else spike
    LineJoinRound -> (p1:) $ case side of
      1 -> trlVertices (p1 .+^ v1, arc' (lwf/2) (direction v1 :: Rad) (direction v2))
      _ -> trlVertices (p1 .+^ v2, arc' (lwf/2) (direction v2 :: Rad) (direction v1))
    LineJoinBevel -> bevel
 where norm1       = normalized (p1 .-. p0) ^* (lwf/2)
       norm2       = normalized (p3 .-. p1) ^* (lwf/2)
       side        = if detV norm1 norm2 > 0
                       then  1
                       else -1
       v1 :: R2
       v1          = D.rotate (side * (-tau/4)::Rad) norm1
       v2 :: R2
       v2          = D.rotate (side * (-tau/4)::Rad) norm2
       bevel       = [ p1 .+^ v1
                     , p1 .+^ v2
                     , p1
                     ]
       spikeAngle  = (direction v1 - direction v2) / 2
       spikeLength = (lwf/2) / cos (getRad spikeAngle)
       v3 :: R2
       v3          = D.rotate (direction v1 - spikeAngle) unitX ^* spikeLength
       spike       = [ p1 .+^ v1
                     , p1 .+^ v3
                     , p1 .+^ v2
                     , p1
                     ]
       -- | The determinant of two vectors.
       detV :: R2 -> R2 -> Double
       detV (unr2 -> (x1,y1)) (unr2 -> (x2,y2)) = x1 * y2 - y1 * x2


calcLine :: Double -> (P2, P2) -> [P2]
calcLine lwf (p0, p1) =
  [ p0 .-^ c
  , p0
  , p0 .+^ c
  , p1 .+^ c
  , p1
  , p1 .-^ c
  ]
 where vec   = p1 .-. p0
       norm  = normalized vec ^* (lwf/2)
       c = D.rotate (-tau/4 :: Rad) norm

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

renderPolygon :: AlphaColour Double -> Double -> [P2] -> GlPrim P2
renderPolygon c o ps = GlPrim TriangleFan (dissolve o c) ps

trlVertices :: (P2, Trail R2) -> [P2]
trlVertices (p0, t) =
  vertices <> if isClosed t && (magnitude (p0 .-. lp) > 0.0001)
              then [p0]
              else mempty
  where vertices = concat $ zipWith segVertices
                   (trailVertices p0 t) (trailSegments t ++ [straight (0 & 0)])
        lp = last $ trailVertices p0 t

segVertices :: P2 -> Segment R2 -> [P2]
segVertices p (D.Linear _) = [p]
segVertices p cubic = map ((p .+^) . atParam cubic) [0,i..1-i] where
  i = 1/30

tessRegion :: TessWinding -> [[P2]] -> [[P2]]
tessRegion fr trs = renderTriangulation $ unsafePerformIO $
  GL.triangulate fr 0.0001 (Normal3 0 0 0)
    (\_ _ -> 0) $
    ComplexPolygon [ComplexContour (map createVertex trail) | trail <- trs]
 where createVertex (unp2 -> (x,y)) =
          AnnotatedVertex (Vertex3 (r2f x) (r2f y) 0) (0::Int)
       renderTriangulation (Triangulation ts) = map renderTriangle ts
       renderTriangle (Triangle a b c) = map deAnnotate [a, b, c]
       deAnnotate (AnnotatedVertex (Vertex3 x y _) _) = p2 (r2f x, r2f y)

data GLRenderState =
  GLRenderState{ currentLineColor  :: AlphaColour Double
               , currentFillColor  :: AlphaColour Double
               , currentOpacity    :: Double
               , currentLineWidth  :: Double
               , currentLineCap    :: LineCap
               , currentLineJoin   :: LineJoin
               , currentFillRule   :: TessWinding
               , currentDashArray  :: [Double]
               , currentDashOffset :: Double
               , currentClip       :: [[P2]]
               }

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

type GLRenderM a = State GLRenderState a

instance Backend OpenGL R2 where
  data Render OpenGL R2 = GlRen (BoundingBox R2) (GLRenderM [GlPrim P2])
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

-- | The OpenGL backend expects doRender to be called in a loop.
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
    mapM_ draw2 ps
    flush

instance Monoid (Render OpenGL R2) where
  mempty = GlRen emptyBox $ return mempty
  (GlRen b1 p01) `mappend` (GlRen b2 p02) =
    GlRen (b1 <> b2) $ liftA2 (<>) p01 p02

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

{- Style changes -}

changeLineColor :: Style v -> GLRenderM ()
changeLineColor s =
  case lcol of
    Just (r, g, b, a) ->
      modify $ \st -> st{currentLineColor = withOpacity (sRGB r g b) a}
    Nothing           -> return ()
 where lcol = colorToSRGBA <$> getLineColor <$> getAttr s

changeFillColor :: Style v -> GLRenderM ()
changeFillColor s =
  case fcol of
    Just (r, g, b, a) ->
      modify $ \st -> st{currentFillColor = withOpacity (sRGB r g b) a}
    Nothing           -> return ()
 where fcol = colorToSRGBA <$> getFillColor <$> getAttr s

changeOpacity :: Style v -> GLRenderM ()
changeOpacity s =
  case op of
    Just o -> modify $ \st -> st{currentOpacity = o}
    Nothing           -> return ()
 where op =  getOpacity <$> getAttr s

changeLineWidth :: Style v -> GLRenderM ()
changeLineWidth s =
  case lwid of
    Just a  -> modify $ \st -> st{currentLineWidth = realToFrac a}
    Nothing -> return ()
 where lwid = getLineWidth <$> getAttr s

changeLineCap :: Style v -> GLRenderM ()
changeLineCap s =
  case lcap of
    Just a  -> modify $ \st -> st{currentLineCap = a}
    Nothing -> return ()
 where lcap = getLineCap <$> getAttr s

changeLineJoin:: Style v -> GLRenderM ()
changeLineJoin s =
  case lj of
    Just a  -> modify $ \st -> st{currentLineJoin = a}
    Nothing -> return ()
 where lj = getLineJoin <$> getAttr s

changeFillRule :: Style v -> GLRenderM ()
changeFillRule s =
  case fr of
    Just Winding -> modify $ \st -> st{currentFillRule = TessWindingNonzero}
    Just EvenOdd -> modify $ \st -> st{currentFillRule = TessWindingOdd}
    Nothing      -> return ()
 where fr = getFillRule <$> getAttr s

changeDashing :: Style v -> GLRenderM ()
changeDashing s =
  case dash of
    Just (Dashing a o) ->
      modify $ \st ->
      st{ currentDashArray  = map realToFrac a
        , currentDashOffset = realToFrac o
        }
    Nothing      -> return ()
 where dash = getDashing <$> getAttr s

changeClip :: Style v -> GLRenderM ()
changeClip s =
  case clip of
    Just (Path trs:_) ->
      modify $ \st ->
      st{ currentClip = tessRegion TessWindingNonzero $
                        map trlVertices trs
        }
    Just _       -> return ()
    Nothing      -> return ()
 where clip = getClip <$> getAttr s
