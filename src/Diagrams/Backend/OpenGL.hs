{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Diagrams.Backend.OpenGL (aspectRatio, OpenGL(..), Options(..), diagramToTexture) where

-- General  Haskell
import           Control.Monad.State
import           Data.Typeable

-- Graphics
import           Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           Data.Vinyl
import qualified Linear as L
import           Foreign.Ptr
import           System.Exit

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
  GlRen (boundingBox p) $ do
    _fc <- gets currentFillColor
    _lc <- gets currentLineColor
    _o <- gets currentOpacity
    _fr <- gets currentFillRule
    _lw <- gets currentLineWidth
    _lcap <- gets currentLineCap
    _lj <- gets currentLineJoin
    _darr <- gets currentDashArray
    _clip <- gets currentClip
    put initialGLRenderState
    let
        trails                  = map trlVertices trs
        simplePolygons          = tessRegion _fr trails

        linePolygons :: [Convex]
        linePolygons = concatMap (calcLines _darr _lw _lcap _lj) trails

        clippedPolygons :: [Convex] -> [Convex]
        clippedPolygons vis
            | null _clip = vis
            | otherwise =
                concatMap (tessRegion TessWindingAbsGeqTwo . (: map unConvex _clip) . unConvex) vis
        in
     return . mconcat $
       map (renderPolygon $ dissolve _o _fc) (clippedPolygons simplePolygons) ++
       map (renderPolygon $ dissolve _o _lc) (clippedPolygons linePolygons)

renderPolygon :: AlphaColour Double -> Convex -> GlPrim
renderPolygon c (Convex ps) = GlPrim (zipRecs vertices colors) elements
  where
    vertices = map (coord2d =:) . map p2ToV2 $ ps
    colors = repeat $ vColor =: (v4Color c)
    lastElement = fromIntegral (length vertices) - 2
    elements = concat [[0, i, i+1] | i <- [1..lastElement]]

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
  data Render OpenGL R2 = GlRen (BoundingBox R2) (GLRenderM GlPrim)
  type Result OpenGL R2 = IO (GLFW.Window -> IO ())
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
    -- Boring OpenGL init
    clearColor $= glColor (bgColor o)
    GL.blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    -- collect all the prims into GPU buffers
    let ps = evalState p initialGLRenderState
    resources <- initResources ps
    -- return an action which will redraw
    return (draw (inclusiveOrtho b) resources)

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

inclusiveOrtho :: BoundingBox R2 -> Int -> Int -> PlainRec '[MVP]
inclusiveOrtho b w h = mvp =: L.mkTransformationMat scl trns where
  defaultBounds = (p2 (-1,-1), p2 (1,1))
  (ll, ur) = maybe defaultBounds id $ getCorners b
  trns = p2ToV3 (centroid [ll, ur]) L.^* (-1)
  ext = ur .-. ll
  scl = diagonalMatrix $ aspectScale  / r2ToV3 ext
  aspect = fi w / fi h
  aspectScale = L.V3 (2 / max 1 aspect) (2 / max 1 (1/aspect)) 1

-- | Renders the diagram into a framebuffer texture.
diagramToTexture :: (Monoid m, Semigroup m) => Size -> Options OpenGL R2 -> QDiagram OpenGL R2 m -> IO TextureObject
diagramToTexture s@(Size w h) opts d = do
    -- Generate a framebuffer object
    fb <- genObjectName
    bindFramebuffer Framebuffer $= fb
    -- Generate a texture to attach to the framebuffer.
    tex <- genObjectName
    textureBinding Texture2D $= Just tex
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texImage2D
        Texture2D
        NoProxy
        0
        RGBA'
        (TextureSize2D w h)
        0
        (PixelData RGB UnsignedByte nullPtr)
    -- Attach!
    framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D tex 0
    -- Check for failure.
    status <- GL.get $ framebufferStatus Framebuffer
    unless (status == Complete) $ do
        print status
        exitFailure

    renderDiagram s OpenGL opts d

    -- Unbind and cleanup.
    bindFramebuffer Framebuffer $= defaultFramebufferObject
    deleteObjectName fb
    return tex

renderDiagram :: (Semigroup m, Monoid m) => Size -> OpenGL -> Options OpenGL R2 -> QDiagram OpenGL R2 m -> IO ()
renderDiagram s b opts d = do
    let (_, d')   = adjustDia b opts d
        (GlRen box p) = renderData b d'
    -- collect all the prims into GPU buffers
    let ps = evalState p initialGLRenderState
    resources <- initResources ps
    -- Draw the diagram into the currently setup and bound context
    draw' (inclusiveOrtho box) resources s
    unknitResources resources

