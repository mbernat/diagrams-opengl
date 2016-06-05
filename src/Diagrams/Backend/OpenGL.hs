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
import           Data.Foldable (foldMap)
import Data.Tree
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
import           Diagrams.Core.Compile

-- In this package
import           Graphics.Rendering.Util
import           Diagrams.Backend.OpenGL.TwoD.Attributes
import           Diagrams.Backend.OpenGL.TwoD.Outlines
import           Diagrams.Backend.OpenGL.TwoD.Tesselate

renderPath :: Path V2 Double -> Render OpenGL V2 Double
renderPath p@(Path trs) =
  GlRen $ do
    _fc <- gets _currentFillColor
    _lc <- gets _currentLineColor
    _o <- gets _currentOpacity
    _fr <- gets _currentFillRule
    _lw <- gets _currentLineWidth
    _lcap <- gets _currentLineCap
    _lj <- gets _currentLineJoin
    _dash <- gets _currentDashing
    _clip <- gets _currentClip
    let
        -- trail to vertices
        trails                  = map trlVertices trs
        -- pick convex polygons for the fill
        simplePolygons          = tessRegion _fr trails

        -- convex polygons for the line
        linePolygons :: [Convex]
        linePolygons = concatMap (calcLines _dash _lw _lcap _lj) trails

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

flatP2 :: (Fractional a, Num a) => P2 Double -> [a]
flatP2 (unp2 -> (x,y)) = [r2f x, r2f y]

data OpenGL = OpenGL
            deriving (Show, Typeable)

renderRTree' :: RTree OpenGL V2 Double a -> Render OpenGL V2 Double
renderRTree' (Node (RPrim accTr p) _) = render OpenGL $ transform accTr p
renderRTree' (Node (RStyle sty) ts) = GlRen $ do
    let (GlRen sm) = foldMap renderRTree ts
    withStyleState sty sm
renderRTree' (Node _ ts) = foldMap renderRTree ts

instance Backend OpenGL V2 Double where
  data Render OpenGL V2 Double = GlRen (GLRenderM GlPrim)
  type Result OpenGL V2 Double = IO (GLFW.Window -> IO ())
  data Options OpenGL V2 Double = GlOptions
                           { bgColor :: AlphaColour Double -- ^ The clear color for the window
                           , globalSize :: BoundingBox V2 Double  -- ^ Size of the Diagram
                           }
                         deriving Show

  renderRTree = renderRTree'

  -- The backend stores the size of the Diagram in the options record
  adjustDia _ o d = (o { globalSize = boundingBox d }, d)

{-  
--   The OpenGL backend expects doRender to be called in a loop.
-- initResources loads all the data to the GPU
-- and draw is an IO action which redraws the screen
  doRender _ o (GlRen p) = do
    -- Boring OpenGL init
    GL.blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    -- collect all the prims into GPU buffers
    let ps = evalState p initialGLRenderState
    resources <- initResources (bgColor o) (inclusiveOrtho (globalSize o)) ps
    -- return an action which will redraw
    return $ draw resources
-}

instance Monoid (Render OpenGL V2 Double) where
  mempty = GlRen $ return mempty
  (GlRen p01) `mappend` (GlRen p02) = GlRen $ liftA2 (<>) p01 p02

instance Renderable (Path V2 Double) OpenGL where
  render _ = renderPath

instance Renderable (Trail V2 Double) OpenGL where
  render c t = render c $ Path [(t `at` origin)]

instance Renderable (Segment Closed V2 Double) OpenGL where
  render c = render c . trailFromSegments . (:[])

aspectRatio :: Monoid' m => QDiagram b V2 Double m -> Double
aspectRatio = const 1
-- TODO
--aspectRatio = uncurry (/) . size2D

inclusiveOrtho :: BoundingBox V2 Double -> Size -> FieldRec '[MVP]
inclusiveOrtho b (Size w h) = mvp =: L.mkTransformationMat scl trns where
  defaultBounds = (p2 (-1,-1), p2 (1,1))
  (ll, ur) = maybe defaultBounds id $ getCorners b
  trns = p2ToV3 (centroid [ll, ur]) L.^* (-1)
  ext = ur .-. ll
  scl = diagonalMatrix $ aspectScale  / r2ToV3 ext
  aspect = fi w / fi h
  aspectScale = L.V3 (2 / max 1 aspect) (2 / max 1 (1/aspect)) 1

-- | Renders the diagram into a framebuffer texture.
diagramToTexture :: Size -> Options OpenGL V2 Double -> QDiagram OpenGL V2 Double Any -> IO TextureObject
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
        (PixelData RGBA UnsignedByte nullPtr)
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

renderDiagram :: (Semigroup m, Monoid m) => Size -> OpenGL -> Options OpenGL V2 Double -> QDiagram OpenGL V2 Double m -> IO ()
renderDiagram s b opts d = do
    let (o, d')   = adjustDia b opts d
        (GlRen p) = renderData b d'
    -- collect all the prims into GPU buffers
    let ps = evalState p initialGLRenderState
    resources <- initResources transparent (inclusiveOrtho (globalSize o)) ps
    -- Draw the diagram into the currently setup and bound context
    draw' resources s
    unknitResources resources
  where
    renderData _ = renderRTree . toRTree
