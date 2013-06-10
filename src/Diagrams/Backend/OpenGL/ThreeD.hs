{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Diagrams.Backend.OpenGL.ThreeD
       (OpenGL(..),
        Options(..),
        ProjectionType(..),
        Viewpoint(..),
       ) where

--import Control.Lens
import Data.List

import Graphics.Rendering.OpenGL as GL
import qualified Data.Vector.Storable as V
import Data.Colour.SRGB

import Diagrams.Prelude
import Diagrams.ThreeD.Shapes
import Diagrams.ThreeD.Types
import Math.Spline.NurbsSurface

import Graphics.Rendering.Util.ThreeD  -- local module, not exposed
import Graphics.Rendering.Util         -- local module, not exposed
import Diagrams.Backend.OpenGL.Types

instance Monoid (Render OpenGL R3) where
  mempty = GlRen []
  (GlRen a) `mappend` (GlRen b) = GlRen (a <> b)

data ProjectionType = Ortho | Frustum
                    deriving Show

data Viewpoint = Viewpoint ProjectionType
                  Double Double Double Double Double Double
               deriving Show

setViewpoint :: Viewpoint -> IO ()
setViewpoint (Viewpoint t x0 x1 y0 y1 z0 z1) = do
  matrixMode $= Projection
  loadIdentity
  (set t) (r2f x0) (r2f x1) (r2f y0) (r2f y1) (r2f z0) (r2f z1) where
    set Ortho = ortho
    set Frustum = frustum

instance Backend OpenGL R3 where
  data Render OpenGL R3 = GlRen [GlPrim3]
  type Result OpenGL R3 = IO ()
  data Options OpenGL R3 = GlOptions {
    bgColor :: AlphaColour Double, -- ^ The clear color for the window
    viewpoint :: Viewpoint        -- ^ Defines the viewpoint and field of view
    } deriving Show

  withStyle _ s _ (GlRen ps) =
    GlRen $ map (setColor s) ps

  doRender _ o (GlRen p) = do
    clearColor $= glColor (bgColor o)
    clear [ColorBuffer]
    matrixMode $= Modelview 0
    loadIdentity
    setViewpoint $ viewpoint o
    GL.blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    mapM_ draw3 p
    flush

instance Renderable Ellipsoid OpenGL where
  render _ (Ellipsoid t) =
    GlRen [GlPrim3 TriangleStrip defaultColor pts normals] where
      pts = (map (papply t) $ polarSphere 30 30)
      normals = map (r3 . norm . unp3) pts
      norm v = v ^/ magnitude v

instance Renderable NurbsSurface OpenGL where
  render _ n =
    GlRen $ map (\pts -> GlPrim3 TriangleStrip defaultColor pts (normal pts))
              (triangulateQuads . (map . map) p3 . surfaceGrid n 30 $ 30) where
      normal v = map (r3 . norm . unZ . unp3) v
      unZ (x,y,_) = (x,y,0)
      norm v = v ^/ magnitude v

-- Each inner list of points is suitable for use by TriangleStrip
-- Each quad in the input mesh becomes two triangles;
triangulateQuads :: [[a]] -> [[a]]
triangulateQuads rs = (map interleave) pairs where
  pairs = zip (init rs) (tail rs)
  flat2 (a, b) = [a, b]
  interleave = concatMap flat2 . uncurry zip

polarSphere :: Int -> Int -> [P3]
polarSphere nLog nLat = map cartesian tris where
  cartesian (theta, phi) = p3 (cos theta * cos phi, sin theta * cos phi, sin phi)
  -- coördinates at which to evaluate
  dp = pi/(r2f nLat)
  phis' = [-pi/2, -pi/2+dp .. pi/2]
  phis = zip (init phis') (tail phis')
  dt = 2*pi/(r2f nLog)
  thetas = [0, dt .. 2*pi]
  -- more list utility functions
  interleave = concat . transpose
  -- triangulate a cell
  cell :: (Double, Double) -> Double -> [(Double, Double)]
  cell (p0, p1) t = [(t, p0), (t, p1)]
  -- put it all together
  tris :: [(Double, Double)]
  tris =concat $ cell <$> phis <*> thetas

-- icosahedron with unit radius, centered at origin
icosahedron :: [P3]
icosahedron = map (ps !!) $ tris where
    ps = map p3 [
      ( 0, s, l),
      ( 0,-s, l),
      ( 0, s,-l),
      ( 0,-s,-l),
      ( l, 0, s),
      ( l, 0,-s),
      (-l, 0, s),
      (-l, 0,-s),
      ( s, l, 0),
      (-s, l, 0),
      ( s,-l, 0),
      (-s,-l, 0)
      ]
         -- upper pole 0
         -- upper ring 1,4,8,9,6
         -- lower ring 10,5,2,7,11
         -- lower pole 3
    tris = [0,1,4,0,4,8,0,8,9,0,9,6,0,6,1,
            1,10,4,4,5,8,8,2,9,9,7,6,6,11,1,
            10,4,5,5,8,2,2,9,7,7,6,11,11,1,10,
            3,10,5,3,5,2,3,2,7,3,7,11,3,11,10]
    p = (1+sqrt(5)/2)
    l = p/(sqrt (1+p^2))
    s = 1/(sqrt (1+p^2))

setColor :: Style v -> GlPrim3 -> GlPrim3
setColor s p = case colorToSRGBA <$> getFillColor <$> getAttr s of
  Just (r, g, b, a) -> p {primColor = withOpacity (sRGB r g b) a}
  Nothing           -> p


defaultColor = opaque green
