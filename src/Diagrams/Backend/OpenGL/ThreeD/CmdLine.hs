{-# LANGUAGE FlexibleContexts #-}

module Diagrams.Backend.OpenGL.ThreeD.CmdLine where

import Data.Colour.Names
import Graphics.UI.GLUT

import Diagrams.Prelude
import Diagrams.Backend.OpenGL.ThreeD
import Diagrams.ThreeD.Types

import Graphics.Rendering.Util
import Graphics.Rendering.OpenGL as GL

defaultMain :: Diagram OpenGL R3 -> IO ()
defaultMain = mainWith defaultOptions

mainWith :: Options OpenGL R3 -> Diagram OpenGL R3 -> IO ()
mainWith o d = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [WithSamplesPerPixel 16,
                         WithDepthBuffer,
                         RGBAMode,
                         WithAlphaComponent]
  _ <- createWindow "Diagrams"
  lighting $= Enabled
  colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
  -- lightModelAmbient $= Color4 0.4 0.4 0.4 1
  let l = Light 0
  light       l $= Enabled
  GL.position l $= Vertex4 100 (-1) 0 0
  GL.diffuse  l $= glColor (opaque white)
  GL.specular l $= glColor (opaque white)
  GL.ambient  l $= Color4 0.3 0.3 0.3 1
  materialAmbient FrontAndBack $= glColor (opaque blue)
  displayCallback $= (renderDia OpenGL o d)
  clientState VertexArray $= Enabled
  clientState NormalArray $= Enabled
  mainLoop

defaultOptions :: Options OpenGL R3
defaultOptions = GlOptions
                 (opaque black)
                 (2 *. isometric)
                 origin
                 (2 *^ plusZOrient isometric)
                 (FOV Ortho (-1) 1 (-1) 1 1 10)

-- functions for standard camera views

-- | A camera position giving an isometric view, in the positive octant
isometric :: P3
isometric = p3 (s33,s33,s33) where
  s33 = 1 / sqrt 3

-- | plusZOrient p calculates the camera-up vector
--   with maximum z-value, for a camera at p pointed at the origin.
plusZOrient :: P3 -> R3
plusZOrient p = r3 (-sin φ * cos θ, sin θ * sin φ, cos φ) where
  (x,y,z) = unp3 p
  φ = asin z
  θ = atan $ y / x
