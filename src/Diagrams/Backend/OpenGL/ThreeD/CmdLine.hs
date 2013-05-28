{-# LANGUAGE FlexibleContexts #-}

module Diagrams.Backend.OpenGL.ThreeD.CmdLine where

import Data.Colour.Names
import Data.Monoid.Split
import Graphics.UI.GLUT

import Diagrams.Prelude
import Diagrams.Backend.OpenGL.ThreeD
import Diagrams.ThreeD.Types

import Graphics.Rendering.Util
import Graphics.Rendering.OpenGL as GL

defaultMain :: Diagram OpenGL R3 -> IO ()
defaultMain d = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [WithSamplesPerPixel 16,WithDepthBuffer,WithDepthBuffer,RGBAMode,WithAlphaComponent]
  _ <- createWindow "Diagrams"
  lighting $= Enabled
  colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
  -- lightModelAmbient $= Color4 0.4 0.4 0.4 1
  let l = Light 0
  light       l $= Enabled
  GL.position l $= Vertex4 1 0 0 0
  GL.diffuse  l $= glColor (opaque white)
  GL.specular l $= glColor (opaque white)
  GL.ambient  l $= Color4 0.3 0.3 0.3 1
  materialAmbient FrontAndBack $= glColor (opaque blue)
  displayCallback $= (renderDia OpenGL defaultOptions d)
  clientState VertexArray $= Enabled
  clientState NormalArray $= Enabled
  mainLoop

defaultOptions :: Options OpenGL R3
defaultOptions = GlOptions (opaque black) (Viewpoint Ortho (-1) 1 (-1) 1 0 1)
