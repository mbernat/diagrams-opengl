module Diagrams.Backend.OpenGL.CmdLine where

import Graphics.UI.GLUT

import Diagrams.Prelude
import Diagrams.Backend.OpenGL

defaultMain :: Diagram OpenGL R2 -> IO ()
defaultMain d = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Diagrams"
  displayCallback $= (renderDia OpenGL (GlOptions Absolute) d)
  reshapeCallback $= (Just $ preserveAspect d)
  clientState VertexArray $= Enabled
  mainLoop

preserveAspect :: Diagram OpenGL R2 -> Size -> IO ()
preserveAspect d (Size w h) = viewport $= (Position 0 0, Size w' h') where
  a = aspectRatio d
  w' = floor $ min (r2f w) (r2f h * a)
  h' = floor $ min (r2f h) (r2f w / a)
