{-# LANGUAGE FlexibleContexts #-}

module Diagrams.Backend.OpenGL.TwoD.CmdLine where

import Data.Colour.Names
import Data.Monoid.Split
import Graphics.UI.GLUT

import Diagrams.Prelude
import Diagrams.Backend.OpenGL.TwoD

import Graphics.Rendering.Util (r2f)

defaultMain :: Diagram OpenGL R2 -> IO ()
defaultMain d = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [WithSamplesPerPixel 16,WithDepthBuffer,WithDepthBuffer,RGBAMode,WithAlphaComponent]
  _ <- createWindow "Diagrams"
  displayCallback $= (renderDia OpenGL defaultOptions d)
  reshapeCallback $= (Just $ preserveAspect d)
  clientState VertexArray $= Enabled
  mainLoop

intermediateRep :: Monoid' m
            => Options OpenGL R2 -> QDiagram OpenGL R2 m -> Render OpenGL R2
intermediateRep opts d  = mconcat . map renderOne . prims $ d'
      where (_, d') = adjustDia OpenGL opts d
            renderOne :: (Prim OpenGL R2, (Split (Transformation R2), Style R2))
                      -> Render OpenGL R2
            renderOne (p, (M t,      s))
              = withStyle OpenGL s mempty (render OpenGL (transform t p))

            renderOne (p, (t1 :| t2, s))
              = withStyle OpenGL s t1 (render OpenGL (transform (t1 <> t2) p))


preserveAspect :: Diagram OpenGL R2 -> Size -> IO ()
preserveAspect d (Size w h) = viewport $= (Position 0 0, Size w' h') where
  a = aspectRatio d
  w' = floor $ min (r2f w) (r2f h * a)
  h' = floor $ min (r2f h) (r2f w / a)

defaultOptions :: Options OpenGL R2
defaultOptions = GlOptions (opaque white)
