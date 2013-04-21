{-# LANGUAGE FlexibleContexts #-}

module Diagrams.Backend.OpenGL.CmdLine where

import Data.Monoid.Split
import Graphics.UI.GLUT

import Diagrams.Prelude
import Diagrams.Backend.OpenGL

defaultMain :: Diagram OpenGL R2 -> IO ()
defaultMain d = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [WithSamplesPerPixel 16,WithDepthBuffer,WithDepthBuffer,RGBAMode,WithAlphaComponent]
  _ <- createWindow "Diagrams"
  displayCallback $= (renderDia OpenGL defaultOptions d)
  reshapeCallback $= (Just $ preserveAspectD d)
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
