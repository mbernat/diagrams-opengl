{-# LANGUAGE FlexibleContexts #-}

module Diagrams.Backend.OpenGL.CmdLine where

import Data.Colour.Names

import Diagrams.Prelude
import Diagrams.Backend.OpenGL

import Graphics.Rendering.Util.GLFW

defaultMain :: QDiagram OpenGL V2 Double Any -> IO ()
defaultMain d = do
  win <- initialize "Diagrams"
  drawA <- renderDia OpenGL defaultOptions d
  mainLoop (drawA win) win
  cleanup win

defaultOptions :: Options OpenGL V2 Double
defaultOptions = GlOptions (opaque white) mempty


