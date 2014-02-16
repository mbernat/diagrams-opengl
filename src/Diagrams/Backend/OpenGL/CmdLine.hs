{-# LANGUAGE FlexibleContexts #-}

module Diagrams.Backend.OpenGL.CmdLine where

import Data.Colour.Names
import Data.Monoid.Split

import Diagrams.Prelude
import Diagrams.Backend.OpenGL

import Graphics.Rendering.Util (r2f)
import Graphics.Rendering.Util.GLFW

defaultMain :: Diagram OpenGL R2 -> IO ()
defaultMain d = do
  win <- initialize "Diagrams"
  drawA <- renderDia OpenGL defaultOptions d
  mainLoop (drawA win) win
  cleanup win

defaultOptions :: Options OpenGL R2
defaultOptions = GlOptions (opaque white) mempty


