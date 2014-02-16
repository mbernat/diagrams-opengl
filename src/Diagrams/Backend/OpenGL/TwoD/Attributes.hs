{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TemplateHaskell            #-}

module Diagrams.Backend.OpenGL.TwoD.Attributes
       ( GLRenderM, GLRenderState (..), withStyleState )
       where

-- General  Haskell
import           Control.Monad.State
import           Control.Lens (op, Lens', (.~)
import           Control.Lens.TH

-- Graphics
import           Data.Colour.SRGB as C

-- From Diagrams
import           Diagrams.Prelude as D hiding (Attribute)
import           Diagrams.TwoD.Path

import Diagrams.Backend.OpenGL.TwoD.Outlines (trlVertices, Convex(..))
import Diagrams.Backend.OpenGL.TwoD.Tesselate

type GLRenderM a = State GLRenderState a

data GLRenderState =
  GLRenderState{ _currentLineColor  :: AlphaColour Double
               , _currentFillColor  :: AlphaColour Double
               , _currentOpacity    :: Double
               , _currentLineWidth  :: Double
               , _currentLineCap    :: LineCap
               , _currentLineJoin   :: LineJoin
               , _currentFillRule   :: TessWinding
               , _currentDashing    :: Dashing
               , _currentClip       :: [Convex]
               }

makeLenses ''GLRenderState

initialGLRenderState :: GLRenderState
initialGLRenderState =   GLRenderState
               { _currentLineColor  = (opaque black)
               , _currentFillColor  = transparent
               , _currentOpacity    = 1
               , _currentLineWidth  = 0.01
               , _currentLineCap    = LineCapButt
               , _currentLineJoin   = LineJoinMiter
               , _currentFillRule   = TessWindingNonzero
               , _currentDashing    = Dashing [] 0
               , _currentClip       = []
               }

{- Style changes -}

withStyleState :: Style R2 -> GLRenderM a -> GLRenderM a
withStyleState s = do
    prev <- get
    modify . foldr (.) . map ($s) $
        [ changeWith (toAlphaColour . getLineColor) currentLineColor
        , changeWith (toAlphaColour . getFillColor) currentFillColor
        , changeWith getOpacity currentOpacity
        , changeWith getLineWidth currentLineWidth
        , changeWith getLineCap currentLineCap
        , changeWith getLineJoin currentLineJoin
        , changeWith getFillRule currentFillRule
        , changeWith getDashing currentDashing
        , changeClip
        ]
    set prev  -- TODO restore only changed values?

-- | @changeWith get set sty@ is @id@ if @sty@ does not have the
-- 'Attribute' specified by @get@.  If the @Attribute@ is available,
-- @changeWith@ returns a function which sets it.
changeWith :: AttributeClass a =>
              (a -> b) -> (Lens' b GLRenderState) -> Style R2 -> GLRenderState -> GLRenderState
changeWith g s sty = case g <$> getAttr sty of
    Just v -> s .~ v
    Nothing -> id

changeClip :: Style R2 -> GLRenderState -> GLRenderState
changeClip s = case op Clip <$> getAttr s of
    Just (Path trs:_) ->
        currentClip .~ tessRegion TessWindingNonzero $ map trlVertices trs
    _ -> id
