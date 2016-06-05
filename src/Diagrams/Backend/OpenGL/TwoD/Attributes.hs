{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RankNTypes                 #-}

module Diagrams.Backend.OpenGL.TwoD.Attributes
       ( GLRenderM, GLRenderState (..), withStyleState , initialGLRenderState)
       where

-- General  Haskell
import           Control.Monad.State
import           Control.Lens.TH

-- From Diagrams
import           Diagrams.Prelude as D hiding (Attribute)
import           Diagrams.TwoD.Path
import Diagrams.TwoD.Attributes

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
               , _currentDashing    :: Dashing Double
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

changeFT :: FillTexture Double -> Maybe (AlphaColour Double)
changeFT = (^? _AC) . getFillTexture

changeLT :: LineTexture Double -> Maybe (AlphaColour Double)
changeLT = (^? _AC) . getLineTexture

withStyleState :: Style V2 Double -> GLRenderM a -> GLRenderM a
withStyleState s act = do
    prev <- get
    modify . foldr1 (.) . map ($ s) $
        [ changeWith changeLT currentLineColor
        , changeWith changeFT currentFillColor
        , changeWith (pure . getOpacity) currentOpacity
        , changeWith (pure . getLineWidth) currentLineWidth
        , changeWith (pure . getLineCap) currentLineCap
        , changeWith (pure . getLineJoin) currentLineJoin
        , changeWith (pure . fr . getFillRule) currentFillRule
        , changeWith (pure . getDashing) currentDashing
        , changeClip
        ]
    r <- act
    put prev  -- TODO restore only changed values?
    return r

-- | @changeWith get set sty@ is @id@ if @sty@ does not have the
-- 'Attribute' specified by @get@.  If the @Attribute@ is available,
-- @changeWith@ returns a function which sets it.
changeWith :: AttributeClass a =>
              (a -> Maybe b) -> (Lens' GLRenderState b) -> Style V2 Double -> GLRenderState -> GLRenderState
changeWith g s sty = case getAttr sty >>= g of
    Just v -> s .~ v
    Nothing -> id

changeClip :: Style V2 Double -> GLRenderState -> GLRenderState
changeClip s = case op Clip <$> getAttr s of
    Just (Path trs:_) ->
        currentClip .~ (tessRegion TessWindingNonzero $ map trlVertices trs)
    _ -> id

fr :: FillRule -> TessWinding
fr Winding = TessWindingNonzero
fr EvenOdd = TessWindingOdd
