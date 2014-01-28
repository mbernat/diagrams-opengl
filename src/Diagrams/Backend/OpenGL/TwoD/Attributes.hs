{-# LANGUAGE ViewPatterns               #-}

module Diagrams.Backend.OpenGL.TwoD.Attributes where


-- General  Haskell
import           Control.Monad.State
-- import           Data.Semigroup
-- import           Data.Tuple
import           Control.Lens (op)
-- import           System.IO.Unsafe
-- import qualified Data.Vector.Storable as V

-- Graphics
import           Data.Colour.SRGB as C

-- From Diagrams
--import           Diagrams.BoundingBox
import           Diagrams.Prelude as D hiding (Attribute)
import           Diagrams.TwoD.Path

import Diagrams.Backend.OpenGL.TwoD.Outlines (trlVertices)
import Diagrams.Backend.OpenGL.TwoD.Tesselate

type GLRenderM a = State GLRenderState a

data GLRenderState =
  GLRenderState{ currentLineColor  :: AlphaColour Double
               , currentFillColor  :: AlphaColour Double
               , currentOpacity    :: Double
               , currentLineWidth  :: Double
               , currentLineCap    :: LineCap
               , currentLineJoin   :: LineJoin
               , currentFillRule   :: TessWinding
               , currentDashArray  :: [Double]
               , currentDashOffset :: Double
               , currentClip       :: [[P2]]
               }

{- Style changes -}

changeLineColor :: Style v -> GLRenderM ()
changeLineColor s =
  case lcol of
    Just (r, g, b, a) ->
      modify $ \st -> st{currentLineColor = withOpacity (sRGB r g b) a}
    Nothing           -> return ()
 where lcol = colorToSRGBA <$> getLineColor <$> getAttr s

changeFillColor :: Style v -> GLRenderM ()
changeFillColor s =
  case fcol of
    Just (r, g, b, a) ->
      modify $ \st -> st{currentFillColor = withOpacity (sRGB r g b) a}
    Nothing           -> return ()
 where fcol = colorToSRGBA <$> getFillColor <$> getAttr s

changeOpacity :: Style v -> GLRenderM ()
changeOpacity s =
  case opa of
    Just o -> modify $ \st -> st{currentOpacity = o}
    Nothing           -> return ()
 where opa =  getOpacity <$> getAttr s

changeLineWidth :: Style v -> GLRenderM ()
changeLineWidth s =
  case lwid of
    Just a  -> modify $ \st -> st{currentLineWidth = realToFrac a}
    Nothing -> return ()
 where lwid = getLineWidth <$> getAttr s

changeLineCap :: Style v -> GLRenderM ()
changeLineCap s =
  case lcap of
    Just a  -> modify $ \st -> st{currentLineCap = a}
    Nothing -> return ()
 where lcap = getLineCap <$> getAttr s

changeLineJoin:: Style v -> GLRenderM ()
changeLineJoin s =
  case lj of
    Just a  -> modify $ \st -> st{currentLineJoin = a}
    Nothing -> return ()
 where lj = getLineJoin <$> getAttr s

changeFillRule :: Style v -> GLRenderM ()
changeFillRule s =
  case fr of
    Just Winding -> modify $ \st -> st{currentFillRule = TessWindingNonzero}
    Just EvenOdd -> modify $ \st -> st{currentFillRule = TessWindingOdd}
    Nothing      -> return ()
 where fr = getFillRule <$> getAttr s

changeDashing :: Style v -> GLRenderM ()
changeDashing s =
  case dash of
    Just (Dashing a o) ->
      modify $ \st ->
      st{ currentDashArray  = map realToFrac a
        , currentDashOffset = realToFrac o
        }
    Nothing      -> return ()
 where dash = getDashing <$> getAttr s

changeClip :: Style v -> GLRenderM ()
changeClip s =
  case clip of
    Just (Path trs:_) ->
      modify $ \st ->
      st{ currentClip = tessRegion TessWindingNonzero $
                        map trlVertices trs
        }
    Just _       -> return ()
    Nothing      -> return ()
 where clip = op Clip <$> getAttr s
