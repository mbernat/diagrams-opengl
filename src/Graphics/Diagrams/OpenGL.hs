{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.Diagrams.OpenGL where

-- General Haskell
import Prelude
import Data.List
import           Data.Typeable
import qualified Data.Vector.Storable as V

-- Graphics
import Data.Colour.Names
import Graphics.Rendering.OpenGL hiding (RGB, Linear, Render)

-- From Diagrams
import Diagrams.Prelude hiding (Attribute, close, e, (<>))
import Graphics.Rendering.Util

renderPath :: Path R2 -> Render OpenGL R2
renderPath (Path trs) = GlRen $ fmap renderTrail trs

renderTrail :: (P2, Trail R2) -> GlPrim
renderTrail (p0, t) = GlPrim mode white vertices where
  mode = case isClosed t of
    True  -> LineLoop
    False -> LineStrip
  vertices = V.fromList $ concatMap flatP2 $ trailVertices p0 t

flatP2 :: (Fractional a, Num a) => P2 -> [a]
flatP2 (unp2 -> (x,y)) = [r2f x, r2f y]

r2f :: (Real a, Fractional b) => a -> b
r2f x = realToFrac x

data OpenGL = OpenGL
            deriving (Show, Typeable)

instance Backend OpenGL R2 where
  data Render OpenGL R2 = GlRen [GlPrim]
                        deriving Show
  type Result OpenGL R2 = IO ()
  data Options OpenGL R2 = GlOptions
                           { size :: SizeSpec2D   -- ^ The requested size.
                           }
                         deriving Show

  withStyle _ _ _ r = r

-- | The OpenGL backend expects doRender to be called in a loop.
--   Ideally, most of the work would be done on the first rendering,
--   and subsequent renderings should require very little CPU computation
  doRender _ _ = displayPrims

  -- type Result OpenGL R2 = String
  -- data Options OpenGL R2 = GlOptions

  -- withStyle _ _ _ r = r

  -- doRender _ _ (GlRen ps) = intercalate "\n" $ map show ps

displayPrims :: Render OpenGL R2 -> IO ()
displayPrims (GlRen ps) =do
    clearColor $= glColor (black :: Colour GLfloat)
    clear [ColorBuffer]
    mapM_ (drawOGL 2) ps
    flush

instance Monoid (Render OpenGL R2) where
  mempty = GlRen mempty
  (GlRen r0) `mappend` (GlRen r1) = GlRen $ r0 `mappend` r1

instance Renderable (Path R2) OpenGL where
  render _ = renderPath

instance Renderable (Trail R2) OpenGL where
  render c t = render c $ Path [(p2 (0,0), t)]

instance Renderable (Segment R2) OpenGL where
  render c = render c . flip Trail False . (:[])
