module Graphics.Rendering.Util.TwoD where

import Data.Colour
import Foreign.Ptr
import Foreign.Storable
import System.IO
import qualified Data.Vector.Storable as V

import Diagrams.TwoD.Types
import Diagrams.Attributes
import Graphics.Rendering.OpenGL
import Graphics.Rendering.Util

data GlPrim a = GlPrim {
  primMode  :: PrimitiveMode,
  primColor :: (AlphaColour Double),
  primPts   :: [a] }

instance (Show a) => Show (GlPrim a) where
  show (GlPrim mode c ps) = concat ["GlPrim ", show mode, " ", show c, " ", show ps]

draw2 :: GlPrim P2 -> IO ()
draw2 (GlPrim mode c pts) = draw 2 mode c $ V.fromList . map r2f . flatP2 $ pts

-- | The first argument is the number of coördinates given for each vertex
--   2 and 3 are readily interpreted; 4 indicates homogeneous 3D coördinates
draw :: (Real c, Floating c) => NumComponents -> PrimitiveMode -> AlphaColour c -> V.Vector GLfloat -> IO ()
draw dims mode c pts = do
  color $ (glColor c :: Color4 GLfloat) -- all vertices same color
  V.unsafeWith pts $ \ptr -> do
    arrayPointer VertexArray $= VertexArrayDescriptor dims Float 0 ptr

  drawArrays mode 0 ptCount where
    ptCount = fromIntegral $ V.length pts `quot` (fromIntegral dims)
