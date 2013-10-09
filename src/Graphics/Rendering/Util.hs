--{-# LANGUAGE DatatypeContexts #-}

module Graphics.Rendering.Util where

-- Wrap OpenGL calls in a slightly more declarative syntax
-- TODO more consistent naming

import qualified Data.ByteString as BS
import Data.Colour
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Vector.Storable as V

import Diagrams.TwoD.Types
import Diagrams.Attributes
import Graphics.Rendering.OpenGL

initProgram :: BS.ByteString -> BS.ByteString -> IO Program
initProgram v f = do
  vSh <-  createShader VertexShader
  fSh <- createShader FragmentShader
  shaderSourceBS vSh $= v
  shaderSourceBS fSh $= f
  compileShader vSh
  compileShader fSh

  shProg <- createProgram
  attachedShaders shProg $= [vSh, fSh]
  linkProgram shProg
  return shProg

-- | Load a vertex shader and a fragment shader from the specified files
loadShaders :: String -> String -> IO Program
loadShaders vFile fFile = do
    vText <- BS.readFile vFile
    fText <- BS.readFile fFile
    initProgram vText fText

-- TODO wrap output with info about length, foor use in render
initGeometry :: Storable a => V.Vector a -> IO BufferObject
initGeometry tris = do
  [vbo] <- genObjectNames 1
  bindBuffer ArrayBuffer $= Just vbo
  let len = fromIntegral $ V.length tris  * sizeOf (V.head tris)
  V.unsafeWith tris $ \ptr ->
    bufferData ArrayBuffer $= (len, ptr, StaticDraw)
  return vbo

bindVao :: BufferObject -> IO VertexArrayObject
bindVao vb = do
  [vao] <- genObjectNames 1
  bindVertexArrayObject $= Just vao
  vertexAttribArray (AttribLocation 0)  $= Enabled
  bindBuffer ArrayBuffer $= Just vb
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
  return vao

-- | Convert a haskell / diagrams color to OpenGL
--   TODO be more correct about color space
glColor :: (Real a, Floating a, Fractional b) => AlphaColour a -> Color4 b
glColor c = Color4 r g b a where
  -- rgb = toSRGB c
  -- r = realToFrac $ channelRed rgb
  -- g = realToFrac $ channelGreen rgb
  -- b = realToFrac $ channelBlue rgb
  (r,g,b,a) = r2fQuad $ colorToSRGBA c

r2f :: (Real r, Fractional f) => r -> f
r2f x = realToFrac x

r2fPr :: (Real r, Fractional f) => (r,r) -> (f,f)
r2fPr (a,b) = (r2f a, r2f b)

r2fQuad :: (Real r, Fractional f) => (r,r,r,r) -> (f,f,f,f)
r2fQuad (a,b,c,d) = (r2f a, r2f b, r2f c, r2f d)

flatP2 :: (Fractional a, Num a) => [P2] -> [a]
flatP2 = (concatMap (flat2 . unp2))

flat2 :: (Real r, Fractional a, Num a) => (r, r) -> [a]
flat2 (a,b) = [r2f a, r2f b]
