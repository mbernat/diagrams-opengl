module Graphics.Rendering.Util where

-- Wrap OpenGL calls in a slightly more declarative syntax
-- TODO more consistent naming

import Data.Colour.SRGB
import Data.Colour.Names () -- instance Show Colour
import Foreign.Ptr
import Foreign.Storable
import System.IO
import qualified Data.Vector.Storable as V

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLUT as GLUT hiding (RGB)

data GlPrim = GlPrim PrimitiveMode (Colour Float) (V.Vector GLfloat)

instance Show GlPrim where
  show (GlPrim mode c v) = concat ["GlPrim ", show mode, " ", show c, " ", show v]

initProgram :: String -> String -> IO Program
initProgram v f = do
  [vSh] <-  genObjectNames 1
  [fSh] <- genObjectNames 1
  shaderSource vSh $= [v]
  shaderSource fSh $= [f]
  compileShader vSh
  compileShader fSh

  [shProg] <- genObjectNames 1
  attachedShaders shProg $= ([vSh], [fSh])
  linkProgram shProg
  return shProg

-- | Load a vertex shader and a fragment shader from the specified files
loadShaders :: String -> String -> IO Program
loadShaders vFile fFile = do
  withFile vFile ReadMode $ \vHandle -> do
    withFile fFile ReadMode $ \fHandle -> do
      vText <- hGetContents vHandle
      fText <- hGetContents fHandle
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
glColor :: (Real a, Floating a, Fractional b) => Colour a -> Color4 b
glColor c = Color4 r g b 1 where
  rgb = toSRGB c
  r = realToFrac $ channelRed rgb
  g = realToFrac $ channelGreen rgb
  b = realToFrac $ channelBlue rgb

drawOGL :: NumComponents -> GlPrim -> IO ()
drawOGL dims (GlPrim mode c v) = draw dims mode c v

-- | The first argument is the number of coördinates given for each vertex
--   2 and 3 are readily interpreted; 4 indicates homogeneous 3D coördinates
draw :: (Real c, Floating c) => NumComponents -> PrimitiveMode -> Colour c -> V.Vector GLfloat -> IO ()
draw dims mode c pts = do
  color $ (glColor c :: Color4 GLfloat) -- all vertices same color
  V.unsafeWith pts $ \ptr -> do
    arrayPointer VertexArray $= VertexArrayDescriptor dims Float 0 ptr

  drawArrays mode 0 ptCount where
    ptCount = fromIntegral $ V.length pts `quot` (fromIntegral dims)

initGlut :: IO () -> IO ()
initGlut display = do
  _ <- GLUT.getArgsAndInitialize
  GLUT.createWindow "Diagrams"
  GLUT.displayCallback $= display
  clientState VertexArray $= Enabled
  GLUT.mainLoop
