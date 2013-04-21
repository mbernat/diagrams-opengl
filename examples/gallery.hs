{-# Language StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Monad

import Codec.Picture

import qualified Data.Typeable as T
import qualified Data.Vector.Storable as V
import Data.Maybe
import Data.List
import Data.IORef

import Diagrams.Prelude hiding (scale, blend, translate)
import Diagrams.Backend.OpenGL

import Graphics.Rendering.OpenGL.GL.Texturing.Objects
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.UI.GLUT

import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable

import Language.Haskell.Interpreter hiding (get)
import Language.Haskell.Interpreter.Unsafe

import System.IO
import System.IO.Unsafe
import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist)
import System.FilePath

deriving instance T.Typeable Any

loadDiagram :: forall a. (T.Typeable a) =>
               String -> IO (Diagram a R2)
loadDiagram f = do r <- runInterpreter $ loadExampleFromFile f
                   case r of
                     Left err -> print err >> return mempty
                     Right d -> return d

loadExampleFromFile :: forall a. (T.Typeable a) =>
                       String -> Interpreter (Diagram a R2)
loadExampleFromFile f =
    do
      liftIO . print $ "Loading: " ++ f
      loadModules [f]
      setTopLevelModules =<< getLoadedModules
      setImportsQ [ ("Prelude", Nothing)
                  , ("Diagrams.Prelude", Nothing)
                  , ("Diagrams.Backend.OpenGL", Nothing)]
      let expr = "example"
      interpret expr (as :: Diagram a R2)

main :: IO ()
main = do
  -- initialize window
  _ <- getArgsAndInitialize
  initialDisplayMode $= [WithSamplesPerPixel 16,WithDepthBuffer,WithDepthBuffer,RGBAMode,WithAlphaComponent]
  _ <- createWindow "Diagrams"

  -- load all examples
  let examplesDir = "examples"
  ds <- liftM (map (examplesDir </>) . sort . filter (\e -> any (`isSuffixOf` e) [".hs" , ".lhs"])) $ getDirectoryContents examplesDir
  d <- mapM (unsafeInterleaveIO . loadDiagram) ds

  -- it is needed for rendering
  clientState VertexArray $= Enabled

  --rendering
  textures <- mapM (renderDia OpenGLTexture defOpts) d

  -- load shader programs
  p <- loadShaders "examples/TransformVertexShader.vertexshader" "examples/TextureFragmentShader.fragmentshader"
  texID <- get $ uniformLocation p "myTextureSampler"
  matVPID <- withCString "VP" (\c_string -> let gl_string = castPtr c_string in glGetUniformLocation (programID p) gl_string)
  matMID <- withCString "M" (\c_string -> let gl_string = castPtr c_string in glGetUniformLocation (programID p) gl_string)

  -- variables initialisation
  perspectiveMat <- newIORef =<< perspectivePixelPerfectMatrix (Size 1000 1000) 2000 10 (-10)
  rotationMat <- newIORef =<< rotationMatrix (Vector3 0 0 1) 0
  windowSize <- newIORef (Size 100 100)
  imageNo <- newIORef 0

  -- big square
  square <- initGeometry . V.fromList $ map (*512) $
    concat [[-1, -1, 0],[ 1, -1, 0],[ 1,  1, 0]
           ,[ 1,  1, 0],[-1,  1, 0],[-1, -1, 0]]

  -- texture coordinates
  squareUV <- initGeometry . V.fromList $
    concat [[ 0, 0],[ 1, 0],[ 1, 1]
           ,[ 1, 1],[ 0, 1],[ 0, 0]]

  --init opengl
  [vao] <- genObjectNames 1
  bindVertexArrayObject $= Just vao
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  texture Texture2D $= Enabled
  clearColor $= Color4 1 1 0.9 1
  currentProgram $= Just p


  displayCallback $= do
    clear [ColorBuffer, DepthBuffer]

    curImageNo <- get imageNo
    textureBinding Texture2D $= (textures !! curImageNo)

    uniform texID $= TextureUnit 0

    curMat <- get perspectiveMat
    withMatrix curMat $ \_ ptr -> glUniformMatrix4fv matVPID 1 0 ptr
    rotMat <- get rotationMat
    withMatrix rotMat $ \_ ptr -> glUniformMatrix4fv matMID 1 0 ptr

    bindVao 3 0 square
    bindVao 2 1 squareUV
    drawArrays Triangles 0 6
    swapBuffers
  reshapeCallback $= (Just $ \s -> do
    reshape s
    windowSize $= s
    m <- perspectivePixelPerfectMatrix s 1500 3 (-3)
    perspectiveMat $= m
    return ())
  passiveMotionCallback $= (Just $ \(Position x y) -> do
    Size w h <- get windowSize
    let dx = fromIntegral $ w `div` 2 - x
        dy = fromIntegral $ h `div` 2 - y
        l  = sqrt (dx*dx + dy*dy) / fromIntegral w
    m <- rotationMatrix (Vector3 dy dx 0) l
    rotationMat $= m
    postRedisplay Nothing
    return ())
  keyboardMouseCallback $= (Just $ keyboardMouse imageNo)
  mainLoop

keyboardMouse :: IORef Int -> KeyboardMouseCallback
keyboardMouse n (Char 'j') Down _ _ = do
  modifyIORef n (+1)
  postRedisplay Nothing
keyboardMouse n (Char 'k') Down _ _ = do
  modifyIORef n (flip (-) 1)
  postRedisplay Nothing
keyboardMouse _ _ _ _ _ = return ()

defOpts :: Options OpenGLTexture R2
defOpts = GlTexOptions (withOpacity gray 0) 4096

initGeometry :: V.Vector GLfloat -> IO BufferObject
initGeometry tris = do
  [vbo] <- genObjectNames 1
  bindBuffer ArrayBuffer $= Just vbo
  let len = fromIntegral $ V.length tris * sizeOf (V.head tris)
  V.unsafeWith tris $ \ptr ->
    bufferData ArrayBuffer $= (len, ptr, StaticDraw)
  return vbo

bindVao :: GLint -> GLuint -> BufferObject -> IO ()
bindVao size loc vb = do
  vertexAttribArray (AttribLocation loc) $= Enabled
  bindBuffer ArrayBuffer $= Just vb
  vertexAttribPointer (AttribLocation loc) $= (ToFloat, VertexArrayDescriptor size Float 0 nullPtr)

initProgram :: String -> String -> IO Program
initProgram v f = do
  [vSh] <- genObjectNames 1
  [fSh] <- genObjectNames 1
  shaderSource vSh $= [v]
  shaderSource fSh $= [f]
  compileShader vSh
  compileShader fSh
  [shProg] <- genObjectNames 1
  attachedShaders shProg $= ([vSh], [fSh])
  linkProgram shProg
  print =<< get (shaderInfoLog vSh)
  print =<< get (shaderInfoLog fSh)
  print =<< get (programInfoLog shProg)
  return shProg

-- | Load a vertex shader and a fragment shader from the specified files
loadShaders :: String -> String -> IO Program
loadShaders vFile fFile =
  withFile vFile ReadMode $ \vHandle ->
    withFile fFile ReadMode $ \fHandle -> do
      vText <- hGetContents vHandle
      fText <- hGetContents fHandle
      initProgram vText fText

reshape :: Size -> IO ()
reshape (Size w 0) = reshape $ Size w 1 -- prevent divide by zero
reshape (Size w h) = viewport $= (Position 0 0, Size width height)
 where
  width = (2::GLsizei) * fromIntegral (w `div` 2)
  height = (2::GLsizei) * fromIntegral (h `div` 2)

-- | width and height defines the 2D space available at z=0, must be the same
--   as the size of the viewport.
--   z_near defines the z position of the near plane, must be greater than 0.
--   z_far defines the z position of the far plane, must be lesser than 0.
--   z_eye defines the position of the viewer, must be greater that z_near.
perspectivePixelPerfectMatrix :: Size -> GLfloat -> GLfloat -> GLfloat -> IO(GLmatrix GLfloat)
perspectivePixelPerfectMatrix (Size w h) z_eye z_near z_far =
  newMatrix ColumnMajor [(2 * z_eye) / width, 0, 0, 0
                        , 0, (2 * z_eye) / height, 0, 0
                        , 0, 0, ktz - ksz * z_eye, -1
                        , 0 :: GLfloat, 0, ksz, z_eye]
 where kdn = z_eye - z_near
       kdf = z_eye - z_far
       ksz = - (kdf + kdn) / (kdf - kdn)
       ktz = - (2 * kdn * kdf) / (kdf - kdn)
       width = 2 * fromIntegral (w `div` 2)
       height = 2 * fromIntegral (h `div` 2)

rotationMatrix :: Vector3 GLfloat -> GLfloat -> IO(GLmatrix GLfloat)
rotationMatrix axis angle =
  newMatrix ColumnMajor [ oc * x * x + c    , oc * x * y - z * s, oc * z * x + y * s, 0.0
                        , oc * x * y + z * s, oc * y * y + c    , oc * y * z - x * s, 0.0
                        , oc * z * x - y * s, oc * y * z + x * s, oc * z * z + c    , 0.0
                        , 0.0               , 0.0               , 0.0               , 1.0]
 where Vector3 x y z = normalizeVector3 axis
       s  = sin angle
       c  = cos angle
       oc = 1 - c;

normalizeVector3 :: Vector3 GLfloat -> Vector3 GLfloat
normalizeVector3 (Vector3 x y z) = Vector3 (x/l) (y/l) (z/l)
 where l = sqrt $ x*x + y*y + z*z
