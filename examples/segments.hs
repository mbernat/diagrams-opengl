import Diagrams.Prelude hiding (doRender)
import Graphics.Diagrams.OpenGL
import Graphics.Rendering.Util (initGlut)

main :: IO ()
main = do
  let v1 = r2 (0,1)
  let v2 = r2 (0.5,-0.5)
  let p = fromOffsets [v1,v2] :: Path R2
  putStrLn $ show $ renderPath p
  let d = stroke p :: Diagram OpenGL R2
  let r = renderDia OpenGL (GlOptions Absolute) $ d
  initGlut $ r
  return ()
