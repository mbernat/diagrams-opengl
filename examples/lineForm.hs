import Diagrams.Prelude hiding (doRender)
import Diagrams.Backend.OpenGL
import Diagrams.Backend.OpenGL.CmdLine

path :: Diagram OpenGL R2
path = fromVertices (map p2 [(0,0), (1,0.3), (2,0), (2.2,0.3)]) # lw 0.1

example :: Diagram OpenGL R2
example = centerXY . vcat' with { sep = 0.1 }
          $ map (path #)
            [ lineCap LineCapButt   . lineJoin LineJoinMiter
            , lineCap LineCapRound  . lineJoin LineJoinRound
            , lineCap LineCapSquare . lineJoin LineJoinBevel
            , dashing [0.1,0.2,0.3,0.1] 0
            ]

main :: IO ()
main = defaultMain example
