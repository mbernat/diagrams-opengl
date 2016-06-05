{-# LANGUAGE ViewPatterns               #-}

module Diagrams.Backend.OpenGL.TwoD.Outlines
       ( calcLines, trlVertices, Convex(..) )
       where

-- General  Haskell
import           Control.Lens ((^.))
import           Data.Tuple

-- From Diagrams
import           Diagrams.Prelude as D hiding (Attribute)
import           Diagrams.TwoD.Arc
import           Graphics.Rendering.Util

{- calculate drawn outlines of styled lines -}

newtype Convex = Convex { unConvex :: [P2] }

-- | calcLines is (nearly) the sole entry point for this module
calcLines :: Dashing
             -> Double -- ^ Line Width
             -> LineCap
             -> LineJoin
             -> [P2]  -- ^ Points from a single Trail, with curves already linearized
             -> [Convex] -- ^ Each inner list is the outline of a (convex) polygon
calcLines _ 0 _ _ _ = []
calcLines dash lwf lcap lj ps@(_:_:_) =
  case dash of
    (Dashing [] _)
        -> map (calcLine lwf) strokedLines <>
             map (calcJoin lj lwf) joins
    (Dashing darr _)
        -> calcDashedLines (cycle darr) False lwf lcap lj strokedLines
           <> if dist < 0.0001
              then [calcJoin lj lwf (pup, fp, sp)]
              else [calcCap lwf lcap $ swap $ head strokedLines] <>
                   [calcCap lwf lcap $ last strokedLines]
 where strokedLines = zip  ps (tail ps)
       joins = zip3 ps (tail ps) (tail $ tail ps)
       pup   = ps !! (length ps - 2)
       lp    = last ps
       fp    = head ps
       sp    = ps !! 1
       dist = magnitude $ lp .-. fp
calcLines _ _ _ _ _ = mempty

calcDashedLines :: [Double] -- ^ Dashing pattern, first element of Dashing type
                   -> Bool -- ^ Currently in a gap between dashes
                   -> Double -- ^ Line Width
                   -> LineCap
                   -> LineJoin
                   -> [(P2, P2)] -- ^ Line segments, defined by their endpoints
                   -> [Convex] -- ^ Each inner list is the outline of a (convex) polygon
calcDashedLines (d:ds) hole lwf lcap lj ((p0, p1):ps) =
  if hole
  then if len >= d
       then calcDashedLines ds           (not hole) lwf lcap lj ((p0 .+^ vec, p1):ps)
       else calcDashedLines (d - len:ds) (    hole) lwf lcap lj ps
  else if len >= d
       then calcLine lwf (p0, p0 .+^ vec):
            calcDashedLines ds           (not hole) lwf lcap lj ((p0 .+^ vec, p1):ps)
       else calcLine lwf (p0, p1):
            case ps of
              ((_, p3):_) -> calcJoin lj lwf (p0, p1, p3):
                             calcDashedLines (d - len:ds) hole lwf lcap lj ps
              []     -> mempty
 where len = magnitude (p1 .-. p0)
       vec = normalized (p1 .-. p0) ^* d
calcDashedLines _ _ _ _ _ _ = mempty

calcCap :: Double -- ^ Line Width
           -> LineCap
           -> (P2, P2) -- ^ Endpoints of final line segment
           -> Convex -- ^ The outline of the line's end cap
calcCap lwf lcap (p0, p1) = Convex $
  case lcap of
    LineCapButt   -> mempty
    LineCapRound  ->
      trlVertices $ (arcT (-tau/4 @@ rad) (tau/4 @@ rad)
                             # D.scale (r2f lwf/2) # D.rotate angle) `at` p1 .+^ c
    LineCapSquare -> [ p1 .+^ c
                     , p1 .-^ c
                     , p1 .+^ (norm - c)
                     , p1 .+^ (norm + c)
                     ]
 where vec   = p1 .-. p0
       norm  = normalized vec ^* (lwf/2)
       c = D.rotate (-tau/4 @@ rad) norm
       angle = direction vec

calcJoin :: LineJoin
            -> Double -- ^ Line Width
            -> (P2, P2, P2) -- ^ Two line segments meeting at the middle point
            -> Convex
calcJoin lj lwf (p0, p1, p3) = Convex $
  case lj of
    LineJoinMiter -> if abs spikeLength > 10 * lwf
                       then bevel
                       else spikeJoin
    LineJoinRound -> (p1:) $ case side of
      1 -> trlVertices $ (arc' (lwf/2) (direction v1) (direction v2)) `at` p1 .+^ v1
      _ -> trlVertices $ (arc' (lwf/2) (direction v2) (direction v1)) `at` p1 .+^ v2
    LineJoinBevel -> bevel
 where norm1       = normalized (p1 .-. p0) ^* (lwf/2)
       norm2       = normalized (p3 .-. p1) ^* (lwf/2)
       side        = if detV norm1 norm2 > 0
                       then  1
                       else -1
       v1 :: R2
       v1          = D.rotate (side * (-tau/4) @@ rad) norm1
       v2 :: R2
       v2          = D.rotate (side * (-tau/4) @@ rad) norm2
       bevel       = [ p1 .+^ v1
                     , p1 .+^ v2
                     , p1
                     ]
       spikeAngle  = (direction v1 ^-^ direction v2) ^/ 2
       spikeLength = (lwf/2) / cos (spikeAngle^.rad)
       v3 :: R2
       v3          = D.rotate (direction v1 ^-^ spikeAngle) unitX ^* spikeLength
       spikeJoin   = [ p1 .+^ v1
                     , p1 .+^ v3
                     , p1 .+^ v2
                     , p1
                     ]
       -- | The determinant of two vectors.
       detV :: R2 -> R2 -> Double
       detV (unr2 -> (x1,y1)) (unr2 -> (x2,y2)) = x1 * y2 - y1 * x2


calcLine :: Double -> (P2, P2) -> Convex
calcLine lwf (p0, p1) = Convex
  [ p0 .-^ c
  , p0
  , p0 .+^ c
  , p1 .+^ c
  , p1
  , p1 .-^ c
  ]
 where vec   = p1 .-. p0
       norm  = normalized vec ^* (lwf/2)
       c = D.rotate (-tau/4 @@ rad) norm

-- | trlVertices converts a Trail to a list of vertices, approximating
-- cubic segments in the process.  Mostly handles boundary cases.
-- @segVertices@ does the real work for each segment.
trlVertices :: Located (Trail R2) -> [P2]
trlVertices lt@(viewLoc -> (p0, t)) =
  vertices <> if isLoop t && (magnitude (p0 .-. lp) > 0.0001)
              then [p0]
              else mempty
  where vertices = concat $ zipWith segVertices
                   (trailVertices lt) (trailSegments t ++ [straight (0 ^& 0)])
        lp = last $ trailVertices lt

-- TODO do something adaptive instead of hardcoded 30 points per Cubic

-- | Add one point for Linear segments, or 30 for Cubic segments
segVertices :: P2 -> Segment Closed R2 -> [P2]
segVertices p (D.Linear _) = [p]
segVertices p cubic = map ((p .+^) . atParam cubic) [0,i..1-i] where
  i = 1/30
