module Lizard exposing (..)

import Shape exposing (..)

pt : Float -> Float -> Point
pt x y = { x = x, y = y }

curve : Point -> Point -> Point -> PathSegment 
curve cp1 cp2 ep = 
  CurveSegment 
  { controlPoint1 = cp1
  , controlPoint2 = cp2
  , endPoint = ep }

lizard : PathDef
lizard = 
  { start = pt 0 0 
  , segments = [
      curve (pt 0.020 0.050) (pt 0.030 0.120) (pt 0.025 0.185)
    , curve (pt 0.100 0.120) (pt 0.200 0.085) (pt 0.310 0.090)
    ] }
