module Shape exposing (..)

type alias Point = { x : Float, y : Float }

type alias Line = 
  { lineTo : Point }

type alias Curve = 
  { controlPoint1 : Point
  , controlPoint2 : Point
  , endPoint : Point }

type PathSegment = 
    LineSegment Line 
  | CurveSegment Curve
  
type alias PathDef = 
  { start : Point
  , segments : List PathSegment }
  