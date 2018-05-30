module Shape exposing (..)

type alias Point = { x : Float, y : Float }

type alias Line = 
  { targetPoint : Point }

type alias Curve = 
  { controlPoint1 : Point
  , controlPoint2 : Point
  , endPoint : Point }

type alias CircleDef = 
  { center : Point
  , radius : Float }

type PathSegment = 
    LineSegment Line 
  | CurveSegment Curve
  
type alias PathDef = 
  { start : Point
  , segments : List PathSegment
  , closed : Bool }

type Shape = 
    PathShape PathDef 
  | CircleShape CircleDef

type alias Figure = List Shape
  