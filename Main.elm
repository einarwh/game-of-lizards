import Html exposing (Html, div)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Dict exposing (..)
import Lizard exposing (..)
import Shape exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias LiveCell = (Int, Int)
type alias Model = List LiveCell

copperhead : Model 
copperhead =  
  [ (2,0), (3,0)
  , (1,1), (2,1), (3,1), (4,1)
  , (0,4), (5,4)
  , (0,5), (5,5)
  , (0,6), (1,6), (4,6), (5,6)
  , (1,7), (2,7), (3,7), (4,7)
  , (0,8), (5,8)
  , (0,9), (5,9)
  , (1,10), (4,10)
  , (1,11), (4,11)
  , (2,12), (3,12) ]

translate : (Int, Int) -> LiveCell -> LiveCell 
translate (xoffset, yoffset) (x, y) = 
  (xoffset + x, yoffset + y)

init : (Model, Cmd Msg)
init =
  (copperhead |> List.map (translate (10, 0)), Cmd.none)

type Msg
  = Tick

evolve : List LiveCell -> List LiveCell 
evolve cells = 
  let 
    inc : LiveCell -> Dict LiveCell Int -> Dict LiveCell Int 
    inc cell d = 
      case Dict.get cell d of 
        Just v -> Dict.insert cell (v + 1) d
        Nothing -> Dict.insert cell 1 d
    add : LiveCell -> Dict LiveCell Int -> Dict LiveCell Int 
    add (x,y) d = 
      let 
        xmax = 32
        ymax = 32
        xleft = (x + xmax - 1) % xmax
        xright = (x + 1) % xmax 
        ybot = (y + ymax - 1) % ymax 
        ytop = (y + 1) % ymax 
      in
        d |> inc (xleft, ybot)
          |> inc (xleft, y)
          |> inc (xleft, ytop)
          |> inc (x, ybot)
          |> inc (x, ytop)
          |> inc (xright, ybot)
          |> inc (xright, y)
          |> inc (xright, ytop)        
    neighbors = List.foldl add Dict.empty cells 
    neighborList = Dict.toList neighbors
    bumpLive (cell, count) = 
      if count == 2 && List.member cell cells then (cell, 3) else (cell, count)
    candidates = List.map bumpLive neighborList
    alive (cell, count) = count == 3
    justCell (cell, count) = cell
  in 
    candidates |> List.filter alive |> List.map justCell

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (evolve model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (100 * Time.millisecond) (\_ -> Tick)

findMinY : List LiveCell -> Int 
findMinY cells =
  cells |> List.map (\(x,y) -> y)
        |> List.minimum
        |> Maybe.withDefault -1

findMaxY : List LiveCell -> Int 
findMaxY cells =
  cells |> List.map (\(x,y) -> y)
        |> List.maximum
        |> Maybe.withDefault -1

scalePoint : (Float, Float) -> Point -> Point 
scalePoint (xf, yf) { x, y } = 
  { x = x * xf, y = y * yf }

scaleLine : (Float, Float) -> Line -> Line 
scaleLine f { lineTo } = 
  { lineTo = scalePoint f lineTo }

scaleCurve : (Float, Float) -> Curve -> Curve 
scaleCurve f { controlPoint1, controlPoint2, endPoint } = 
  { controlPoint1 = scalePoint f controlPoint1
  , controlPoint2 = scalePoint f controlPoint2 
  , endPoint = scalePoint f endPoint }

scaleSegment : (Float, Float) -> PathSegment -> PathSegment
scaleSegment f ps = 
  case ps of 
    LineSegment ls -> LineSegment (scaleLine f ls)
    CurveSegment cs -> CurveSegment (scaleCurve f cs)

scalePath : (Float, Float) -> PathDef -> PathDef 
scalePath f { start, segments } = 
  { start = scalePoint f start
  , segments = segments |> List.map (scaleSegment f) }

toRect : (Int, Int) -> LiveCell -> Svg.Svg msg
toRect (w, h) cell = 
  case cell of 
    (xval, yval) -> 
      let 
        xpos = xval * w 
        ypos = yval * h 
      in 
        Svg.rect [ x (toString xpos)
                 , y (toString ypos)
                 , width (toString w)
                 , height (toString h)
                 , fill "black" ] []

view : Model -> Html Msg
view model =
  let
    foos = List.map (toRect (6, 6)) model 
    cellCount = List.length model
    minY = findMinY model 
    maxY = findMaxY model 
  in
    div [] [
      svg [ viewBox "0 0 192 192", width "400px" ] foos
    , div [] [ text (toString cellCount) ]
    , div [] [ text (toString minY) ]
    , div [] [ text (toString maxY) ]
    ]
