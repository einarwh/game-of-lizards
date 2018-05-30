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
type alias Directions = 
  { north : Figure
  , west : Figure 
  , south : Figure 
  , east : Figure }

cellSize : (Float, Float) 
cellSize = (20, 20)

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
        xmax = 20
        ymax = 20
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
  Time.every (350 * Time.millisecond) (\_ -> Tick)

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

movePoint : (Float, Float) -> Point -> Point 
movePoint (dx, dy) { x, y } = 
  { x = x + dx, y = y + dy }

turnPoint : Point -> Point 
turnPoint { x, y } = { x = 1 - y, y = x }

withLine : (Point -> Point) -> Line -> Line 
withLine fn { targetPoint } = 
  { targetPoint = fn targetPoint }

withCurve : (Point -> Point) -> Curve -> Curve 
withCurve fn { controlPoint1, controlPoint2, endPoint } = 
  { controlPoint1 = fn controlPoint1
  , controlPoint2 = fn controlPoint2 
  , endPoint = fn endPoint }

withSegment : (Point -> Point) -> PathSegment -> PathSegment
withSegment fn ps = 
  case ps of 
    LineSegment ls -> LineSegment (withLine fn ls)
    CurveSegment cs -> CurveSegment (withCurve fn cs)

withPath : (Point -> Point) -> PathDef -> PathDef 
withPath fn { start, segments, closed } = 
  { start = fn start
  , segments = segments |> List.map (withSegment fn)
  , closed = closed }

withCircle : (Point -> Point) -> CircleDef -> CircleDef
withCircle fn { center, radius } = 
  { center = fn center
  , radius = radius }

withShape : (Point -> Point) -> Shape -> Shape
withShape fn shape = 
  case shape of 
    CircleShape c -> CircleShape (withCircle fn c)
    PathShape p -> PathShape (withPath fn p)

withFigure : (Point -> Point) -> Figure -> Figure
withFigure fn = List.map (withShape fn)

moveFigure : (Float, Float) -> Figure -> Figure 
moveFigure f =
  withFigure (movePoint f)

scaleFigure : (Float, Float) -> Figure -> Figure 
scaleFigure f = 
  withFigure (scalePoint f)

turnFigure : Figure -> Figure  
turnFigure = withFigure turnPoint 

mirrorPoint : Int -> Point -> Point 
mirrorPoint ymax {x, y} = 
  {x = x, y = toFloat ymax - y}
  
mirrorFigure : Int -> Figure -> Figure 
mirrorFigure ymax = 
  withFigure (mirrorPoint ymax)

lizards : Directions
lizards = 
  let 
    lz1 = lizard |> scaleFigure cellSize 
    lz2 = lizard |> turnFigure |> scaleFigure cellSize 
    lz3 = lizard |> turnFigure |> turnFigure |> scaleFigure cellSize 
    lz4 = lizard |> turnFigure |> turnFigure |> turnFigure |> scaleFigure cellSize  
  in
    { north = lz1 
    , west = lz2 
    , south = lz3 
    , east = lz4 }

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

pathToSvg : PathDef -> Svg.Svg msg 
pathToSvg { start, segments, closed } =
  let 
    toStr : Point -> String 
    toStr {x, y} = 
      (toString x) ++ " " ++ (toString y)
    toLine : Line -> String
    toLine { targetPoint } = 
      "L " ++ (toStr targetPoint)
    toCurve : Curve -> String 
    toCurve { controlPoint1, controlPoint2, endPoint } = 
      let
        cp1 = toStr controlPoint1
        cp2 = toStr controlPoint2 
        ep = toStr endPoint 
      in 
        "C " ++ cp1 ++ " " ++ cp2 ++ " " ++ ep 
    segToStr : PathSegment -> String 
    segToStr seg = 
      case seg of 
        LineSegment l -> toLine l
        CurveSegment c -> toCurve c
    st = toStr start 
    segs : String
    segs = segments |> List.map segToStr |> String.join " "
    dval = "M" ++ st ++ " " ++ segs
  in
    Svg.path 
      [ stroke "white"
      , strokeWidth "0.3"
      , fill (if closed then "black" else "None")
      , d (if closed then dval ++ " Z" else dval)] []  

circleToSvg : CircleDef -> Svg.Svg msg
circleToSvg { center, radius } = 
  case center of 
    {x, y} -> 
      Svg.circle 
        [ fill "white"
        , cx (toString x)
        , cy (toString y)
        , r (toString radius) ] []

shapeToSvg : Shape -> Svg.Svg msg 
shapeToSvg shape = 
  case shape of 
    CircleShape c -> circleToSvg c
    PathShape p -> pathToSvg p 
      
figureToSvg : Figure -> List (Svg.Svg msg)
figureToSvg shapes = 
  shapes |> List.map shapeToSvg

chooseLizard : (Int, Int) -> Figure 
chooseLizard cellType = 
  case cellType of 
    (0, 0) -> lizards.north 
    (1, 0) -> lizards.west
    (0, 1) -> lizards.east 
    _ -> lizards.south 

toSvg : (Float, Float) -> LiveCell -> List (Svg.Svg msg)
toSvg (w, h) cell = 
  case cell of 
    (xval, yval) -> 
      let 
        xpos = toFloat xval * w 
        ypos = toFloat yval * h 
        lz = chooseLizard (xval % 2, yval % 2)
        movedLizard = moveFigure (xpos, ypos) lz |> mirrorFigure 396
      in 
        figureToSvg movedLizard

view : Model -> Html Msg
view model =
  let
    --foos = List.map (toRect (6, 6)) model 
    elements = List.concatMap (toSvg cellSize) model 
    cellCount = List.length model
    minY = findMinY model 
    maxY = findMaxY model 
  in
    div [] [
      svg [ viewBox "0 0 396 396", width "600px" ] elements
    , div [] [ text (toString cellCount) ]
    , div [] [ text (toString minY) ]
    , div [] [ text (toString maxY) ]
    ]
