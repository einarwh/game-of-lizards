import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

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

init : (Model, Cmd Msg)
init =
  (copperhead, Cmd.none)

type Msg
  = Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second (\_ -> Tick)

toRect : LiveCell -> Svg.Svg msg
toRect cell = 
  case cell of 
    (xval, yval) -> 
      let 
        xpos = xval * 5 + 10
        ypos = yval * 5 + 10 
      in 
        Svg.rect [ x (toString xpos)
                 , y (toString ypos)
                 , width "3"
                 , height "3"
                 , fill "black" ] []

view : Model -> Html Msg
view model =
  let
    foos = List.map toRect model 
  in
    svg [ viewBox "0 0 100 100", width "300px" ]
      foos
