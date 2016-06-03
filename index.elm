import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import Random

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model

type alias Model =
  { coords: List (Int, Int)
  , running: Bool
  , clicks: Int
  }


init : (Model, Cmd Msg)
init =
  (Model [] True 0, Cmd.none)

-- Update

type Msg
  = NewDot (Int, Int)
  | Create
  | Start
  | Stop
  | Reset
  | Clicked Int

removeAtIndex : Int -> List a -> List a
removeAtIndex i xs =
  let
    indexList = List.indexedMap (,) xs
    filteredIndexList = List.filter (\(idx,_) -> idx /= i) indexList
  in
    List.map (\(_,v) -> v) filteredIndexList

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewDot tuple -> 
      ({ model | coords = model.coords ++ [tuple] }, Cmd.none)

    Create -> 
      ( model
      , Random.generate NewDot (Random.pair (Random.int 5 95) (Random.int 5 95))
      )

    Clicked i ->
      ({ model |
         clicks = model.clicks + 1
       , coords = (removeAtIndex i model.coords)
       }, Cmd.none)

    Start ->
      ({ model | running = True }, Cmd.none)

    Stop ->
      ({ model | running = False }, Cmd.none)

    Reset ->
      ({ model | coords = [], clicks = 0 }, Cmd.none)


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.running then
     Time.every (2 * second / logBase 2 (toFloat model.clicks + 2))  (\_ -> Create)
  else
     Sub.none


-- View

view : Model -> Html Msg
view { coords } =
  div []
  [ div [] 
    [ button [ onClick Start ] [ text "Start" ]
    , button [ onClick Stop ] [ text "Stop" ]
    , button [ onClick Reset ] [ text "Reset" ]
    ]
  , div [] (List.indexedMap createDot coords)
  ]


createDot : Int -> (Int, Int) -> Html Msg
createDot i loc =
  img [ onClick (Clicked i), src ("https://unsplash.it/50?random&i=" ++ (toString i)), dotStyle loc i ] []


toPercent : Int -> String
toPercent x =
  (toString x) ++ "%"


dotStyle : (Int, Int) -> Int -> Attribute msg
dotStyle (x, y) i =
  style
    [ ("position", "absolute")
    , ("top", toPercent x)
    , ("left", toPercent y)
    , ("background-color", "white")
    , ("height", (toString (20*i)) ++ "px")
    , ("width", (toString (20*i)) ++ "px")
    , ("border-radius", "50%")
    ]

