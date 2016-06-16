module ChampionInfo exposing (..)

import Json.Decode exposing (..)
import Core exposing (..)
import Html exposing (..)

-- MODEL

type alias Info = 
    { attack: Int
    , defense: Int
    , difficulty: Int
    , magic: Int
    }

type Model = Model Info

info : Decoder Info
info =
    Info
    <$> "attack" := int
    <+> "defense" := int
    <+> "difficulty" := int
    <+> "magic" := int

  
decoder : Decoder Model
decoder = map Model info

-- UPDATE 

type Msg = NewChampionInfo Model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        NewChampionInfo new ->
            (new, Cmd.none)
 
emptyInfo : Info
emptyInfo = Info 0 0 0 0

-- VIEW

view : Model -> Html Msg
view (Model info) =
    text <| toString info


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- INIT

init : (Model, Cmd Msg)
init = (Model emptyInfo, Cmd.none)