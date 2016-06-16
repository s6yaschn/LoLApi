module Recommended exposing (..)

import Html exposing (..)
import Html.App as Html
import Core exposing (..)
import Block
import Json.Decode exposing (..)

main = Html.program
    { init = init 
    , update = update
    , subscriptions = subscriptions
    , view = view 
    }

 
-- MODEL

type alias Recommended =
    { blocks: List Block.Model
    , champion: String
    , map: String
    , mode: String
    , priority: Bool
    , title: String
    , typ: String -- originally type
    }

type Model = Model Recommended

recommended : Decoder Recommended
recommended =   
    Recommended
    <$> "blocks" := list Block.decoder
    <+> "champion" := string
    <+> "map" := string 
    <+> "mode" := string
    <+>  oneOf ["priority" := bool, succeed False] -- not always included, default to False
    <+> "title" := string
    <+> "type" := string

decoder : Decoder Model
decoder = Json.Decode.map Model recommended

emptyRecommended: Recommended
emptyRecommended = Recommended [] "" "" "" False "" ""


-- ACCESSORS

blocks : Model -> List Block.Model
blocks (Model rec) = rec.blocks

map : Model -> String 
map (Model rec) = rec.map

mode : Model -> String 
mode (Model rec) = rec.mode

title : Model -> String 
title (Model rec) = rec.title

typ : Model -> String 
typ (Model rec) = rec.typ

-- UPDATE

type Msg = NewRecommended Recommended

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewRecommended new -> 
          (Model new, Cmd.none)


  
-- VIEW

view : Model -> Html Msg 
view (Model recommended) =
  let
    f: Block.Model -> Html Msg
    f x = div [] [h3 [] [text <| Block.typ x], br [] [],  Block.view x ]
  in
    div []
     <| h1 [] [text recommended.typ] ::  List.map f recommended.blocks

 
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- INIT

init : (Model, Cmd Msg)
init = (Model emptyRecommended, Cmd.none)