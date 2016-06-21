module Recommended exposing (..)

import Html exposing (..)
import Core exposing (..)
import Block
import Item
import Json.Decode exposing (..)
import List
import Realm

 
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
  
-- VIEW

view : Realm.Model -> Model -> Html a 
view realm (Model recommended) =
  let
    f: Block.Model -> Html a
    f x = div [] <| [h3 [] [text <| Block.typ x], br [] []] ++  List.map (Item.icon realm) (Block.items x)
  in
    div []
     <| h1 [] [text recommended.typ] ::  List.map f recommended.blocks
