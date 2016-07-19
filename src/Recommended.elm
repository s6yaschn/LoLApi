module Recommended
    exposing
        ( Model
        , empty
        , isEmpty
        , decoder
        , blocks
        , map
        , mode
        , title
        , typ'
        , view
        , viewLoc
        )

import Html exposing (..)
import Core exposing (..)
import Block
import Item
import Json.Decode exposing (..)
import List
import Realm
import Json.Decode.Extra exposing (..)
import Dict exposing (Dict)

-- MODEL


type alias Recommended =
    { blocks : List Block.Model
    , champion : String
    , map : String
    , mode : String
    , priority : Bool
    , title : String
    , typ :
        String
        -- originally type
    }


type Model
    = Model Recommended
    | Empty


empty : Model
empty =
    Empty


isEmpty : Model -> Bool
isEmpty m =
    case m of
        Empty ->
            True

        _ ->
            False


recommended : Decoder Recommended
recommended =
    succeed Recommended
        |: ("blocks" := list Block.decoder)
        |: ("champion" := string)
        |: ("map" := string)
        |: ("mode" := string)
        |: withDefault False ("priority" := bool)
        |: ("title" := string)
        |: ("type" := string)


decoder : Decoder Model
decoder =
    Json.Decode.map Model recommended

 

-- ACCESSORS


blocks : Model -> Result String (List Block.Model)
blocks m =
    case m of
        Empty ->
            emptyModelError "Recommended.blocks"

        Model { blocks } ->
            Ok blocks


map : Model -> Result String String
map m =
    case m of
        Empty ->
            emptyModelError "Recommended.map"

        Model { map } ->
            Ok map


mode : Model -> Result String String
mode m =
    case m of
        Empty ->
            emptyModelError "Recommended.mode"

        Model { mode } ->
            Ok mode


title : Model -> Result String String
title m =
    case m of
        Empty ->
            emptyModelError "Recommended.title"

        Model { title } ->
            Ok title


typ' : Model -> Result String String
typ' m =
    case m of
        Empty ->
            emptyModelError "Recommended.typ'"

        Model { typ } ->
            Ok typ



-- VIEW


view : Realm.Model -> Model -> Html a
view realm m =
    case m of
        Empty ->
            text ""

        Model recommended ->
            if Realm.isEmpty realm then
                text ""
            else
                let
                    f : Block.Model -> Html a
                    f x =
                        span [] <|
                            [ h4 [] [ text <| Result.withDefault "" (Block.typ' x) ]
                            ]
                                ++ List.map (Item.icon realm) (Result.withDefault [] (Block.items x))
                in
                    span [] <|
                        h3 [] [ text recommended.title ]
                            :: List.map f recommended.blocks

viewLoc : Realm.Model -> Dict String String -> Model -> Html a 
viewLoc realm languageStrings m =
     case m of
        Empty ->
            text ""

        Model recommended ->
            if Realm.isEmpty realm then
                text ""
            else
                let
                    translate str = Maybe.withDefault str <| Dict.get ("recommended_" ++ str) languageStrings

                    f : Block.Model -> Html a
                    f x =
                        span [] <|
                            [ h4 [] [ text <| translate <| Result.withDefault "" (Block.typ' x)] 
                            ]
                                ++ List.map (Item.icon realm) (Result.withDefault [] (Block.items x))
                in
                    span [] <| List.map f recommended.blocks    