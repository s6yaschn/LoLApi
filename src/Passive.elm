module Passive
    exposing
        ( Model
        , empty
        , isEmpty
        , decoder
        , description
        , name
        , icon
        )

import Html exposing (img, Html, div, button, text)
import Html.Attributes exposing (src, alt)
import Core exposing (..)
import Image
import Json.Decode exposing (..)
import Image
import Realm
import Result


-- MODEL


type alias Passive =
    { description : String
    , image : Image.Model
    , name : String
    , sanitizedDescription : String
    }


type Model
    = Model Passive
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


passive : Decoder Passive
passive =
    Passive
        <$> "description"
        := string
        <+> "image"
        := Image.decoder
        <+> "name"
        := string
        <+> "sanitizedDescription"
        := string


decoder : Decoder Model
decoder =
    map Model passive



-- ACCESSORS


description : Model -> Result String String
description m =
    case m of
        Empty ->
            emptyModelError "Passive.description"

        Model { sanitizedDescription } ->
            Ok sanitizedDescription


name : Model -> Result String String
name m =
    case m of
        Empty ->
            emptyModelError "Passive.name"

        Model { name } ->
            Ok name



-- VIEW


icon : Realm.Model -> Model -> Html a
icon realm m =
    case m of
        Empty ->
            text ""

        Model passive ->
            if Realm.isEmpty realm then
                text ""
            else
                img
                    [ src
                        <|
                            ddragon
                        ++
                            "/"
                        ++
                            Result.withDefault "" (Realm.version realm)
                        -- should never default
                        ++
                            "/img/passive/"
                        ++
                            Result.withDefault "" (Image.full passive.image)
                    , alt passive.name
                    ]
                    []
