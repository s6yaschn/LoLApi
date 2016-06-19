module Passive exposing (..)

import Html exposing (img, Html, div, button, text)
import Html.Attributes exposing (src, alt)
import Core exposing (..)
import Image
import Json.Decode exposing (..)
import Image
import Realm


-- MODEL

type alias Passive =
    { description: String
    , image: Image.Model 
    , name: String
    , sanitizedDescription: String
    }

type Model = Model Passive

passive : Decoder Passive
passive =
    Passive
    <$> "description" := string
    <+> "image" := Image.decoder
    <+> "name" := string
    <+> "sanitizedDescription" := string

decoder : Decoder Model
decoder = map Model passive

-- VIEW

icon : Realm.Model -> Model -> Html a
icon realm (Model passive) =
    img [src <| ddragon ++ "/" ++ Realm.version realm ++ "/img/passive/"
            ++ Image.full passive.image
        , alt passive.name] []