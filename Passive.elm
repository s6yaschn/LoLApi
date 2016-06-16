module Passive exposing (..)

import Html exposing (img, Html, div, button, text)
import Html.Attributes exposing (src, alt)
import Core exposing (..)
import Html.App exposing (program)
import Html.Events exposing (onClick) 
import Version
import Image
import Json.Decode exposing (..)
import Image

main = program
    { init = init 
    , subscriptions = subscriptions
    , update = update
    , view = view 
    }


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

emptyPassive : Passive
emptyPassive = Passive "" (fst Image.init) "" ""


-- UPDATE

type Msg = NewPassive Model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewPassive newPassive -> 
            (newPassive, Cmd.none)


-- VIEW

icon : Model -> Html Msg
icon (Model passive) =
    img [src <| ddragon ++ "/" ++ Version.getVersion Version.testVersion ++ "/img/passive/"
            ++ Image.full passive.image
        , alt passive.name] []

view : Model -> Html Msg
view model =
    div [] 
    [ Html.button [onClick <| NewPassive <| Model emptyPassive] [text "Click me"]
    , icon model
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- INIT

init : (Model, Cmd Msg)
init =
    (Model emptyPassive, Cmd.none )