module Passive exposing (Msg, update, icon, init)

import Html exposing (img, Html, div, button, text)
import Html.Attributes exposing (src, alt)
import Core exposing (ddragon, Passive, Image, passive, emptyPassive, emptyImage)
import Html.App exposing (program)
import Html.Events exposing (onClick)
import Version


main = program
    { init = init 
    , subscriptions = subscriptions
    , update = update
    , view = view 
    }


-- MODEL

type alias Model =
  { passive: Passive
  }


-- UPDATE

type Msg
    = NewPassive Passive

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewPassive newPassive ->
            (Model newPassive, Cmd.none)


-- VIEW

icon : Model -> Html Msg
icon model =
    img [src <| ddragon ++ "/" ++ Version.getVersion Version.testVersion ++ "/img/passive/"
            ++ model.passive.image.full
        , alt model.passive.name] []

view : Model -> Html Msg
view model =
    div [] 
    [ Html.button [onClick <| NewPassive {emptyPassive| image = {emptyImage| full = "Anivia_P.png"}}] [text "Click me"]
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