module Main exposing (..)

import Champion
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App

main = Html.App.program 
    { init = init 
    , update = update
    , view = view
    , subscriptions = subscriptions
    }




type alias Model =
  { key: Key.Model
  , champion: Champion.Model
  }


-- UPDATE
 
type Msg 
    = NewKey Key.Model

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of 
        NewKey newKey ->
            ({model| key = newKey}, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "enter API key"
                , onInput (NewKey << Key.new)
                ] []
        , input [ placeholder "enter Champion ID"
                , onInput ]
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- INIT

init : (Model, Cmd Msg)
init = ({ key = Key.init}, Cmd.none)