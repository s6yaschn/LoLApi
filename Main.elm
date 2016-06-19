module Main exposing (..)

import Champion
import Static
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Region
import String
import Result
import Task
import Http

main = Html.App.program 
    { init = init 
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


type alias Model =
  { static: Static.Model
  , champion: Champion.Model
  }


-- UPDATE
 
type Msg 
    = NewKey String
    | Search String
    | Fail Http.Error
    | Succeed Champion.Model

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of 
        NewKey key ->
            ({model| static = Static.new Region.euw key}, Cmd.none)
        Search s ->
            let
                id = Result.withDefault -1 <| String.toInt s 
            in 
                (model, Task.perform Fail Succeed <| Static.getChampionById model.static id)
        Fail _ -> 
            (model, Cmd.none) 
        Succeed champ ->
            ({model| champion = champ}, Cmd.none)




-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "enter API key"
                , onInput NewKey
                ] []
        , input [ placeholder "enter Champion ID"
                , onInput Search
                ] []
        , br [] []
        , Champion.splashArt model.champion
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- INIT

init : (Model, Cmd Msg)
init = ({static = Static.new Region.euw "", champion = Champion.empty} , Cmd.none)