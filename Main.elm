module Main exposing (..)

import Champion
import ChampionList
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
import Dict
import Debug
import Result exposing (andThen)
import List

main = Html.App.program 
    { init = init 
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


type alias Model =
  { static: Static.Model
  , champion: Champion.Model
  , all: ChampionList.Model
  }


-- UPDATE
 
type Msg 
    = NewKey String
    | Search String
    | Fail Http.Error
    | Succeed Champion.Model
    | Init ChampionList.Model

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of 
        NewKey key ->
            ({model| static = Static.new Region.euw key}, Task.perform Fail Init <| Static.getAllChampions <| Static.new Region.euw key)
        Search s ->
            let
                id = Result.withDefault -1 <| String.toInt s 
            in 
                (model, Task.perform Fail Succeed <| Static.getChampionById model.static id)
        Fail _ -> 
            (model, Cmd.none) 
        Succeed champ ->
            ({model| champion = champ}, Cmd.none)
        Init new ->
            let 
                old = model.all
            in 
                if ChampionList.isEmpty old 
                    then ({model| all = new}, Task.perform Fail Succeed <| Static.getChampionById model.static 1)
                    else (model, Cmd.none)


-- VIEW 

view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "enter API key"
                , onInput NewKey
                ] []
        , if ChampionList.isEmpty model.all 
            then div [] []
            else viewSelect model
        , br [] []
        , viewChampion model
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- INIT

init : (Model, Cmd Msg)
init = ({ static = Static.new Region.euw ""
        , champion = Champion.empty
        , all = ChampionList.empty} , Cmd.none)

-- helper functions
 
viewSelect : Model -> Html Msg
viewSelect {all} =
    let 
        keys = Result.withDefault Dict.empty <| ChampionList.keys all
    in 
        select [ onInput Search]
            <| List.map (\(x, y) -> option [value x] [text y]) <| Dict.toList keys

viewChampion : Model -> Html Msg
viewChampion {champion} = -- TODO: reduce boilerplate
    Result.withDefault (div [] []) 
    <| andThen (Champion.allytips champion)
    <| \allytips -> andThen (Champion.enemytips champion) 
    <| \enemytips -> andThen (Champion.id champion)
    <| \id -> andThen (Champion.info champion) 
    <| \info -> andThen (Champion.lore champion) 
    <| \lore -> andThen (Champion.name champion) 
    <| \name -> andThen (Champion.passive champion)
    <| \passive -> andThen (Champion.recommended champion) 
    <| \recommended -> andThen (Champion.skins champion) 
    <| \skins -> andThen (Champion.spells champion) 
    <| \spells -> andThen (Champion.stats champion) 
    <| \stats -> andThen (Champion.title champion) 
    <| \title -> Ok 
    <| div [] [ div [align "center"] [ h1 [] [ text name]
                          , h3 [] [ text title]]
                 , text lore]