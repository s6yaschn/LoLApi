module ChampionList exposing (..)


import Task
import Static
import Region
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Core exposing (..)


main = Html.program 
    { init = init 
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type alias Model =
  { championList: ChampionList
  }


-- UPDATE

type Msg 
    = NewChampionList ChampionList
    | Fetch

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewChampionList new -> 
            (Model new, Cmd.none)
        Fetch ->
            (model, Task.perform (always <| NewChampionList model.championList) NewChampionList <| Static.getAllChampions Region.euw)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

--  VIEW

view : Model -> Html Msg
view model =
    div []
        [ div [] [button [onClick Fetch] [text "Go Fetch!"]
        , div [] [text <| toString model.championList.keys]]]

-- INIT

init : (Model, Cmd Msg)
init = (Model emptyChampionList, Cmd.none)
