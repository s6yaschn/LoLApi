module ChampionList exposing (..)


import Task
import Static
import Region
import Html exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import Core exposing (..)
import Champion 
import Json.Decode exposing (..)
import Dict exposing (Dict)

main = Html.program 
    { init = init 
    , update = update
    , subscriptions = subscriptions
    , view = view
    } 

-- MODEL 

type alias ChampionList =
    { data: Dict String Champion.Model
    , format: String 
    , keys: Dict String String 
    , typ: String -- originally type
    , version: String
    }

type Model = Model ChampionList

championList : Decoder ChampionList
championList = 
    ChampionList
    <$> "data" := dict Champion.decoder
    <+> "format" := string 
    <+> "keys" := dict string 
    <+> "type" := string 
    <+> "version" := string 

decoder : Decoder Model
decoder = map Model championList

emptyChampionList : ChampionList
emptyChampionList = ChampionList Dict.empty "" Dict.empty "" ""

-- ACCESSORS

data : Model -> Dict String Champion.Model
data (Model cl) = cl.data

keys : Model -> Dict String String 
keys (Model cl) = cl.keys

getAllChampions : Region.Endpoint -> Request Model
getAllChampions endpoint =
    Static.request endpoint decoder "champion?champData=all" 


-- UPDATE

type Msg 
    = NewChampionList Model
    | Fetch

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewChampionList new -> 
            (new, Cmd.none)
        Fetch ->
            (model, Task.perform (always <| NewChampionList model) NewChampionList <| getAllChampions Region.euw)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

--  VIEW

view : Model -> Html Msg
view (Model championList) =
    div []
        [ div [] [button [onClick Fetch] [text "Go Fetch!"]
        , div [] [text <| toString <| championList.keys]]]

-- INIT

init : (Model, Cmd Msg)
init = (Model emptyChampionList, Cmd.none) 
