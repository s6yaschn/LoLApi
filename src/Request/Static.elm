module Request.Static exposing (..)

import Json.Decode as Json exposing ((:=))
import Endpoint
import Http
import String exposing (contains)
import Debug exposing (log)
import Champion
import Realm
import ChampionList
import Task exposing (Task)


-- CONSTANTS


version : String
version =
    "v1.2"


type alias Static =
    { endpoint : Endpoint.Model
    , key : String
    }


type Model
    = Model Static


new : Endpoint.Model -> String -> Model
new reg key =
    Model <| Static reg key



-- REQUESTS


getRealm : Model -> Task Http.Error Realm.Model
getRealm model =
    request model Realm.decoder "realm"


getChampionById : Model -> Int -> Task Http.Error Champion.Model
getChampionById model id =
    request model Champion.decoder <| "champion/" ++ toString id ++ "?champData=all"


getAllChampions : Model -> Task Http.Error ChampionList.Model
getAllChampions model =
    request model ChampionList.decoder "champion?champData=all"



-- HTTP


request : Model -> Json.Decoder a -> String -> Task Http.Error a
request (Model { endpoint, key }) decoder request =
    let
        reg =
            Endpoint.region endpoint

        url =
            "https://global.api.pvp.net/api/lol/static-data/"
                ++ reg
                ++ "/"
                ++ version
                ++ "/"
                ++ request
                ++ if contains "?" request then
                    "&api_key=" ++ key
                   else
                    "?api_key=" ++ key
    in
        -- Debug
        Http.get decoder (log url url)
