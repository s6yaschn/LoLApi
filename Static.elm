module Static exposing (..)

import Json.Decode as Json exposing ((:=))
import Region
import Http
import Core exposing (..)
import String exposing (contains)
import Debug exposing (log)
import Champion
import Realm 
import ChampionList

-- CONSTANTS 

version: String
version = "v1.2" 
 


type alias Static =
    { endpoint: Region.Endpoint
    , key: String 
    }

 
type Model = Model Static


new : Region.Endpoint -> String -> Model 
new reg key = Model <| Static reg key
 

-- REQUESTS

getRealm : Model -> Request Realm.Model
getRealm model = 
    request model Realm.decoder "realm"

getChampionById : Model -> Int -> Request Champion.Model
getChampionById model id =
    request model Champion.decoder <| "champion/" ++ toString id ++ "?champData=all" 

getAllChampions : Model -> Request ChampionList.Model
getAllChampions model =
    request model ChampionList.decoder "champion?champData=all" 

-- HTTP 

request: Model -> Json.Decoder a -> String -> Request a
request (Model {endpoint, key}) decoder request = 
    let
        reg = Region.region endpoint
        url = "https://global.api.pvp.net/api/lol/static-data/"
            ++ reg ++ "/"
            ++ version ++ "/"
            ++ request
            ++ if contains "?" request
                then "&api_key=" ++ key 
                else "?api_key=" ++ key       
    in 
        Http.get decoder (log url url) -- Debug