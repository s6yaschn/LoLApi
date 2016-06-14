module Static exposing (..)

import Json.Decode as Json
import Region
import Http
import Html
import Html.App
import Task
import Config exposing (key)
import Core exposing (..)

import Debug exposing (log)

main = Html.App.program
    { init = ("", Task.perform toString toString (getAllChampions Region.euw))
    , view = Html.text
    , update = \x -> \y -> (x, Cmd.none)
    , subscriptions =\x -> Sub.none
     }
 

-- CONSTANTS 

version: String
version = "v1.2"




-- REQUESTS

getAllChampions : Region.Endpoint -> Request ChampionList
getAllChampions endpoint =
    request endpoint "champion?champData=all" championList

getChampionById : Region.Endpoint -> Int -> Request Champion 
getChampionById endpoint id =
    request endpoint ("champion/" ++ toString id ++ "?champData=all") champion
 



-- HTTP

request: Region.Endpoint -> String -> Json.Decoder a -> Request a
request endpoint request decoder = 
    let
        reg = Region.region endpoint
        url = "https://global.api.pvp.net/api/lol/static-data/"
            ++ reg ++ "/"
            ++ version ++ "/"
            ++ request
            ++ "&api_key=" ++ key       
    in
        Http.get decoder (log url url)
