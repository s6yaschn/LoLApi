module Static exposing (..)

import Json.Decode as Json exposing ((:=))
import Region
import Http
import Task
import Dict exposing (Dict)
import Config exposing (key)


version: String
version = "v1.2"


request: Region.Endpoint -> String -> Cmd (Result Http.Error (Dict String Json.Value))
request endpoint request = 
    let
        reg = Region.region endpoint
        url = "https://global.api.pvp.net/api/lol/static-data/"
            ++ reg ++ "/"
            ++ version ++ "/"
            ++ request
            ++ "&api_key=" ++ key
        t = Http.get ("data" := Json.dict (Json.value)) "https://global.api.pvp.net/api/lol/static-data/euw/v1.2/champion?champData=all&api_key=b7d8810b-de94-4ea5-8dc2-8e84acc05171"
    in
        Task.perform Err Ok t 

