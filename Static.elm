module Static exposing (..)

import Json.Decode as Json exposing ((:=))
import Region
import Http
import Config exposing (key)
import Core exposing (..)
import String exposing (contains)
import Debug exposing (log)
 

-- CONSTANTS 

version: String
version = "v1.2"



 
-- REQUESTS


 
  

 

-- HTTP 

request: Region.Endpoint -> Json.Decoder a -> String -> Request a
request endpoint decoder request = 
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
 