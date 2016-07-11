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
import Dict exposing (Dict)
import SummonerSpell
import SummonerSpellList


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


endpoint : Model -> Endpoint.Model
endpoint (Model { endpoint }) =
    endpoint


updateEndpoint : Model -> Endpoint.Model -> Model
updateEndpoint (Model model) new =
    Model { model | endpoint = new }



-- REQUESTS


getRealm : Model -> Task Http.Error Realm.Model
getRealm model =
    request model Realm.decoder "realm"


getChampionById : Model -> Int -> Task Http.Error Champion.Model
getChampionById model id =
    request model Champion.decoder <| "champion/" ++ toString id ++ "?champData=all"


getChampionByIdLoc : String -> Model -> Int -> Task Http.Error Champion.Model
getChampionByIdLoc lang model id =
    request model Champion.decoder <| "champion/" ++ toString id ++ "?champData=all&locale=" ++ lang


getAllChampions : Model -> Task Http.Error ChampionList.Model
getAllChampions model =
    request model ChampionList.decoder "champion?champData=all"


getAllChampionsLoc : String -> Model -> Task Http.Error ChampionList.Model
getAllChampionsLoc lang model =
    request model ChampionList.decoder <| "champion?champData=all&locale=" ++ lang


getLanguages : Model -> Task Http.Error (List String)
getLanguages model =
    request model (Json.list Json.string) "languages"


getLanguageStrings : Model -> String -> Task Http.Error (Dict String String)
getLanguageStrings model lang =
    request model ("data" := Json.dict Json.string) <| "language-strings?locale=" ++ lang


getSummonerSpellById : Model -> Int -> Task Http.Error SummonerSpell.Model
getSummonerSpellById model id =
    request model SummonerSpell.decoder <| "summoner-spell/" ++ toString id ++ "?spellData=all"


getSummonerSpellByIdLoc : String -> Model -> Int -> Task Http.Error SummonerSpell.Model
getSummonerSpellByIdLoc lang model id =
    request model SummonerSpell.decoder <| "summoner-spell/" ++ toString id ++ "?spellData=all&locale=" ++ lang


getAllSummonerSpells : Model -> Task Http.Error SummonerSpellList.Model
getAllSummonerSpells model =
    request model SummonerSpellList.decoder "summoner-spell?spellData=all"


getAllSummonerSpellsLoc : String -> Model -> Task Http.Error SummonerSpellList.Model
getAllSummonerSpellsLoc lang model =
    request model SummonerSpellList.decoder <| "summoner-spell?spellData=all&locale=" ++ lang



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
        Http.get decoder (log "Request" url)
