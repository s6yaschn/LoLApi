module SummonerSpellList exposing (..)

import Core exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import SummonerSpell
import Dict exposing (Dict)


type alias SummonerSpellList =
    { data : Dict String SummonerSpell.Model
    , typ' : String
    , version : String
    }


type Model
    = Model SummonerSpellList
    | Empty


summonerSpellList : Decoder SummonerSpellList
summonerSpellList =
    succeed SummonerSpellList
        |: ("data" := dict SummonerSpell.decoder)
        |: ("type" := string)
        |: ("version" := string)


decoder : Decoder Model
decoder =
    map Model summonerSpellList


empty : Model
empty =
    Empty


isEmpty : Model -> Bool
isEmpty m =
    case m of
        Empty ->
            True

        _ ->
            False

-- ACCESSORS

data : Model -> Result String (Dict String SummonerSpell.Model)
data m =
    case m of 
        Empty -> emptyModelError "SummonerSpellList.data"
        Model {data} -> Ok data

