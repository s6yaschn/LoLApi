module ChampionList exposing (..)


import Core exposing (..)
import Champion 
import Json.Decode exposing (..)
import Dict exposing (Dict)
import List

-- MODEL 

type alias ChampionList =
    { data: Dict String Champion.Model
    , format: String 
    , keys: Dict String String 
    , typ: String -- originally type
    , version: String
    }

type Model 
    = Model ChampionList
    | Empty

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

empty : Model
empty = Empty

isEmpty : Model -> Bool
isEmpty m =
    case m of 
        Empty -> True
        _ -> False

-- ACCESSORS

data : Model -> Result String (Dict String Champion.Model)
data m =
    case m of 
        Empty -> emptyModelError "ChampionList.data"
        Model {data} -> Ok data

--types
keys : Model -> Result String (Dict String String) 
keys m = 
    case m of 
        Empty -> emptyModelError "ChampionList.keys"
        Model {keys} -> Ok keys


-- types
ids : Model -> Result String (Dict String String)
ids m =
    let 
        switch = \(a,b) -> (b,a)
    in 
        case m of 
            Empty -> emptyModelError "ChampionList.ids"
            Model {keys} -> keys |> Dict.toList |> List.map switch |> Dict.fromList |> Ok
