module ChampionList exposing (..)


import Core exposing (..)
import Champion 
import Json.Decode exposing (..)
import Dict exposing (Dict)

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



-- ACCESSORS

data : Model -> Dict String Champion.Model
data (Model cl) = cl.data

keys : Model -> Dict String String 
keys (Model cl) = cl.keys



