module ChampionInfo exposing (..)

import Json.Decode exposing (..)
import Core exposing (..)

-- MODEL

type alias Info = 
    { attack: Int
    , defense: Int
    , difficulty: Int
    , magic: Int
    }

type Model = Model Info


info : Decoder Info
info =
    Info
    <$> "attack" := int
    <+> "defense" := int
    <+> "difficulty" := int
    <+> "magic" := int

  
decoder : Decoder Model
decoder = map Model info


-- ACCESSORS