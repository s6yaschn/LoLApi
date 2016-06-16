module LevelTip exposing (..)


import Json.Decode exposing (..)
import Core exposing (..)

-- MODEL

type alias LevelTip =
    { effect: List String
    , label: List String
    }

type Model = Model LevelTip

levelTip : Decoder LevelTip
levelTip =
    LevelTip
    <$> "effect":= list string
    <+> "label":= list string

decoder : Decoder Model 
decoder = map Model levelTip 

emptyLevelTip: LevelTip
emptyLevelTip = LevelTip [] []

-- ACCESSORS

effect : Model -> List String 
effect (Model tip) = tip.effect

label : Model -> List String
label (Model tip) = tip.label



-- INIT

init : (Model, Cmd msg)
init = (Model emptyLevelTip, Cmd.none)