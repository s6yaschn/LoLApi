module SpellVars exposing (..)

import Json.Decode exposing (..)
import Core exposing (..)

-- MODEL

type alias SpellVars =
    { coeff: List Float
    , dyn: String
    , key: String
    , link: String
    , ranksWith: String
    }
 
type Model = Model SpellVars
 
spellVars : Decoder SpellVars
spellVars = 
    SpellVars
    <$> "coeff" := list float
    <+> oneOf ["dyn" := string, succeed ""] -- optional
    <+> "key" := string
    <+> "link" := string
    <+> oneOf ["ranksWith" := string, succeed ""] -- optional

decoder : Decoder Model
decoder = map Model spellVars