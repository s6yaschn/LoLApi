module SpellVars exposing (Model,
 empty, isEmpty, decoder)

import Json.Decode exposing (..)
import Core exposing (..)


-- MODEL


type alias SpellVars =
    { coeff : List Float
    , dyn : String
    , key : String
    , link : String
    , ranksWith : String
    }


type Model
    = Model SpellVars
    | Empty

empty: Model
empty = Empty

isEmpty : Model -> Bool
isEmpty m = 
    case m of 
        Empty -> True 
        _ -> False


spellVars : Decoder SpellVars
spellVars =
    SpellVars
        <$>
            "coeff"
        :=
            list float
        <+>
            oneOf [ "dyn" := string, succeed "" ]
        -- optional
        <+>
            "key"
        :=
            string
        <+>
            "link"
        :=
            string
        <+> -- optional
            oneOf [ "ranksWith" := string, succeed "" ]






decoder : Decoder Model
decoder =
    map Model spellVars
