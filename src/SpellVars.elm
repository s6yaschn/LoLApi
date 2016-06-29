module SpellVars
    exposing
        ( Model
        , empty
        , isEmpty
        , decoder
        )

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)


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


spellVars : Decoder SpellVars
spellVars =
    succeed SpellVars
        |: ("coeff" := list float)
        |: withDefault "" ("dyn" := string)
        |: ("key" := string)
        |: ("link" := string)
        |: withDefault "" ("ranksWith" := string)


decoder : Decoder Model
decoder =
    map Model spellVars
