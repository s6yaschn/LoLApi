module LevelTip
    exposing
        ( Model
        , empty
        , isEmpty
        , decoder
        , effect
        , label
        )

import Json.Decode exposing (..)
import Core exposing (..)
import Json.Decode.Extra exposing (..)


-- MODEL


type alias LevelTip =
    { effect : List String
    , label : List String
    }


type Model
    = Model LevelTip
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


levelTip : Decoder LevelTip
levelTip =
    succeed LevelTip
        |: ("effect" := list string)
        |: ("label" := list string)


decoder : Decoder Model
decoder =
    map Model levelTip



-- ACCESSORS


effect : Model -> Result String (List String)
effect m =
    case m of
        Empty ->
            emptyModelError "LevelTip.effect"

        Model { effect } ->
            Ok effect


label : Model -> Result String (List String)
label m =
    case m of
        Empty ->
            emptyModelError "LevelTip.label"

        Model { label } ->
            Ok label
