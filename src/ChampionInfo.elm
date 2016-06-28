module ChampionInfo
    exposing
        ( Model
        , empty
        , isEmpty
        , decoder
        , attack
        , defense
        , difficulty
        , magic
        )

import Json.Decode exposing (..)
import Core exposing (..)


-- MODEL


type alias Info =
    { attack : Int
    , defense : Int
    , difficulty : Int
    , magic : Int
    }


type Model
    = Model Info
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


info : Decoder Info
info =
    Info
        <$> "attack"
        := int
        <+> "defense"
        := int
        <+> "difficulty"
        := int
        <+> "magic"
        := int


decoder : Decoder Model
decoder =
    map Model info



-- ACCESSORS


attack : Model -> Result String Int
attack m =
    case m of
        Empty ->
            emptyModelError "ChampionInfo.attack"

        Model { attack } ->
            Ok attack


defense : Model -> Result String Int
defense m =
    case m of
        Empty ->
            emptyModelError "ChampionInfo.defense"

        Model { defense } ->
            Ok defense


difficulty : Model -> Result String Int
difficulty m =
    case m of
        Empty ->
            emptyModelError "ChampionInfo.difficulty"

        Model { difficulty } ->
            Ok difficulty


magic : Model -> Result String Int
magic m =
    case m of
        Empty ->
            emptyModelError "ChampionInfo.magic"

        Model { magic } ->
            Ok magic
