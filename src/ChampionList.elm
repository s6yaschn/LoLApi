module ChampionList
    exposing
        ( Model
        , empty
        , isEmpty
        , decoder
        , data
        , keys
        , ids
        )

import Core exposing (..)
import Champion
import Json.Decode exposing (..)
import Dict exposing (Dict)
import List
import Json.Decode.Extra exposing (..)


-- MODEL


type alias ChampionList =
    { data : Dict String Champion.Model
    , format : String
    , keys : Dict Int String
    , typ :
        String
        -- originally type
    , version : String
    }


type Model
    = Model ChampionList
    | Empty


championList : Decoder ChampionList
championList =
    succeed ChampionList
        |: ("data" := dict Champion.decoder)
        |: ("format" := string)
        |: ("keys" := dict2 int string)
        |: ("type" := string)
        |: ("version" := string)


decoder : Decoder Model
decoder =
    map Model championList


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


data : Model -> Result String (Dict String Champion.Model)
data m =
    case m of
        Empty ->
            emptyModelError "ChampionList.data"

        Model { data } ->
            Ok data



keys : Model -> Result String (Dict Int String)
keys m =
    case m of
        Empty ->
            emptyModelError "ChampionList.keys"

        Model { keys } ->
            Ok keys


ids : Model -> Result String (Dict String Int)
ids m =
    case m of
        Empty ->
            emptyModelError "ChampionList.ids"

        Model { keys } ->
            let
                switch =
                    \( a, b ) -> ( b, a )
            in
                keys |> Dict.toList |> List.map switch |> Dict.fromList |> Ok
