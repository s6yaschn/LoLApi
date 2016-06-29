module Realm
    exposing
        ( Model
        , empty
        , isEmpty
        , decoder
        , cdn
        , version
        )

import Json.Decode exposing (..)
import Dict exposing (Dict)
import Core exposing (..)
import Json.Decode.Extra exposing (..)


-- MODEL


type alias Realm =
    { cdn : String
    , css : String
    , dd : String
    , l : String
    , lg : String
    , n : Dict String String
    , profileiconmax : Int
    , store : String
    , v : String
    }


type Model
    = Model Realm
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


realm : Decoder Realm
realm =
    succeed Realm
        |: ("cdn" := string)
        |: ("css" := string)
        |: ("dd" := string)
        |: ("l" := string)
        |: ("lg" := string)
        |: ("n" := dict string)
        |: ("profileiconmax" := int)
        |: withDefault "" ("store" := string)
        |: ("v" := string)


decoder : Decoder Model
decoder =
    map Model realm



-- ACCESSORS


cdn : Model -> Result String String
cdn m =
    case m of
        Empty ->
            emptyModelError "Realm.cdn"

        Model { cdn } ->
            Ok cdn


version : Model -> Result String String
version m =
    case m of
        Empty ->
            emptyModelError "Realm.version"

        Model { dd } ->
            Ok dd
