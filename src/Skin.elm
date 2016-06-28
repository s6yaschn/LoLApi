module Skin
    exposing
        ( Model
        , empty
        , isEmpty
        , decoder
        , id
        , name
        , num
        )

import Json.Decode exposing (..)
import Core exposing (..)


-- MODEL


type alias Skin =
    { id : Int
    , name : String
    , num : Int
    }


type Model
    = Model Skin
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


skin : Decoder Skin
skin =
    Skin
        <$> "id"
        := int
        <+> "name"
        := string
        <+> "num"
        := int


decoder : Decoder Model
decoder =
    map Model skin



-- ACCESSORS


id : Model -> Result String Int
id m =
    case m of
        Empty ->
            emptyModelError "Skin.id"

        Model { id } ->
            Ok id


name : Model -> Result String String
name m =
    case m of
        Empty ->
            emptyModelError "Skin.id"

        Model { name } ->
            Ok name


num : Model -> Result String Int
num m =
    case m of
        Empty ->
            emptyModelError "Skin.num"

        Model { num } ->
            Ok num
