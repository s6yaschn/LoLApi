module Block
    exposing
        ( Model
        , empty
        , isEmpty
        , decoder
        , items
        , typ'
        )

import Core exposing (..)
import Item
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)


-- MODEL


type alias Block =
    { items : List Item.Model
    , recMath : Bool
    , typ :
        String
        -- originally type
    }


type Model
    = Model Block
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


block : Decoder Block
block =
    succeed Block
        |: ("items" := list Item.decoder)
        |: withDefault False ("recMath" := bool)
        |: withDefault "" ("type" := string)


decoder : Decoder Model 
decoder =
    map Model block



-- ACCESSORS


items : Model -> Result String (List Item.Model)
items m =
    case m of
        Empty ->
            emptyModelError "Block.items"

        Model { items } ->
            Ok items


typ' : Model -> Result String String
typ' m =
    case m of
        Empty ->
            emptyModelError "Block.typ"

        Model { typ } ->
            Ok typ
