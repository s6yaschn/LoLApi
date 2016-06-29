module Item
    exposing
        ( Model
        , empty
        , isEmpty
        , decoder
        , count
        , id
        , icon
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Core exposing (..)
import Json.Decode exposing (..)
import Realm
import Json.Decode.Extra exposing (..)


-- MODEL


type alias BlockItem =
    { count : Int
    , id : Int
    }


type Model
    = Model BlockItem
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


blockItem : Decoder BlockItem
blockItem =
    succeed BlockItem
        |: ("count" := int)
        |: ("id" := int)


decoder : Decoder Model
decoder =
   map Model blockItem



-- ACCESSORS


count : Model -> Result String Int
count m =
    case m of
        Empty ->
            emptyModelError "Item.count"

        Model { count } ->
            Ok count


id : Model -> Result String Int
id m =
    case m of
        Empty ->
            emptyModelError "Item.id"

        Model { id } ->
            Ok id



-- VIEW


icon : Realm.Model -> Model -> Html msg
icon realm m =
    case m of
        Empty ->
            text ""

        Model item ->
            if Realm.isEmpty realm then
                text ""
            else
                img
                    [ src <|
                        ddragon
                            ++ "/"
                            ++ Result.withDefault "" (Realm.version realm)
                            ++ "/img/item/"
                            ++ toString item.id
                            ++ ".png"
                    , alt ""
                    ]
                    []
