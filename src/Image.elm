module Image
    exposing
        ( Model
        , empty
        , isEmpty
        , decoder
        , view
        , full
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Core exposing (..)
import Json.Decode exposing (..)
import Realm
import Json.Decode.Extra exposing (..)


-- MODEL


type Model
    = Model Image
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


type alias Image =
    { full : String
    , group : String
    , h : Int
    , sprite : String
    , w : Int
    , x : Int
    , y : Int
    }


image : Decoder Image
image =
    succeed Image
        |: ("full" := string)
        |: ("group" := string)
        |: ("h" := int)
        |: ("sprite" := string)
        |: ("w" := int)
        |: ("x" := int)
        |: ("y" := int)
 

decoder : Decoder Model
decoder =
    map Model image



-- ACCESSORS


full : Model -> Result String String
full m =
    case m of
        Empty ->
            emptyModelError "Image.full"

        Model { full } ->
            Ok full



{-
   group : Model -> Result String String
   group m =
       case m of
           Empty -> emptyModelError "Image.group"
           Model {group} -> Ok group


   sprite : Model -> String
   sprite (Model img) =
       img.sprite


   size : Model -> ( Int, Int )
   size (Model img) =
       ( img.w, img.h )


   position : Model -> ( Int, Int )
   position (Model img) =
       ( img.x, img.y )
-}
-- VIEW
-- load from Sprite


view : Realm.Model -> Model -> Html msg
view realm m =
    case m of
        Empty ->
            text ""

        Model img ->
            if Realm.isEmpty realm then
                text ""
            else
                let
                    url =
                        ddragon ++ "/" ++ Result.withDefault "" (Realm.version realm) ++ "/img/sprite/" ++ img.sprite

                    pos =
                        toString (negate img.x) ++ "px " ++ toString (negate img.y) ++ "px"
                in
                    canvas
                        [ width img.w
                        , height img.h
                        , alt img.full
                        , style
                            [ ( "background-image", "url('" ++ url ++ "') " )
                            , ( "background-position", pos )
                            ]
                        ]
                        []
