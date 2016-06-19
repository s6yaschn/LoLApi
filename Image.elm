module Image exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Core exposing (..)
import Json.Decode exposing (..)
import Realm

-- MODEL


type Model = Model Image


type alias Image =
    { full: String
    , group: String
    , h: Int
    , sprite: String
    , w: Int
    , x: Int
    , y: Int
    }


image : Decoder Image 
image =  
    Image
    <$> "full" := string
    <+> "group" := string
    <+> "h" := int
    <+> "sprite" := string
    <+> "w" := int
    <+> "x" := int
    <+> "y" := int

decoder : Decoder Model
decoder = map Model image

-- ACCESSORS


full : Model -> String
full (Model img) = img.full

group : Model -> String 
group (Model img) = img.group

sprite : Model -> String
sprite (Model img) = img.sprite

size : Model -> (Int, Int)
size (Model img) = (img.w, img.h)

position : Model -> (Int, Int)
position (Model img) = (img.x, img.y)

-- VIEW

-- load from Sprite
view : Realm.Model -> Model -> Html msg
view realm (Model img) =
    let 
        url =  ddragon ++ "/" ++ Realm.version realm ++ "/img/sprite/" ++ img.sprite
        pos = toString (negate img.x) ++ "px " ++ toString (negate img.y) ++ "px"
    in 
        canvas 
            [ width img.w
            , height img.h
            , alt img.full
            , style 
                [ ("background-image", "url('" ++ url ++ "') ")
                , ("background-position", pos)]
            ] []