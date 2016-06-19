module Item exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Core exposing (..)
import Json.Decode exposing (..)
import Realm

-- MODEL

type alias BlockItem = 
    { count: Int
    , id: Int
    }

type Model = Model BlockItem

blockItem : Decoder BlockItem 
blockItem = 
    BlockItem 
    <$> "count" := int
    <+> "id" := int 

decoder : Decoder Model
decoder = Model <$> blockItem 

-- VIEW

icon : Realm.Model -> Model -> Html msg
icon realm (Model item) =
    img [src <| ddragon ++ "/"
            ++ Realm.version realm 
            ++ "/img/item/"
            ++ toString item.id
            ++ ".png"
        , alt ""] []
