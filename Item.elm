module Item exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Core exposing (..)
import Version exposing (getVersion)
import Json.Decode exposing (..)

main = Html.program
    { init = init 
    , update = update
    , subscriptions = subscriptions
    , view = view 
    }

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

emptyItem: BlockItem
emptyItem = BlockItem 0 0
 
-- UPDATE

type Msg = NewItem Model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        NewItem newItem ->
            (newItem, Cmd.none)


-- VIEW

icon : Version.Model -> Model -> Html msg
icon version (Model item) =
    img [src <| ddragon ++ "/"
            ++ getVersion version 
            ++ "/img/item/"
            ++ toString item.id
            ++ ".png"
        , alt ""] []


view : Model -> Html Msg
view model =
    div []
        [ button [onClick <| NewItem <| Model {emptyItem| id = 1041}] [text "Click Me"]
        , icon Version.testVersion model]
 


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- INIT

init : (Model, Cmd Msg)
init =
    (Model {emptyItem| id=1001}, Cmd.none)