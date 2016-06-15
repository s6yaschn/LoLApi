module Item exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Core exposing (..)
import Version exposing (getVersion)

main = Html.program
    { init = init 
    , update = update
    , subscriptions = subscriptions
    , view = view 
    }

-- MODEL

type alias Model =
  { item: BlockItem
  }


-- UPDATE

type Msg = NewItem BlockItem

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        NewItem newItem ->
            (Model newItem, Cmd.none)


-- VIEW

icon : Version.Model -> Model -> Html Msg
icon version model =
    img [src <| ddragon ++ "/"
            ++ getVersion version 
            ++ "/img/item/"
            ++ toString model.item.id
            ++ ".png"
        , alt ""] []


view : Model -> Html Msg
view model =
    div []
        [ button [onClick <| NewItem {emptyItem| id = 1041}] [text "Click Me"]
        , icon Version.testVersion model]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- INIT

init : (Model, Cmd Msg)
init =
    (Model {emptyItem| id=1001}, Cmd.none)