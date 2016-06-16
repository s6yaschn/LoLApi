module Block exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Core exposing (..)
import Version exposing (getVersion)
import Item
import Version exposing (testVersion)


main = Html.program
    { init = init 
    , update = update
    , subscriptions = subscriptions
    , view = view' 
    }


-- MODEL

type alias Model =
  { block: Block
  }


-- UPDATE

type Msg = NewBlock Block

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        NewBlock newBlock ->
            (Model newBlock, Cmd.none)


-- VIEW

view : Block -> Html msg
view block =
    let
        items = block.items 
    in 
        div [] <| List.map (Item.icon testVersion) items

view' : Model -> Html Msg
view' model = view model.block

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- INIT

init : (Model, Cmd Msg)
init = (Model {emptyBlock| items = 
    [ {emptyItem| id = 1001}
    , {emptyItem| id = 1041}]}, Cmd.none)
