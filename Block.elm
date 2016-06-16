module Block exposing (..)

import Html exposing (..)
import Html.App as Html
import Core exposing (..)
import Version exposing (getVersion)
import Item
import Version exposing (testVersion)
import Json.Decode exposing (..)
import Item

main = Html.program
    { init = init 
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

 
-- MODEL 

type alias Block = 
    { items: List Item.Model
    , recMath: Bool
    , typ: String -- originally type
    }

type Model = Model Block

block : Decoder Block
block = 
    Block
    <$> "items" := list Item.decoder
    <+> oneOf ["recMath" := bool, succeed False] -- optional
    <+> oneOf ["type" := string, succeed ""] -- optional


decoder : Decoder Model
decoder = map Model block 

emptyBlock: Block
emptyBlock = Block [] False ""

-- ACCESSORS

items : Model -> List Item.Model
items (Model b) = b.items

typ : Model -> String 
typ (Model b) = b.typ

-- UPDATE 

type Msg = NewBlock Model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        NewBlock newBlock ->
            (newBlock, Cmd.none)


-- VIEW

view : Model -> Html msg
view (Model block) =
    let
        items = block.items 
    in 
        div [] <| List.map (Item.icon testVersion) items

-- SUBSCRIPTIONS 

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none 


-- INIT

init : (Model, Cmd Msg)
init = (Model emptyBlock, Cmd.none)
