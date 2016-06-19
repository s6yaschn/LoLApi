module Block exposing (..)

import Core exposing (..)
import Item
import Json.Decode exposing (..)

 
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

-- ACCESSORS

items : Model -> List Item.Model
items (Model b) = b.items

typ : Model -> String 
typ (Model b) = b.typ