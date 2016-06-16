module Skin exposing (..)

import Json.Decode exposing (..)
import Core exposing (..)

-- MODEL

type alias Skin =
    { id: Int
    , name: String
    , num: Int
    }

type Model = Model Skin

skin : Decoder Skin
skin =
    Skin
    <$> "id" := int
    <+> "name" := string
    <+> "num" := int

decoder : Decoder Model
decoder = map Model skin

-- ACCESSORS

id : Model -> Int  
id (Model skin) = skin.id
 
name : Model -> String 
name (Model skin) = skin.name

num : Model -> Int
num (Model skin) = skin.num


-- UPDATE

type Msg = NewSkin Model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewSkin new ->
            (new, Cmd.none)