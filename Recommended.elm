module Recommended exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Core exposing (..)
import Version exposing (getVersion)
import Block

main = Html.program
    { init = init 
    , update = update
    , subscriptions = subscriptions
    , view = view 
    }


-- MODEL

type alias Model =
    { recommended: Recommended
    }


-- UPDATE

type Msg = NewRecommended Recommended

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewRecommended new -> 
          (Model new, Cmd.none)



-- VIEW

view : Model -> Html Msg
view model =
  let
    f: Block -> Html Msg
    f x = div [] [h3 [] [text x.typ], br [] [],  Block.view x ]
  in
    div []
     <| h1 [] [text model.recommended.typ] ::  List.map f model.recommended.blocks

 
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- INIT

init : (Model, Cmd Msg)
init = (Model {emptyRecommended|
              typ = "Recommended Items"
              , blocks = 
                [ {emptyBlock| typ = "Section 1", items= [BlockItem 1 1001, BlockItem 1 1041, BlockItem 1 1001]}
                , {emptyBlock| typ = "Section 2", items = [BlockItem 2 1041]}]}, Cmd.none)