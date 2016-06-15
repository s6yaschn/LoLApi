module ChampionSpell exposing (..)


import Core exposing (ChampionSpell, ddragon, Image, emptySpell, emptyImage)
import Html exposing (Html, img, text, div, button)
import Html.App exposing (program)
import Html.Attributes exposing (src, alt)
import Version exposing (getVersion, testVersion)
import Html.Events exposing (onClick)

main = program 
  { init = init 
  , update = update
  , view = view
  , subscriptions = subscriptions
  }



-- MODEL

type alias Model = 
  { spell: ChampionSpell
  }


-- UPDATE

type Msg = NewSpell ChampionSpell

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewSpell newSpell ->
      (Model newSpell, Cmd.none)

 

-- VIEW

icon : Version.Model -> Model -> Html Msg
icon version model =
  img [src <| ddragon 
        ++ "/" ++ getVersion version
        ++ "/img/spell/" 
        ++ model.spell.image.full
      , alt model.spell.name] []


view : Model -> Html Msg
view model =
  div [] 
    [ button [onClick <| NewSpell {emptySpell| image = {emptyImage| full = "FlashFrost.png"}, name = "Flash Frost"}] [text "Click me"]
    , icon testVersion model]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- INIT

init : (Model, Cmd Msg)
init =
  (Model {emptySpell| image = {emptyImage| full = "Disintegrate.png"}, name = "Disintegrate"}, Cmd.none)