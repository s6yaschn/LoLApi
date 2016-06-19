module Spell exposing (..)


import Core exposing (..)
import Html exposing (Html, img, text, div, button)
import Html.Attributes exposing (src, alt)
import Realm
import Image
import LevelTip
import SpellVars
import Json.Decode exposing (..) 
 
-- MODEL

type alias ChampionSpell =
    { altimages: List Image.Model
    , cooldown: List Float 
    , cooldownBurn: String
    , cost: List Int
    , costBurn: String
    , costType: String
    , description: String
    , effect: List (List Float)
    , effectBurn: List String
    , image: Image.Model 
    , key: String
    , leveltip: LevelTip.Model
    , maxrank: Int
    , name: String
    , range: Result String (List Int) -- either List of Ints or "self"
    , rangeBurn: String
    , resource: String
    , sanitizedDescription: String
    , sanitizedTooltip: String
    , tooltip: String
    , vars: List SpellVars.Model
    }


type Model = Model ChampionSpell


championSpell : Decoder ChampionSpell
championSpell =
    ChampionSpell
    <$> oneOf ["altimages" := list Image.decoder, succeed []] -- optional
    <+> "cooldown" := list float
    <+> "cooldownBurn" := string
    <+> "cost" := list int
    <+> "costBurn" := string
    <+> "costType" := string
    <+> "description" := string
    <+> "effect" := list (oneOf [null [-1], (list float)])  -- 1-indexed arrays, ignore the first null and insert -1
    <+> "effectBurn" := list string
    <+> "image" := Image.decoder
    <+> "key" := string
    <+> "leveltip" := LevelTip.decoder
    <+> "maxrank" := int
    <+> "name" := string
    <+> "range" := oneOf [Err <$> string, Ok <$> list int]
    <+> "rangeBurn" := string
    <+> oneOf ["resource" := string, succeed ""] -- optional
    <+> "sanitizedDescription" := string
    <+> "sanitizedTooltip" := string
    <+> "tooltip" := string
    <+> oneOf ["vars" := list SpellVars.decoder, succeed []] -- optional

decoder : Decoder Model
decoder = map Model championSpell


-- VIEW

icon : Realm.Model -> Model -> Html a
icon realm (Model spell) =
  img [src <| ddragon 
        ++ "/" ++ Realm.version realm
        ++ "/img/spell/" 
        ++ Image.full spell.image
      , alt spell.name] []
