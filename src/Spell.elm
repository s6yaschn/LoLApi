module Spell
    exposing
        ( Model
        , empty
        , isEmpty
        , decoder
        , description
        , icon
        )

import Core exposing (..)
import Html exposing (Html, img, text, div, button)
import Html.Attributes exposing (src, alt)
import Realm
import Image
import LevelTip
import SpellVars
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)


-- MODEL


type alias ChampionSpell =
    { altimages : List Image.Model
    , cooldown : List Float
    , cooldownBurn : String
    , cost : List Int
    , costBurn : String
    , costType : String
    , description : String
    , effect : List (List Float)
    , effectBurn : List String
    , image : Image.Model
    , key : String
    , leveltip : LevelTip.Model
    , maxrank : Int
    , name : String
    , range :
        Result String (List Int)
        -- either List of Ints or "self"
    , rangeBurn : String
    , resource : String
    , sanitizedDescription : String
    , sanitizedTooltip : String
    , tooltip : String
    , vars : List SpellVars.Model
    }


type Model
    = Model ChampionSpell
    | Empty


empty : Model
empty =
    Empty


isEmpty : Model -> Bool
isEmpty m =
    case m of
        Empty ->
            True

        _ ->
            False


championSpell : Decoder ChampionSpell
championSpell =
    succeed ChampionSpell
        |: withDefault [] ("altimages" := list Image.decoder)
        |: ("cooldown" := list float)
        |: ("cooldownBurn" := string)
        |: ("cost" := list int)
        |: ("costBurn" := string)
        |: ("costType" := string)
        |: ("description" := string)
        |: ("effect" := list (oneOf [ null [ -1 ], (list float) ]))
        |: ("effectBurn" := list string)
        |: ("image" := Image.decoder)
        |: ("key" := string)
        |: ("leveltip" := LevelTip.decoder)
        |: ("maxrank" := int)
        |: ("name" := string)
        |: ("range" := oneOf [ map Err string, map Ok (list int) ])
        |: ("rangeBurn" := string)
        |: withDefault "" ("resource" := string)
        |: ("sanitizedDescription" := string)
        |: ("sanitizedTooltip" := string)
        |: ("tooltip" := string)
        |: withDefault [] ("vars" := list SpellVars.decoder)


decoder : Decoder Model
decoder =
    map Model championSpell



-- ACCESSORS


description : Model -> Result String String
description m =
    case m of
        Empty ->
            emptyModelError "Spell.Description"

        Model { sanitizedDescription } ->
            Ok sanitizedDescription



-- VIEW


icon : Realm.Model -> Model -> Html a
icon realm m =
    case m of
        Empty ->
            text ""

        Model spell ->
            if Realm.isEmpty realm then
                text ""
            else
                img
                    [ src <|
                        ddragon
                            ++ "/"
                            ++ Result.withDefault "" (Realm.version realm)
                            ++ "/img/spell/"
                            ++ Result.withDefault "" (Image.full spell.image)
                    , alt spell.name
                    ]
                    []
