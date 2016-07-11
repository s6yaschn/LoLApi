module SummonerSpell exposing (..)

import Core exposing (..)
import Image
import LevelTip
import SpellVars
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)


type alias SummonerSpell =
    { cooldown : List Float
    , cooldownBurn : String
    , cost : List Int
    , costBurn : String
    , costType : String
    , description : String
    , effect : List (List Float)
    , effectBurn : List String
    , id : Int
    , image : Image.Model
    , key : String
    , leveltip : LevelTip.Model
    , maxrank : Int
    , modes : List String
    , name : String
    , range : Result String (List Int)
    , rangeBurn : String
    , ressource : String
    , sanitizedDescription : String
    , sanitizedTooltip : String
    , summonerLevel : Int
    , tooltip : String
    , vars : List SpellVars.Model
    }


type Model
    = Model SummonerSpell
    | Empty


summonerSpell : Decoder SummonerSpell
summonerSpell =
    succeed SummonerSpell
        |: ("cooldown" := list float)
        |: ("cooldownBurn" := string)
        |: ("cost" := list int)
        |: ("costBurn" := string)
        |: ("costType" := string)
        |: ("description" := string)
        |: ("effect" := list (list float))
        |: ("effectBurn" := list string)
        |: ("id" := int)
        |: ("image" := Image.decoder)
        |: ("key" := string)
        |: ("leveltip" := LevelTip.decoder)
        |: ("maxrank" := int)
        |: ("modes" := list string)
        |: ("name" := string)
        |: oneOf [ map Err string, map Ok (list int) ]
        |: ("rangeBurn" := string)
        |: ("resource" := string)
        |: ("sanitizedDescription" := string)
        |: ("sanitizedTooltip" := string)
        |: ("summonerLevel" := int)
        |: ("tooltip" := string)
        |: ("vars" := list SpellVars.decoder)


decoder : Decoder Model
decoder =
    map Model summonerSpell


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

-- ACCESSORS

key : Model -> Result String String
key m =
    case m of 
        Empty -> emptyModelError "SummonerSpell.key"
        Model {key} -> Ok key