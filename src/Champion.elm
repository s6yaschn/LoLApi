module Champion
    exposing
        ( Model
        , empty
        , isEmpty
        , decoder
        , allytips
        , blurb
        , blurbFormatted
        , enemytips
        , id
        , image
        , info
        , key
        , lore
        , loreFormatted
        , name
        , partype
        , passive
        , recommended
        , skins
        , spells
        , stats
        , tags
        , title
        , splashArt
        , skinSplashArt
        , loadingScreen
        , skinLoadingScreen
        , icon
        )

import Realm
import Html exposing (..)
import Html.Attributes exposing (src, alt)
import Core exposing (..)
import Json.Decode exposing (..)
import Image
import ChampionInfo as Info
import Recommended
import Spell
import Skin
import Stats
import Passive
import Json.Decode.Extra exposing (..)
import String


-- MODEL


type Model
    = Model Champion
    | Empty


type alias Champion =
    { allytips : List String
    , blurb : String
    , enemytips : List String
    , id : Int
    , image : Image.Model
    , info : Info.Model
    , key : String
    , lore : String
    , name : String
    , partype : String
    , passive : Passive.Model
    , recommended : List Recommended.Model
    , skins : List Skin.Model
    , spells : List Spell.Model
    , stats : Stats.Model
    , tags : List String
    , title : String
    }


champion : Decoder Champion
champion =
    succeed Champion
        |: ("allytips" := list string)
        |: ("blurb" := string)
        |: ("enemytips" := list string)
        |: ("id" := int)
        |: ("image" := Image.decoder)
        |: ("info" := Info.decoder)
        |: ("key" := string)
        |: ("lore" := string)
        |: ("name" := string)
        |: ("partype" := string)
        |: ("passive" := Passive.decoder)
        |: ("recommended" := list Recommended.decoder)
        |: ("skins" := list Skin.decoder)
        |: ("spells" := list Spell.decoder)
        |: ("stats" := Stats.decoder)
        |: ("tags" := list string)
        |: ("title" := string)


decoder : Decoder Model
decoder =
    map Model champion


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


allytips : Model -> Result String (List String)
allytips m =
    case m of
        Empty ->
            emptyModelError "Champion.allytips"

        Model { allytips } ->
            Ok allytips


blurb : Model -> Result String String
blurb m =
    case m of
        Empty ->
            emptyModelError "Champion.blurb"

        Model { blurb } ->
            Ok (break blurb)


blurbFormatted : Model -> Result String (Html msg)
blurbFormatted m =
    case m of
        Empty ->
            emptyModelError "Champion.blurbFormatted"

        Model { blurb } ->
            Ok <| span [] <| List.intersperse (br [] []) <| List.map text <| String.split "<br>" blurb


enemytips : Model -> Result String (List String)
enemytips m =
    case m of
        Empty ->
            emptyModelError "Champion.enemytips"

        Model { enemytips } ->
            Ok enemytips


id : Model -> Result String Int
id m =
    case m of
        Empty ->
            emptyModelError "Champion.id"

        Model { id } ->
            Ok id


image : Model -> Result String Image.Model
image m =
    case m of
        Empty ->
            emptyModelError "Champion.image"

        Model { image } ->
            Ok image


info : Model -> Result String Info.Model
info m =
    case m of
        Empty ->
            emptyModelError "Champion.info"

        Model { info } ->
            Ok info


key : Model -> Result String String
key m =
    case m of
        Empty ->
            emptyModelError "Champion.key"

        Model { key } ->
            Ok key


lore : Model -> Result String String
lore m =
    case m of
        Empty ->
            emptyModelError "Champion.lore"

        Model { lore } ->
            Ok (break lore)


loreFormatted : Model -> Result String (Html msg)
loreFormatted m =
    case m of
        Empty ->
            emptyModelError "Champion.loreFormatted"

        Model { lore } ->
            Ok <| span [] <| List.intersperse (br [] []) <| List.map text <| String.split "<br>" lore


name : Model -> Result String String
name m =
    case m of
        Empty ->
            emptyModelError "Champion.name"

        Model { name } ->
            Ok name


partype : Model -> Result String String
partype m =
    case m of
        Empty ->
            emptyModelError "Champion.partype"

        Model { partype } ->
            Ok partype


passive : Model -> Result String Passive.Model
passive m =
    case m of
        Empty ->
            emptyModelError "Champion.passive"

        Model { passive } ->
            Ok passive


recommended : Model -> Result String (List Recommended.Model)
recommended m =
    case m of
        Empty ->
            emptyModelError "Champion.recommended"

        Model { recommended } ->
            Ok recommended


skins : Model -> Result String (List Skin.Model)
skins m =
    case m of
        Empty ->
            emptyModelError "Champion.skins"

        Model { skins } ->
            Ok skins


spells : Model -> Result String (List Spell.Model)
spells m =
    case m of
        Empty ->
            emptyModelError "Champion.spells"

        Model { spells } ->
            Ok spells


stats : Model -> Result String Stats.Model
stats m =
    case m of
        Empty ->
            emptyModelError "Champion.stats"

        Model { stats } ->
            Ok stats


tags : Model -> Result String (List String)
tags m =
    case m of
        Empty ->
            emptyModelError "Champion.tags"

        Model { tags } ->
            Ok tags


title : Model -> Result String String
title m =
    case m of
        Empty ->
            emptyModelError "Champion.title"

        Model { title } ->
            Ok title



-- VIEW


splashArt : Model -> Html a
splashArt m =
    case m of
        Empty ->
            text ""

        Model model ->
            img [ src <| ddragon ++ "/img/champion/splash/" ++ model.key ++ "_0.jpg", alt model.name ] []



-- currently defaults to splashArt if invalid id, make sure to check skin range!


skinSplashArt : Skin.Model -> Model -> Html a
skinSplashArt skin m =
    case m of
        Empty ->
            text ""

        Model model ->
            let
                valid =
                    validSkin skin model

                num =
                    Result.withDefault 0 (Skin.num skin)
            in
                img
                    [ src <|
                        ddragon
                            ++ "/img/champion/splash/"
                            ++ model.key
                            ++ "_"
                            ++ (if valid then
                                    toString num
                                else
                                    "0"
                               )
                            ++ ".jpg"
                    , alt model.name
                    ]
                    []


loadingScreen : Model -> Html a
loadingScreen m =
    case m of
        Empty ->
            text ""

        Model model ->
            img [ src <| ddragon ++ "/img/champion/loading/" ++ model.key ++ "_0.jpg", alt model.name ] []


skinLoadingScreen : Skin.Model -> Model -> Html a
skinLoadingScreen skin m =
    case m of
        Empty ->
            text ""

        Model model ->
            let
                valid =
                    validSkin skin model

                num =
                    Result.withDefault 0 (Skin.num skin)
            in
                img
                    [ src <|
                        ddragon
                            ++ "/img/champion/loading/"
                            ++ model.key
                            ++ "_"
                            ++ (if valid then
                                    toString num
                                else
                                    "0"
                               )
                            ++ ".jpg"
                    , alt model.name
                    ]
                    []



-- /= Image.view , giving 120x120 image
-- requesting image -> Image.view will give a 48x48 icon loaded from sprite


icon : Realm.Model -> Model -> Html a
icon r m =
    case m of
        Empty ->
            text ""

        Model { image, key } ->
            if Realm.isEmpty r then
                text ""
            else
                img
                    [ src <|
                        ddragon
                            ++ "/"
                            ++ Result.withDefault "" (Realm.version r)
                            ++ "/img/champion/"
                            ++ key
                            ++ ".png"
                    ]
                    []



-- helper functions


validSkin : Skin.Model -> Champion -> Bool
validSkin skin { skins } =
    List.member skin skins


break : String -> String
break =
    String.join "\n" << String.split "<br>"
