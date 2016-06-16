module Champion exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (src, alt)
import Core exposing (..)
import Static
import Task
import String
import Region
import Json.Decode exposing (..)
import Image
import ChampionInfo as Info
import Recommended
import Spell
import Skin
import Stats
import Passive

main =
    Html.program
        { init = init
        , view = view 
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL 

type Model = Model Champion

type alias Champion = 
    { allytips: List String
    , blurb: String
    , enemytips: List String
    , id: Int
    , image: Image.Model
    , info: Info.Model
    , key: String 
    , lore: String
    , name: String
    , partype: String
    , passive: Passive.Model
    , recommended: List Recommended.Model
    , skins: List Skin.Model
    , spells: List Spell.Model
    , stats: Stats.Model
    , tags: List String
    , title: String 
    }

champion : Decoder Champion
champion =
    Champion
    <$> "allytips" := list string
    <+> "blurb" := string 
    <+> "enemytips" := list string 
    <+> "id" := int 
    <+> "image" := Image.decoder
    <+> "info" := Info.decoder
    <+> "key" := string  
    <+> "lore" := string 
    <+> "name" := string  
    <+> "partype" := string 
    <+> "passive" := Passive.decoder
    <+> "recommended" := list Recommended.decoder
    <+> "skins" := list Skin.decoder
    <+> "spells" := list Spell.decoder
    <+> "stats" := Stats.decoder
    <+> "tags" := list string 
    <+> "title" := string

decoder : Decoder Model 
decoder = map Model champion

emptyChampion : Champion
emptyChampion = 
    Champion [] "" [] 34 (fst Image.init) (fst Info.init) "" "" "" "" (fst Passive.init) [] [] [] (fst Stats.init) [] ""

-- ACCESSORS

allytips: Model -> List String 
allytips (Model champ) = champ.allytips

blurb: Model -> String 
blurb (Model champ) = champ.blurb

enemytips : Model -> List String 
enemytips (Model champ) = champ.enemytips

id : Model -> Int 
id (Model champ) = champ.id

image : Model -> Image.Model
image (Model champ) = champ.image

info : Model -> Info.Model
info (Model champ) = champ.info

key : Model -> String 
key (Model champ) = champ.key

lore : Model -> String 
lore (Model champ) = champ.lore

name : Model -> String 
name (Model champ) = champ.name

-- clarify
partype : Model -> String 
partype (Model champ) = champ.partype


getChampionById : Region.Endpoint -> Int -> Request Model
getChampionById endpoint id =
    Static.request endpoint decoder <| "champion/" ++ toString id ++ "?champData=all" 


-- INIT

init : (Model, Cmd Msg)
init = ( Model emptyChampion , Cmd.none)


-- UPDATE


type Msg
    = Search Int
    | NewChamp Model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        Search id ->
            (model, Task.perform (\_ -> NewChamp model) NewChamp (getChampionById Region.euw id))
        NewChamp newChamp ->
            (newChamp, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

splashArt : Model -> Html Msg
splashArt (Model model) =
    img [src <| ddragon ++ "/img/champion/splash/" ++ model.key ++ "_0.jpg", alt model.name] []



-- currently defaults to splashArt if invalid id, make sure to check skin range!
skinSplashArt : Int -> Model -> Html Msg
skinSplashArt id (Model model) =
    let 
        valid = validSkin id model
    in 
        img [ src <| ddragon ++ "/img/champion/splash/" 
                ++ model.key ++ "_" 
                ++ (if valid then toString id else "0")
                ++ ".jpg"
            , alt model.name] []
 

loadingScreen : Model -> Html Msg
loadingScreen (Model model) =
    img [src <| ddragon ++ "/img/champion/loading/" ++ model.key ++ "_0.jpg", alt model.name] []


skinLoadingScreen : Int -> Model -> Html Msg
skinLoadingScreen id (Model model) =
    let
        valid = validSkin id model
    in
        img [ src <| ddragon ++ "/img/champion/loading/"
                ++ model.key ++ "_"
                ++ (if valid then toString id else "0")
                ++ ".jpg"
            , alt model.name] []

view : Model -> Html Msg
view model =
    div []
        [ input [onInput (\x -> Search (Result.withDefault 34 (String.toInt x)))] []
        , skinLoadingScreen 3 model
        ]


-- helper functions
validSkin : Int -> Champion -> Bool
validSkin id champ =
    not <| List.isEmpty <| List.filter (\x -> Skin.num x == id) champ.skins