module Champion exposing (init, Msg, update, splashArt, skinSplashArt, loadingScreen, skinLoadingScreen)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Core exposing (..)
import Static
import Task
import String
import Region

main =
    Html.program
        { init = init {emptyChampion| key = "Aatrox", name = "Aatrox"}
        , view = view 
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL 

type alias Model =
   Champion

init : Champion -> (Model, Cmd Msg)
init champ =
    ( champ , Cmd.none)


-- UPDATE


type Msg
    = Search Int
    | NewChamp Champion

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        Search id ->
            (model, Task.perform (\_ -> NewChamp model) NewChamp (Static.getChampionById Region.euw id))
        NewChamp newChamp ->
            (newChamp, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

splashArt : Model -> Html Msg
splashArt model =
    img [src <| ddragon ++ "/img/champion/splash/" ++ model.key ++ "_0.jpg", alt model.name] []



-- currently defaults to splashArt if invalid id, make sure to check skin range!
skinSplashArt : Int -> Model -> Html Msg
skinSplashArt id model =
    let 
        valid = validSkin id model
    in 
        img [ src <| ddragon ++ "/img/champion/splash/" 
                ++ model.key ++ "_" 
                ++ (if valid then toString id else "0")
                ++ ".jpg"
            , alt model.name] []
 

loadingScreen : Model -> Html Msg
loadingScreen model =
    img [src <| ddragon ++ "/img/champion/loading/" ++ model.key ++ "_0.jpg", alt model.name] []


skinLoadingScreen : Int -> Model -> Html Msg
skinLoadingScreen id model =
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
    not <| List.isEmpty <| List.filter (\x -> x.num == id) champ.skins