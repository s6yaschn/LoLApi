module Main exposing (..)

import Champion
import ChampionList
import Passive
import Spell
import Realm
import Request.Static
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Endpoint
import List.Extra
import Result
import Task
import Http
import Dict
import Result exposing (andThen)
import List
import Json.Decode as Json
import Skin


type alias Flags =
    { key : String
    }


main : Program Flags
main =
    Html.App.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { static : Request.Static.Model
    , champion : Champion.Model
    , all : ChampionList.Model
    , full : Bool
    , realm : Realm.Model
    , currentSkin : Int
    }



-- UPDATE


type Msg
    = NewKey String
    | Validate
    | Search String
    | Fail Http.Error
    | Succeed Champion.Model
    | Init ChampionList.Model
    | Full
    | Blurb
    | NewRealm Realm.Model
    | PreviousSkin
    | NextSkin


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NewKey key ->
            ( { model | static = Request.Static.new Endpoint.euw key }, Cmd.none )

        Validate ->
            ( model, Task.perform Fail Init <| Request.Static.getAllChampions model.static )

        Search s ->
            let
                old =
                    model.champion

                new =
                    Result.toMaybe (ChampionList.data model.all) `Maybe.andThen` Dict.get s
            in
                ( { model | champion = Maybe.withDefault old new, currentSkin = 0 }, Cmd.none )

        Fail err ->
            Debug.log (toString err) <|
                ( model, Cmd.none )

        Succeed champ ->
            ( { model | champion = champ, currentSkin = 0 }, Cmd.none )

        Init new ->
            let
                old =
                    model.all
            in
                if ChampionList.isEmpty old then
                    ( { model | all = new }, Task.perform Fail NewRealm <| Request.Static.getRealm model.static )
                else
                    ( model, Cmd.none )

        Full ->
            ( { model | full = True }, Cmd.none )

        Blurb ->
            ( { model | full = False }, Cmd.none )

        NewRealm new ->
            if Realm.isEmpty new then
                ( model, Cmd.none )
            else
                ( { model | realm = new }, Cmd.none )

        NextSkin ->
            let
                old =
                    model.currentSkin

                max =
                    Result.withDefault 0 <| Result.map List.length (Champion.skins model.champion)
            in
                if old >= max - 1 then
                    ( model, Cmd.none )
                else
                    ( { model | currentSkin = old + 1 }, Cmd.none )

        PreviousSkin ->
            let
                old =
                    model.currentSkin
            in
                if old <= 0 then
                    ( model, Cmd.none )
                else
                    ( { model | currentSkin = old - 1 }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view ({ all } as model) =
    div []
        [ if ChampionList.isEmpty all then
            viewKeyInput
          else
            span []
                [ viewSelect model
                , button [ onClick PreviousSkin ] [ text "previous skin" ]
                , button [ onClick NextSkin ] [ text "nextSkin" ]
                ]
        , br [] []
        , viewChampion model
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT


init : Flags -> ( Model, Cmd Msg )
init { key } =
    update (Validate)
        { static = Request.Static.new Endpoint.euw key
        , champion = Champion.empty
        , all = ChampionList.empty
        , full = False
        , realm = Realm.empty
        , currentSkin = 0
        }



-- helper functions


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    on "change" (Json.map tagger targetValue)


viewKeyInput : Html Msg
viewKeyInput =
    Html.form [ onSubmit Validate ]
        [ input
            [ type' "text"
            , placeholder "enter API key"
            , onInput NewKey
            ]
            []
        , input [ type' "submit" ] [ text "submit" ]
        ]


viewSelect : Model -> Html Msg
viewSelect { all } =
    let
        keys =
            Dict.values <| Result.withDefault Dict.empty <| ChampionList.keys all

        data =
            Result.withDefault Dict.empty <| ChampionList.data all

        keyToName k =
            Maybe.withDefault k <| (Dict.get k data) `Maybe.andThen` (Result.toMaybe << Champion.name)
    in
        select [ onChange Search, required True ] <|
            List.append [ option [ value "", selected True, hidden True ] [ text "Select a Champion:" ] ] <|
                List.map (\( key, name ) -> option [ value key ] [ text name ]) <|
                    List.sortBy snd <|
                        List.Extra.zip keys (List.map keyToName keys)


viewChampion : Model -> Html Msg
viewChampion model =
    span []
        [ viewHeading [] model
        , if model.full then
            viewLore [] model
          else
            viewBlurb [] model
        , br [] []
        , viewPassive model
        , viewSpells model
        , br [] []
        , viewSkin model
        ]


viewHeading : List (Attribute Msg) -> Model -> Html Msg
viewHeading attr { champion } =
    Result.withDefault (text "") <|
        andThen (Champion.name champion) <|
            \name ->
                andThen (Champion.title champion) <|
                    \title ->
                        Ok <|
                            span attr [ h1 [] [ text name ], h3 [] [ text title ] ]


viewLore : List (Attribute Msg) -> Model -> Html Msg
viewLore attr { champion } =
    Result.withDefault (text "") <|
        andThen (Champion.loreFormatted champion) <|
            \lore ->
                Ok <|
                    span attr
                        [ lore
                        , br [] []
                        , button [ onClick Blurb ] [ text "hide lore" ]
                        ]


viewBlurb : List (Attribute Msg) -> Model -> Html Msg
viewBlurb attr { champion } =
    Result.withDefault (text "") <|
        andThen (Champion.blurbFormatted champion) <|
            \blurb ->
                Ok <|
                    span attr
                        [ blurb
                        , br [] []
                        , button [ onClick Full ] [ text "show full lore" ]
                        ]


viewPassive : Model -> Html Msg
viewPassive { champion, realm } =
    Result.withDefault (text "") <|
        andThen (Champion.passive champion) <|
            \passive ->
                andThen (Passive.description passive) <|
                    \description ->
                        Ok <| span [ title description ] [ Passive.icon realm passive ]


viewOneSpell : Realm.Model -> Spell.Model -> Html Msg
viewOneSpell realm spell =
    span [ title <| Result.withDefault "" <| Spell.description spell ] [ Spell.icon realm spell ]


viewSpells : Model -> Html Msg
viewSpells { champion, realm } =
    Result.withDefault (text "") <|
        andThen (Champion.spells champion) <|
            \spells ->
                Ok <| span [] <| List.map (viewOneSpell realm) spells


viewSkin : Model -> Html Msg
viewSkin { champion, currentSkin } =
    Result.withDefault (text "") <|
        andThen (Champion.skins champion) <|
            \skins ->
                let
                    s =
                        Maybe.withDefault Skin.empty (List.Extra.getAt currentSkin skins)
                in
                    Ok <| Champion.skinSplashArt s champion
