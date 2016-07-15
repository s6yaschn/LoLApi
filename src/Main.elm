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
import Dict exposing (Dict)
import Result exposing (andThen)
import List
import Json.Decode as Json
import Skin
import String
import Stats
import Recommended


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
    , currentChampion : Champion.Model
    , all : ChampionList.Model
    , full : Bool
    , realm : Realm.Model
    , currentSkin : Int
    , currentLanguage : String
    , languages : List String
    , loading : Bool
    , languageStrings : Dict String String
    , currentMap : String
    }


regions : Dict String Endpoint.Model
regions =
    let
        endpoints =
            [ Endpoint.euw, Endpoint.eune, Endpoint.br, Endpoint.jp ]
    in
        Dict.fromList <| List.Extra.zip (List.map Endpoint.region endpoints) endpoints


defaultRegion : Endpoint.Model
defaultRegion =
    Endpoint.euw



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
    | NewRegion String
    | NewLanguage String
    | NewLanguageStrings (Dict String String)
    | InitLanguages (List String)
    | Refresh
    | Finish Msg
    | NewMap String


load : (a -> Msg) -> Task.Task Http.Error a -> Model -> ( Model, Cmd Msg )
load tagger task model =
    ( { model | loading = True }, Task.perform (Finish << Fail) (Finish << tagger) task )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NewKey key ->
            ( { model | static = Request.Static.new Endpoint.euw key }, Cmd.none )

        Validate ->
            ( model, Task.perform Fail InitLanguages <| Request.Static.getLanguages model.static )

        Search s ->
            let
                old =
                    model.currentChampion

                new =
                    Result.toMaybe (ChampionList.data model.all) `Maybe.andThen` Dict.get s
            in
                ( { model | currentChampion = Maybe.withDefault old new, currentSkin = 0 }, Cmd.none )

        Fail err ->
            Debug.log (flip String.append "\n" <| String.left 50 <| toString err) <|
                ( model, Cmd.none )

        Succeed champ ->
            ( { model | currentChampion = champ, currentSkin = 0 }, Cmd.none )

        Init new ->
            let
                old =
                    model.all
            in
                ( { model | all = new }, Task.perform Fail NewRealm <| Request.Static.getRealm model.static )

        Full ->
            ( { model | full = True }, Cmd.none )

        Blurb ->
            ( { model | full = False }, Cmd.none )

        NewRealm new ->
            update Refresh { model | realm = new }

        NextSkin ->
            let
                old =
                    model.currentSkin

                max =
                    Result.withDefault 0 <| Result.map List.length (Champion.skins model.currentChampion)
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

        NewRegion regio ->
            let
                new =
                    Maybe.withDefault (Request.Static.endpoint model.static) (Dict.get regio regions)
            in
                ( { model | static = Request.Static.updateEndpoint model.static new }, Task.perform Fail InitLanguages <| Request.Static.getLanguages model.static )

        NewLanguage new ->
            ( { model | currentLanguage = new }, Task.perform Fail NewLanguageStrings <| Request.Static.getLanguageStrings model.static new )

        NewLanguageStrings strings ->
            load Init (Request.Static.getAllChampionsLoc model.currentLanguage model.static) <| { model | languageStrings = strings }

        InitLanguages langs ->
            update (NewLanguage <| Maybe.withDefault "en_US" (List.head langs)) { model | languages = langs }

        Refresh ->
            let
                key =
                    Debug.log "Refresh" <| Result.withDefault "" (Champion.key model.currentChampion)
            in
                if key /= "" then
                    update (Search key) model
                else
                    ( model, Cmd.none )

        Finish msg ->
            update msg { model | loading = False }

        NewMap new ->
            ( { model | currentMap = new }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view ({ all, currentLanguage, loading, languageStrings } as model) =
    if loading then
        text <| Maybe.withDefault "Loading..." <| Dict.get "mobilePleaseWait" languageStrings
    else
        div []
            [ if ChampionList.isEmpty all then
                viewKeyInput
              else
                span []
                    [ viewChampionSelect model
                    , viewRegionSelect model
                    , viewLanguageSelect model
                    , br [] []
                    , viewChampion model
                    ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT


init : Flags -> ( Model, Cmd Msg )
init { key } =
    update (Validate)
        { static = Request.Static.new defaultRegion key
        , currentChampion = Champion.empty
        , all = ChampionList.empty
        , full = True
        , realm = Realm.empty
        , currentSkin = 0
        , languages = []
        , currentLanguage = ""
        , loading = False
        , languageStrings = Dict.empty
        , currentMap = ""
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
            , size 40
            ]
            []
        , input [ type' "submit" ] [ text "submit" ]
        ]


viewChampionSelect : Model -> Html Msg
viewChampionSelect { all, currentChampion, languageStrings } =
    let
        keys =
            Dict.values <| Result.withDefault Dict.empty <| ChampionList.keys all

        data =
            Result.withDefault Dict.empty <| ChampionList.data all

        keyToName k =
            Maybe.withDefault k <| (Dict.get k data) `Maybe.andThen` (Result.toMaybe << Champion.name)

        isSelected =
            (==) <| Result.withDefault "" (Champion.key currentChampion)

        prompt =
            Maybe.withDefault "Select a Champion:" (Dict.get "categoryChampion" languageStrings)
    in
        select [ onChange Search, required True ] <|
            List.append [ option [ value "", selected (isSelected ""), hidden True ] [ text prompt ] ] <|
                List.map (\( key, name ) -> option [ selected (isSelected key), value key ] [ text name ]) <|
                    List.sortBy snd (List.Extra.zip keys (List.map keyToName keys))


viewRegionSelect : Model -> Html Msg
viewRegionSelect { static } =
    let
        isSelected =
            (==) <| Endpoint.region (Request.Static.endpoint static)
    in
        select [ onChange NewRegion ] <| List.map (\x -> option [ selected (isSelected x), value x ] [ text x ]) (Dict.keys regions)


viewLanguageSelect : Model -> Html Msg
viewLanguageSelect { languages, currentLanguage, languageStrings } =
    let
        isSelected =
            (==) currentLanguage

        native l =
            String.append (l ++ " ") <|
                Maybe.withDefault "" <|
                    Maybe.oneOf
                        [ Dict.get ("native_" ++ l) languageStrings
                        , Dict.get ("native_" ++ String.left 2 l) languageStrings
                        ]
    in
        select [ onChange NewLanguage ] <| List.map (\x -> option [ selected (isSelected x), value x ] [ text (native x) ]) languages


viewSkinSelect : Model -> Html Msg
viewSkinSelect { currentChampion, languageStrings, currentSkin } =
    Result.withDefault (text "") <|
        andThen (Champion.skins currentChampion) <|
            \skins ->
                let
                    s =
                        Maybe.withDefault Skin.empty (List.Extra.getAt currentSkin skins)
                in
                    andThen (Skin.name s) <|
                        \name ->
                            Ok <|
                                span []
                                    [ h3 [] [ text name ]
                                    , button [ onClick PreviousSkin ] [ text <| Maybe.withDefault "previous skin" <| Dict.get "Back" languageStrings ]
                                    , button [ onClick NextSkin ] [ text <| Maybe.withDefault "next skin" <| Dict.get "Continue" languageStrings ]
                                    ]


viewChampion : Model -> Html Msg
viewChampion model =
    span []
        [ viewHeading [] model
        , br [] []
        , viewStats [ class "left" ] model
        , if model.full then
            viewLore [] model
          else
            viewBlurb [] model
        , br [ class "clear" ] []
        , viewPassive model
        , viewSpells model
          {- , br [] []
             , viewRecommended model
             , br [] []
          -}
        , viewSkinSelect model
        , span [ class "fullWidth" ] [ viewSkin model ]
        ]


viewHeading : List (Attribute Msg) -> Model -> Html Msg
viewHeading attr { currentChampion } =
    Result.withDefault (text "") <|
        andThen (Champion.name currentChampion) <|
            \name ->
                andThen (Champion.title currentChampion) <|
                    \title ->
                        Ok <|
                            span attr [ h1 [] [ text name ], h3 [] [ text title ] ]


viewLore : List (Attribute Msg) -> Model -> Html Msg
viewLore attr { currentChampion, languageStrings } =
    Result.withDefault (text "") <|
        andThen (Champion.loreFormatted currentChampion) <|
            \lore ->
                Ok <|
                    span attr
                        [ lore
                        , br [] []
                          --   , button [ onClick Blurb ] [ text <| Maybe.withDefault "lore" (Dict.get "Lore" languageStrings) ]
                        ]


viewBlurb : List (Attribute Msg) -> Model -> Html Msg
viewBlurb attr { currentChampion, languageStrings } =
    Result.withDefault (text "") <|
        andThen (Champion.blurbFormatted currentChampion) <|
            \blurb ->
                Ok <|
                    span attr
                        [ blurb
                        , br [] []
                          --   , button [ onClick Full ] [ text <| Maybe.withDefault "lore" (Dict.get "Lore" languageStrings) ]
                        ]


viewPassive : Model -> Html Msg
viewPassive { currentChampion, realm } =
    Result.withDefault (text "") <|
        andThen (Champion.passive currentChampion) <|
            \passive ->
                andThen (Passive.description passive) <|
                    \description ->
                        Ok <| span [ title description ] [ Passive.icon realm passive ]


viewOneSpell : Realm.Model -> Spell.Model -> Html Msg
viewOneSpell realm spell =
    span [ title <| Result.withDefault "" <| Spell.description spell ] [ Spell.icon realm spell ]


viewSpells : Model -> Html Msg
viewSpells { currentChampion, realm } =
    Result.withDefault (text "") <|
        andThen (Champion.spells currentChampion) <|
            \spells ->
                Ok <| span [] <| List.map (viewOneSpell realm) spells


viewSkin : Model -> Html Msg
viewSkin { currentChampion, currentSkin } =
    Result.withDefault (text "") <|
        andThen (Champion.skins currentChampion) <|
            \skins ->
                let
                    s =
                        Maybe.withDefault Skin.empty (List.Extra.getAt currentSkin skins)
                in
                    Ok <| Champion.skinSplashArt s currentChampion


viewRecommended : Model -> Html Msg
viewRecommended { currentChampion, currentMap, languageStrings, realm } =
    --todo: clean up
    let
        isSelected =
            (==) currentMap

        recs =
            Result.withDefault [] (Champion.recommended currentChampion)

        maps =
            List.map (Result.withDefault "" << Recommended.map) recs

        mapId str =
            case str of
                "HA" ->
                    "Map12"

                "TT" ->
                    "Map10"

                "SR" ->
                    "Map1"

                "CS" ->
                    "Map8"

                _ ->
                    str

        translate str =
            Maybe.withDefault str (Dict.get (mapId str) languageStrings)
    in
        if List.isEmpty recs then
            text ""
        else
            span []
                [ select [ onChange NewMap ] <|
                    List.append [ option [ selected (isSelected ""), value "", hidden True ] [ text <| translate "RecommendedItems" ] ] <|
                        List.map (\x -> option [ selected (isSelected x), value x ] [ text <| translate x ]) maps
                , br [] []
                , Recommended.view realm <|
                    -- todo: translated view
                    Maybe.withDefault Recommended.empty
                    <|
                        Maybe.map snd <|
                            List.Extra.find (isSelected << fst) <|
                                List.Extra.zip maps recs
                  -- todo: remove duplicates
                ]


viewStats : List (Attribute Msg) -> Model -> Html Msg
viewStats attr { currentChampion, languageStrings } =
    Result.withDefault (text "") <|
        andThen (Champion.stats currentChampion) <|
            \stats ->
                let
                    show ( x, y ) =
                        ( toString x, toString y )

                    ( hp, hpLvl ) =
                        show (Stats.hp stats)

                    ( ad, adLvl ) =
                        show (Stats.attackdamage stats)

                    ( speed, speedLvl ) =
                        show (Stats.attackspeed stats)

                    ms =
                        toString (Stats.movespeed stats)

                    ( hpreg, hpregLvl ) =
                        show (Stats.hpregen stats)

                    ( armor, armorLvl ) =
                        show (Stats.armor stats)

                    ( mr, mrLvl ) =
                        show (Stats.magicresist stats)

                    ( mana, manaLvl ) =
                        show (Stats.mana stats)

                    ( manareg, manaregLvl ) =
                        show (Stats.manaregen stats)

                    translate str =
                        text <| Maybe.withDefault str (Dict.get str languageStrings)

                    format str1 str2 =
                        text (str1 ++ " (+" ++ str2 ++ ")")
                in
                    Ok <|
                        table attr
                            [ tr []
                                [ th [ colspan 2 ] [ translate "Stats" ]
                                ]
                            , tr []
                                [ td [] [ translate "Health" ]
                                , td [] [ format hp hpLvl ]
                                ]
                            , tr []
                                [ td [] [ translate "HealthRegen" ]
                                , td [] [ format hpreg hpregLvl ]
                                ]
                            , tr []
                                [ td [] [ translate "Attack" ]
                                , td [] [ format ad adLvl ]
                                ]
                            , tr []
                                [ td [] [ translate "AttackSpeed" ]
                                , td [] [ format speed (speedLvl ++ "%") ]
                                ]
                            , tr []
                                [ td [] [ translate "Armor" ]
                                , td [] [ format armor armorLvl ]
                                ]
                            , tr []
                                [ td [] [ translate "SpellBlock" ]
                                , td [] [ format mr mrLvl ]
                                ]
                            , tr []
                                [ td [] [ translate "FlatMovementSpeedMod" ]
                                , td [] [ text ms ]
                                ]
                            , tr []
                                [ td [] [ translate "Mana" ]
                                , td [] [ format mana manaLvl ]
                                ]
                            , tr []
                                [ td [] [ translate "ManaRegen" ]
                                , td [] [ format manareg manaregLvl ]
                                ]
                            ]
