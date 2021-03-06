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
import Window exposing (Size)
import Task.Extra

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
    , currentRecommended : Recommended.Model
    , currentSize : Size
    }


regions : Dict String Endpoint.Model
regions =
    let
        endpoints =
            Endpoint.pbe :: Endpoint.all
    in
        Dict.fromList <| List.Extra.zip (List.map Endpoint.region endpoints) endpoints


defaultRegion : Endpoint.Model
defaultRegion =
    Endpoint.euw


windowSizeBreakpoint : Int
windowSizeBreakpoint =
    800


sideBarWidth : Int
sideBarWidth =
    350



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
    | Finish Msg
    | NewMap String
    | NewRecommended String
    | NewSize Size
    | InitSize Size

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
                update (NewMap model.currentMap) { model | currentChampion = Maybe.withDefault old new, currentSkin = 0 }

        Fail err ->
            Debug.log (flip String.append "\n" <| String.left 50 <| toString err) <|
                ( model, Cmd.none )

        Succeed champ ->
            ( { model | currentChampion = champ, currentSkin = 0 }, Cmd.none )

        Init new ->
            let
                key =
                    Result.withDefault "" <| Champion.key model.currentChampion
            in
                update (Search key) { model | all = new }

        Full ->
            ( { model | full = True }, Cmd.none )

        Blurb ->
            ( { model | full = False }, Cmd.none )

        NewRealm new ->
            update (NewLanguage <| Result.withDefault "en_US" <| Realm.defaultLanguage new) { model | realm = new }

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

                newStatic =
                    Request.Static.updateEndpoint model.static new
            in
                ( { model | static = newStatic }, Task.perform Fail InitLanguages <| Request.Static.getLanguages newStatic )

        NewLanguage new ->
            ( { model | currentLanguage = new }, Task.perform Fail NewLanguageStrings <| Request.Static.getLanguageStrings model.static new )

        NewLanguageStrings strings ->
            load Init (Request.Static.getAllChampionsLoc model.currentLanguage model.static) <| { model | languageStrings = strings }

        InitLanguages langs ->
            ( { model | languages = langs }, Task.perform Fail NewRealm <| Request.Static.getRealm model.static )

        Finish msg ->
            update msg { model | loading = False }

        NewMap new ->
            let
                rec =
                    Result.withDefault [] <| Champion.recommended model.currentChampion

                index =
                    Maybe.withDefault -1 <| List.Extra.findIndex ((==) new << Result.withDefault "" << Recommended.map) rec
            in
                update (NewRecommended <| toString index) { model | currentMap = new }

        NewRecommended nr ->
            let
                rec =
                    Result.withDefault [] <| Champion.recommended model.currentChampion

                index =
                    Result.withDefault 0 <| String.toInt nr

                new =
                    Maybe.withDefault model.currentRecommended <| List.Extra.getAt index rec
            in
                ( { model | currentRecommended = new }, Cmd.none )

        NewSize new ->
            ( { model | currentSize = new }, Cmd.none )

        InitSize new ->
            update Validate {model| currentSize = new}

-- VIEW


view : Model -> Html Msg
view ({ all, currentLanguage, loading, languageStrings, currentChampion } as model) =
    if loading then
        text <| Maybe.withDefault "Loading..." <| Dict.get "mobilePleaseWait" languageStrings
    else
        span []
            [ if ChampionList.isEmpty all then
                viewKeyInput
              else
                div [ class "centered" ]
                    [ viewChampionSelect model
                    , viewRegionSelect model
                    , viewLanguageSelect model
                    ]
            , if Champion.isEmpty currentChampion then
                text ""
              else
                viewChampion model
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes NewSize



-- INIT


init : Flags -> ( Model, Cmd Msg )
init { key } = (
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
        , currentMap = "SR"
        , currentRecommended = Recommended.empty
        , currentSize = Size 800 600
        }, Task.Extra.performFailproof InitSize Window.size)



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
viewChampion ({ currentSize } as model) =
    let
        bigWindow =
            currentSize.width >= windowSizeBreakpoint

        smallWindow =
            not bigWindow

        left =
            style <|
                if bigWindow then
                    [ ( "width", toString sideBarWidth ++ "px" ) ]
                else
                    []

        right =
            style <|
                if bigWindow then
                    [ ( "width", toString (currentSize.width - sideBarWidth - 100) ++ "px" ) ]
                else
                    []
    in
        div []
            [ viewHeading [] model
            , div
                [ classList
                    [ ( "block", bigWindow )
                    , ( "centered", smallWindow )
                    ]
                , left
                ]
                [ viewStats [] model
                , br [] []
                , viewAbilities [] model
                , viewRecommended model
                ]
            , div
                [ classList
                    [ ( "block", bigWindow )
                    , ( "right", bigWindow )
                    , ( "centered", smallWindow )
                    ]
                , right
                ]
                [ viewLore [] model
                , viewSkinSelect model
                , div [ class "fullWidth" ] [ viewSkin model ]
                ]
            ]


viewHeading : List (Attribute Msg) -> Model -> Html Msg
viewHeading attr { currentChampion } =
    Result.withDefault (text "") <|
        andThen (Champion.name currentChampion) <|
            \name ->
                andThen (Champion.title currentChampion) <|
                    \title ->
                        Ok <|
                            span attr [ h1 [] [ text name ], h2 [] [ text title ] ]


viewLore : List (Attribute Msg) -> Model -> Html Msg
viewLore attr { currentChampion, languageStrings } =
    Result.withDefault (text "") <|
        andThen (Champion.loreFormatted currentChampion) <|
            \lore ->
                Ok <|
                    span attr
                        [ h3 [] [ text <| Maybe.withDefault "Lore" <| Dict.get "Lore" languageStrings ]
                        , lore
                        ]


viewAbilities : List (Attribute Msg) -> Model -> Html Msg
viewAbilities attr ({ languageStrings } as model) =
    span attr
        [ h3 [] [ text <| Maybe.withDefault "Abilities" <| Dict.get "Abilities" languageStrings ]
        , viewPassive model
        , viewSpells model
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
viewRecommended { currentChampion, currentMap, languageStrings, realm, currentRecommended } =
    let
        recs =
            List.indexedMap (,) <| Result.withDefault [] <| Champion.recommended currentChampion

        maps =
            List.Extra.unique <| List.map (Result.withDefault "" << Recommended.map << snd) recs

        modes =
            List.map (\( x, y ) -> ( x, Result.withDefault "error" <| Recommended.mode y )) <| List.filter ((==) (Ok currentMap) << Recommended.map << snd) recs

        translate def str =
            Maybe.withDefault def <| Dict.get str languageStrings

        guard : List a -> Html Msg -> Html Msg
        guard l =
            if List.isEmpty l then
                always (text "")
            else
                identity

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

        format str =
            String.left 1 str ++ String.toLower (String.dropLeft 1 str)
    in
        guard recs <|
            span []
                [ h3 [] [ text <| translate "Recommended Items" "RecommendedItems" ]
                , select [ onChange NewMap ] <|
                    List.map (\x -> option [ value x, selected (x == currentMap) ] [ text <| translate x <| mapId x ]) maps
                , guard modes <| select [ onChange NewRecommended ] <| List.map (\( x, y ) -> option [ value <| toString x ] [ text <| translate (format y) <| "mode" ++ format y ]) modes
                , br [] []
                , Recommended.viewLoc realm languageStrings currentRecommended
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
                        span attr <|
                            [ h3 [] [ translate "Stats" ]
                            , table []
                                [ tr []
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
                            ]
