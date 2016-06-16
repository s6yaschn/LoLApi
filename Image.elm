module Image exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Core exposing (..)
import Version exposing (getVersion, testVersion)
import Json.Decode exposing (..)

main = Html.program
    { init = init 
    , view = view testVersion
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

-- make opaque
type Model = Model Image


type alias Image =
    { full: String
    , group: String
    , h: Int
    , sprite: String
    , w: Int
    , x: Int
    , y: Int
    }


image : Decoder Image 
image =  
    Image
    <$> "full" := string
    <+> "group" := string
    <+> "h" := int
    <+> "sprite" := string
    <+> "w" := int
    <+> "x" := int
    <+> "y" := int

decoder : Decoder Model
decoder = map Model image

emptyImage: Image
emptyImage = Image "" "" 0 "" 0 0 0

-- ACCESSORS

-- TBD: naming

full : Model -> String
full (Model img) = img.full

group : Model -> String 
group (Model img) = img.group

sprite : Model -> String
sprite (Model img) = img.sprite

size : Model -> (Int, Int)
size (Model img) = (img.w, img.h)

position : Model -> (Int, Int)
position (Model img) = (img.x, img.y)

-- UPDATE

type Msg 
    = NewImage Model 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewImage new ->
            (new, Cmd.none)


-- VIEW

-- add Sprite support?
view : Version.Model -> Model -> Html msg
view version (Model img) =
    let 
        url =  ddragon ++ "/" ++ getVersion version ++ "/img/sprite/" ++ img.sprite
        pos = toString (negate img.x) ++ "px " ++ toString (negate img.y) ++ "px"
    in 
        canvas 
            [ width img.w
            , height img.h
            , alt img.full
            , style 
                [ ("background-image", "url('" ++ url ++ "') ")
                , ("background-position", pos)]
            ] []

 
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- INIT

init : (Model, Cmd Msg)
init =
    (Model {emptyImage
        | w = 48
        , full = "Incinerate.png"
        , sprite = "spell0.png"
        , group = "spell"
        , h = 48
        , y = 144
        , x = 432}, Cmd.none)

