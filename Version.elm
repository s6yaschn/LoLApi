module Version exposing (Model, init, Msg, update, getVersion, testVersion)

import Task
import Static
import Region
import Html exposing (..)
import Html.Events exposing (..)
import Html.App

main = Html.App.program
  { init = init 
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

-- testing
testVersion : Model
testVersion = {version = "6.12.1"}


-- MODEL

type alias Model =
  { version : String
  }


-- UPDATE

type Msg 
    = Request
    | NewVersion String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      Request ->
        (model, Task.perform (always <| NewVersion model.version) NewVersion <| Task.map .v <| Static.getRealm Region.euw)
      NewVersion newVersion ->
        (Model newVersion, Cmd.none ) 


-- VIEW

getVersion: Model -> String
getVersion = .version

view : Model -> Html.Html Msg
view model =
  button [onClick Request] [text model.version]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- INIT

init : (Model, Cmd Msg)
init =
  (Model "Click Me", Cmd.none)