module Stats exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)


-- MODEL


type alias Stats =
    { armor : Float
    , armorperlevel : Float
    , attackdamage : Float
    , attackdamageperlevel : Float
    , attackrange : Float
    , attackspeedoffset : Float
    , attackspeedperlevel : Float
    , crit : Float
    , critperlevel : Float
    , hp : Float
    , hpperlevel : Float
    , hpregen : Float
    , hpregenperlevel : Float
    , movespeed : Float
    , mp : Float
    , mpperlevel : Float
    , mpregen : Float
    , mpregenperlevel : Float
    , spellblock : Float
    , spellblockperlevel : Float
    }


type Model
    = Model Stats


stats : Decoder Stats
stats =
    succeed Stats
        |: ("armor" := float)
        |: ("armorperlevel" := float)
        |: ("attackdamage" := float)
        |: ("attackdamageperlevel" := float)
        |: ("attackrange" := float)
        |: ("attackspeedoffset" := float)
        |: ("attackspeedperlevel" := float)
        |: ("crit" := float)
        |: ("critperlevel" := float)
        |: ("hp" := float)
        |: ("hpperlevel" := float)
        |: ("hpregen" := float)
        |: ("hpregenperlevel" := float)
        |: ("movespeed" := float)
        |: ("mp" := float)
        |: ("mpperlevel" := float)
        |: ("mpregen" := float)
        |: ("mpregenperlevel" := float)
        |: ("spellblock" := float)
        |: ("spellblockperlevel" := float)


decoder : Decoder Model
decoder =
    map Model stats



-- ACCESSORS


armor : Model -> ( Float, Float )
armor (Model stats) =
    ( stats.armor, stats.armorperlevel )


attackdamage : Model -> ( Float, Float )
attackdamage (Model s) =
    ( s.attackdamage, s.attackdamageperlevel )


range : Model -> Float
range (Model s) =
    s.attackrange


attackspeed : Model -> ( Float, Float )
attackspeed (Model s) =
    let
        base =
            1 / (1.6 * (1 + s.attackspeedoffset))
    in
        ( base, s.attackspeedperlevel )


crit : Model -> ( Float, Float )
crit (Model s) =
    ( s.crit, s.critperlevel )


hp : Model -> ( Float, Float )
hp (Model s) =
    ( s.hp, s.hpperlevel )


hpregen : Model -> ( Float, Float )
hpregen (Model s) =
    ( s.hpregen, s.hpregenperlevel )


movespeed : Model -> Float
movespeed (Model s) =
    s.movespeed


mana : Model -> ( Float, Float )
mana (Model s) =
    ( s.mp, s.mpperlevel )


manaregen : Model -> ( Float, Float )
manaregen (Model s) =
    ( s.mpregen, s.mpregenperlevel )


magicresist : Model -> ( Float, Float )
magicresist (Model s) =
    ( s.spellblock, s.spellblockperlevel )
