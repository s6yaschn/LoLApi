module Endpoint exposing (Model, region, platformID, host, global, br, eune, euw)


type Model
    = Model
        { region : String
        , platformID : String
        , host : String
        }


region : Model -> String
region (Model { region }) =
    region


platformID : Model -> String
platformID (Model { platformID }) =
    platformID


host : Model -> String
host (Model { host }) =
    host


global : Model
global =
    Model
        { region = "global"
        , platformID = ""
        , host = "global.api.pvp.net"
        }


br : Model
br =
    Model
        { region = "br"
        , platformID = "br1"
        , host = "br.api.pvp.net"
        }

 
eune : Model
eune =
    Model
        { region = "eune"
        , platformID = "eun1"
        , host = "eune.api.pvp.net"
        }


euw : Model
euw =
    Model
        { region = "euw"
        , platformID = "euw1"
        , host = "euw.api.pvp.net"
        }
