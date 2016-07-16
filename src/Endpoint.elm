module Endpoint
    exposing
        ( Model
        , region
        , platformID
        , host
        , global
        , br
        , eune
        , euw
        , jp
        , kr
        , lan
        , las 
        , na 
        , oce 
        , tr 
        , ru 
        , pbe
        , all
        )


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


jp : Model
jp =
    Model
        { region = "jp"
        , platformID = "jp1"
        , host = "jp.api.pvp.net"
        }


kr : Model
kr =
    Model
        { region = "kr"
        , platformID = "kr"
        , host = "kr.api.pvp.net"
        }


lan : Model
lan =
    Model
        { region = "lan"
        , platformID = "la1"
        , host = "lan.api.pvp.net"
        }

las : Model
las = Model
    { region ="las"
    , platformID = "la2"
    , host = "las.api.pvp.net"
    }

na : Model
na = Model 
    { region ="na"
    , platformID = "na1"
    , host = "na.api.pvp.net"
    }

oce: Model
oce = Model
    { region = "oce"
    , platformID = "oc1"
    , host = "oce.api.pvp.net"
    }

tr: Model
tr = Model
    { region = "tr"
    , platformID = "tr1"
    , host = "tr.api.pvp.net"
    }

ru: Model
ru = Model 
    { region = "ru"
    , platformID = "ru"
    , host = "ru.api.pvp.net"
    }

pbe : Model 
pbe = Model 
    { region = "pbe"
    , platformID = "pbe1"
    , host = "pbe.api.pvp.net"
    }


-- only lists live regions (not global / pbe)
all : List Model
all = [br, eune, euw, jp, kr, lan, las, na, oce, tr, ru]