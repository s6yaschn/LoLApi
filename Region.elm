module Region exposing (Endpoint, region, platformID, host, global, br, eune, euw)

type Endpoint = Endpoint
    { region: String
    , platformID: String
    , host: String
    }


region: Endpoint -> String
region (Endpoint x) = x.region

platformID: Endpoint -> String
platformID (Endpoint x) = x.platformID

host: Endpoint -> String
host (Endpoint x) = x.host

global: Endpoint
global = Endpoint 
    { region = "global"
    , platformID = ""
    , host = "global.api.pvp.net"
    }

br : Endpoint
br = Endpoint 
    { region = "br"
    , platformID = "br1"
    , host = "br.api.pvp.net"
    }

eune : Endpoint
eune = Endpoint 
    { region = "eune"
    , platformID = "eun1"
    , host = "eune.api.pvp.net"
    }

euw : Endpoint
euw = Endpoint
    { region = "euw"
    , platformID = "euw1"
    , host = "euw.api.pvp.net"
    }