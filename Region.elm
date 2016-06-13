module Region exposing (Endpoint, region, platformID)

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
    { region = "Global"
    , platformID = ""
    , host = "global.api.pvp.net"
    }