module Core exposing (..)


-- CONSTANTS


ddragon : String
ddragon =
    "http://ddragon.leagueoflegends.com/cdn"



-- Error handling


emptyModelError : String -> Result String a
emptyModelError func =
    Err <| "Error in " ++ func ++ ": empty model"
