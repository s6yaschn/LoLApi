module Core exposing (..)

import Json.Decode exposing (..)

-- CONSTANTS

ddragon : String
ddragon = "http://ddragon.leagueoflegends.com/cdn"
 
-- Error handling

emptyModelError : String -> Result String a 
emptyModelError func = Err <| "Error in " ++ func ++ ": empty model"
  
-- for convenience:  

infixl 4 <$>
(<$>) : (a -> b) -> Decoder a -> Decoder b
(<$>) = map

infixl 4 <+>
(<+>) : Decoder (a -> b) -> Decoder a -> Decoder b
(<+>) func val = object2 (<|) func val    