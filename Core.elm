module Core exposing (..)

import Json.Decode exposing (..)
import Task exposing (Task)
import Http

-- TYPES
type alias Request a = Task Http.Error a
 
-- CONSTANTS

ddragon : String
ddragon = "http://ddragon.leagueoflegends.com/cdn"
 

  
-- for convenience:  

infixl 4 <$>
(<$>) : (a -> b) -> Decoder a -> Decoder b
(<$>) = map

infixl 4 <+>
(<+>) : Decoder (a -> b) -> Decoder a -> Decoder b
(<+>) func val = object2 (<|) func val    