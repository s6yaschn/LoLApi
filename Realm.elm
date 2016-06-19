module Realm exposing (..)

import Json.Decode exposing (..)
import Dict exposing (Dict)
import Core exposing (..)
import Region

-- MODEL

type alias Realm =
    { cdn: String
    , css: String
    , dd: String
    , l: String
    , lg: String
    , n: Dict String String
    , profileiconmax: Int
    , store: String
    , v: String
    }

type Model = Model Realm
 
realm: Decoder Realm
realm =  
    Realm
    <$> "cdn" := string 
    <+> "css" := string 
    <+> "dd" := string 
    <+> "l" := string 
    <+> "lg" := string 
    <+> "n" := dict string 
    <+> "profileiconmax" := int 
    <+> oneOf ["store" := string, succeed ""] -- optional 
    <+> "v" := string


decoder : Decoder Model
decoder = map Model realm


-- ACCESSORS 

cdn : Model -> String 
cdn (Model realm) = realm.cdn

version : Model -> String 
version (Model realm) = realm.dd


