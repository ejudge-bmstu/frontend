module Role exposing (Role(..), decoder, encode, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- TYPES


type Role
    = Role String



-- CREATE


decoder : Decoder Role
decoder =
    Decode.map Role Decode.string



-- TRANSFORM


encode : Role -> Value
encode (Role role) =
    Encode.string role


toString : Role -> String
toString (Role role) =
    role
