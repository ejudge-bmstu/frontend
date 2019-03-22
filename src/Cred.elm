module Cred exposing (Cred(..), credDecoder)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser
import Browser.Navigation as Nav
import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import RemoteData exposing (..)
import Role exposing (Role)
import Tokens exposing (Tokens)
import Url exposing (Url)
import Username exposing (Username)



-- CRED


type Cred
    = Cred Username Role Tokens


credDecoder : Decoder Cred
credDecoder =
    Decode.succeed Cred
        |> required "username" Username.decoder
        |> required "role" Role.decoder
        |> required "tokens" Tokens.decoder
