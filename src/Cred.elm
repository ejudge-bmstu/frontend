module Cred exposing (Cred(..), credDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Json.Encode as Encode
import Role exposing (Role)
import Tokens exposing (Tokens)
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
