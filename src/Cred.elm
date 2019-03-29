module Cred exposing (Cred(..), credDecoder, role, username)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Role exposing (Role)
import Tokens exposing (Tokens)
import Username exposing (Username)



-- CRED


type Cred
    = Cred Username Role Tokens


username : Cred -> Username
username (Cred val _ _) =
    val


role : Cred -> Role
role (Cred _ val _) =
    val


credDecoder : Decoder Cred
credDecoder =
    Decode.succeed Cred
        |> required "username" Username.decoder
        |> required "role" Role.decoder
        |> required "tokens" Tokens.decoder
