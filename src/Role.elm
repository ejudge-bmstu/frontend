module Role exposing (Role(..), decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- TYPES


type Role
    = Admin
    | User



-- CREATE


decoder : Decoder Role
decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "admin" ->
                        Decode.succeed Admin

                    "user" ->
                        Decode.succeed User

                    _ ->
                        Decode.fail <| "Unknown role"
            )



-- TRANSFORM


encode : Role -> Value
encode role =
    case role of
        Admin ->
            Encode.string "admin"

        User ->
            Encode.string "user"
