module Role exposing (Role(..), decoder, encode, hasAdminAccess, hasUserAccess)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- TYPES


type Role
    = Admin
    | User
    | Guest



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

                    "guest" ->
                        Decode.succeed Guest

                    _ ->
                        Decode.fail <| "Unknown role"
            )


encode : Role -> Value
encode role =
    case role of
        Admin ->
            Encode.string "admin"

        User ->
            Encode.string "user"

        Guest ->
            Encode.string "guest"


hasAccess : Role -> Role -> Bool
hasAccess access user =
    case ( access, user ) of
        ( _, Admin ) ->
            True

        ( User, User ) ->
            True

        ( Guest, User ) ->
            True

        ( Guest, Guest ) ->
            True

        ( _, _ ) ->
            False


hasAdminAccess : Role -> Bool
hasAdminAccess =
    hasAccess Admin


hasUserAccess : Role -> Bool
hasUserAccess =
    hasAccess User
