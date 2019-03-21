module Tokens exposing (Tokens, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type AccessToken
    = AccessToken String


decodeAccessToken : Decoder AccessToken
decodeAccessToken =
    Decode.map AccessToken (Decode.field "access_token" Decode.string)


encodeAccessToken : AccessToken -> Value
encodeAccessToken (AccessToken at) =
    Encode.string at


type RefreshToken
    = RefreshToken String


decodeRefreshToken : Decoder RefreshToken
decodeRefreshToken =
    Decode.map RefreshToken (Decode.field "refresh_token" Decode.string)


encodeRefreshToken : RefreshToken -> Value
encodeRefreshToken (RefreshToken at) =
    Encode.string at


type alias Tokens =
    { accessToken : String
    , refreshToken : String
    }


decoder : Decoder Tokens
decoder =
    Decode.map2 Tokens
        (Decode.field "access_token" Decode.string)
        (Decode.field "refresh_token" Decode.string)


encode : Tokens -> Value
encode tokens =
    Encode.object
        [ ( "access_token", Encode.string tokens.accessToken )
        , ( "refresh_token", Encode.string tokens.refreshToken )
        ]
