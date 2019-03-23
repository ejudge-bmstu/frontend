port module Viewer exposing (Viewer(..), cred, decoder, store, storeCache)

import Cred exposing (Cred(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Role exposing (Role)
import Tokens
import Username exposing (Username)



-- TYPES


type Viewer
    = Viewer Cred



-- INFO


cred : Viewer -> Cred
cred (Viewer val) =
    val



-- SERIALIZATION


decoder : Decoder (Cred -> Viewer)
decoder =
    Decode.succeed Viewer


store : Viewer -> Cmd msg
store (Viewer credVal) =
    storeCredWith credVal


storeCredWith : Cred -> Cmd msg
storeCredWith (Cred uname role token) =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "username", Username.encode uname )
                        , ( "role", Role.encode role )
                        , ( "tokens", Tokens.encode token )
                        ]
                  )
                ]
    in
    storeCache (Just json)


port storeCache : Maybe Value -> Cmd msg
