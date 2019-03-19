port module Api exposing (Cred, application, delete, get, post, put, storeCredWith, username, viewerChanges)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser
import Browser.Navigation as Nav
import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import RemoteData exposing (..)
import Tokens exposing (Tokens)
import Url exposing (Url)
import Username exposing (Username)



-- CRED


type Cred
    = Cred Username Tokens


username : Cred -> Username
username (Cred val _) =
    val


credHeader : Cred -> Http.Header
credHeader (Cred _ tokens) =
    Http.header "authorization" ("Token " ++ tokens.accessToken)


credDecoder : Decoder Cred
credDecoder =
    Decode.succeed Cred
        |> required "username" Username.decoder
        |> required "tokens" Tokens.decoder



-- PERSISTENCE


decode : Decoder (Cred -> viewer) -> Value -> Result Decode.Error viewer
decode decoder value =
    Decode.decodeValue Decode.string value
        |> Result.andThen
            (\str ->
                Decode.decodeString
                    (Decode.field "user" (decoderFromCred decoder))
                    str
            )


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Maybe viewer -> msg) -> Decoder (Cred -> viewer) -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder (Cred -> viewer) -> Value -> Maybe viewer
decodeFromChange viewerDecoder val =
    Decode.decodeValue (storageDecoder viewerDecoder) val
        |> Result.toMaybe


storeCredWith : Cred -> Cmd msg
storeCredWith (Cred uname token) =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "username", Username.encode uname )
                        , ( "tokens", Tokens.encode token )
                        ]
                  )
                ]
    in
    storeCache (Just json)


port storeCache : Maybe Value -> Cmd msg



-- SERIALIZATION
-- APPLICATION


type alias ProgramDescription model msg viewer =
    { init : Maybe viewer -> Url -> Nav.Key -> ( model, Cmd msg )
    , onUrlChange : Url -> msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    }


application :
    Decoder (Cred -> viewer)
    -> ProgramDescription model msg viewer
    -> Program Value model msg
application viewerDecoder config =
    let
        init flags url navKey =
            let
                maybeViewer =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString (storageDecoder viewerDecoder))
                        |> Result.toMaybe
            in
            config.init maybeViewer url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }


storageDecoder : Decoder (Cred -> viewer) -> Decoder viewer
storageDecoder viewerDecoder =
    Decode.field "user" (decoderFromCred viewerDecoder)



-- HTTP


get :
    Endpoint
    -> Maybe Cred
    -> (WebData a -> msg)
    -> Decoder a
    -> Cmd msg
get url maybeCred msg decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , headers =
            case maybeCred of
                Just cred ->
                    [ credHeader cred ]

                Nothing ->
                    []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


put :
    Endpoint
    -> Cred
    -> Body
    -> (WebData a -> msg)
    -> Decoder a
    -> Cmd msg
put url cred body msg decoder =
    Endpoint.request
        { method = "PUT"
        , url = url
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , headers = [ credHeader cred ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


post :
    Endpoint
    -> Maybe Cred
    -> Body
    -> (WebData a -> msg)
    -> Decoder a
    -> Cmd msg
post url maybeCred body msg decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , headers =
            case maybeCred of
                Just cred ->
                    [ credHeader cred ]

                Nothing ->
                    []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


delete :
    Endpoint
    -> Cred
    -> Body
    -> (WebData a -> msg)
    -> Decoder a
    -> Cmd msg
delete url cred body msg decoder =
    Endpoint.request
        { method = "DELETE"
        , url = url
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , headers = [ credHeader cred ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder
