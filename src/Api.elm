port module Api exposing (Response, application, delete, get, login, logout, post, put, username, viewerChanges)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser
import Browser.Navigation as Nav
import Cred exposing (Cred(..))
import Debug
import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Maybe
import Role exposing (Role)
import Tokens exposing (Tokens)
import Url exposing (Url)
import Username exposing (Username)
import Viewer exposing (Viewer)



-- CRED


username : Cred -> Username
username (Cred val _ _) =
    val


credHeader : Cred -> Http.Header
credHeader (Cred _ _ tokens) =
    Http.header "authorization" ("Bearer " ++ tokens.accessToken)


role : Cred -> Role
role (Cred _ val _) =
    val



-- PERSISTENCE


decode : Value -> Result Decode.Error Viewer
decode value =
    Decode.decodeValue Decode.string value
        |> Result.andThen
            (\str ->
                Decode.decodeString
                    (Decode.field "user" (decoderFromCred Viewer.decoder))
                    str
            )


port onStoreChange : (Maybe String -> msg) -> Sub msg


viewerChanges : (Maybe Viewer -> msg) -> Sub msg
viewerChanges toMsg =
    onStoreChange (\value -> toMsg (decodeFromChange value))


decodeFromChange : Maybe String -> Maybe Viewer
decodeFromChange val =
    case val of
        Just str ->
            Decode.decodeString storageDecoder str
                |> Result.toMaybe

        Nothing ->
            Nothing



-- SERIALIZATION
-- APPLICATION


type alias ProgramDescription model msg =
    { init : Maybe Viewer -> Url -> Nav.Key -> ( model, Cmd msg )
    , onUrlChange : Url -> msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , subscriptions : model -> Sub msg
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    }


application :
    ProgramDescription model msg
    -> Program Value model msg
application config =
    let
        init flags url navKey =
            let
                maybeViewer =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString storageDecoder)
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


storageDecoder : Decoder Viewer
storageDecoder =
    Decode.field "user" (decoderFromCred Viewer.decoder)



-- HTTP


type alias Response a =
    Result ErrorPayload a


type alias ErrorPayload =
    { message : String
    , name : String
    }


errorDecoder : Decode.Decoder ErrorPayload
errorDecoder =
    Decode.map2 ErrorPayload
        (Decode.field "message" Decode.string)
        (Decode.field "type" Decode.string)


expectJson : (Response a -> msg) -> Decode.Decoder a -> Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err <| ErrorPayload "Bad url" "BadUrl"

                Http.Timeout_ ->
                    Err <| ErrorPayload "Timeout" "Timeout"

                Http.NetworkError_ ->
                    Err <| ErrorPayload "Network error" "NetworkError"

                Http.BadStatus_ _ body ->
                    case Decode.decodeString errorDecoder body of
                        Ok value ->
                            Err value

                        Err err ->
                            Err <| ErrorPayload "Bad response" "BadResponse"

                Http.GoodStatus_ _ body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err <| ErrorPayload "Bad response" "BadResponse"


get :
    Endpoint
    -> Maybe Cred
    -> (Response a -> msg)
    -> Decoder a
    -> Cmd msg
get url maybeCred msg decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = expectJson msg decoder
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
    -> (Response a -> msg)
    -> Decoder a
    -> Cmd msg
put url cred body msg decoder =
    Endpoint.request
        { method = "PUT"
        , url = url
        , expect = expectJson msg decoder
        , headers = [ credHeader cred ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


post :
    Endpoint
    -> Maybe Cred
    -> Body
    -> (Response a -> msg)
    -> Decoder a
    -> Cmd msg
post url maybeCred body msg decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = expectJson msg decoder
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
    -> (Response a -> msg)
    -> Decoder a
    -> Cmd msg
delete url cred body msg decoder =
    Endpoint.request
        { method = "DELETE"
        , url = url
        , expect = expectJson msg decoder
        , headers = [ credHeader cred ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


login : Http.Body -> (Response Viewer -> msg) -> Cmd msg
login body toMsg =
    post Endpoint.login Nothing body toMsg (decoderFromCred Viewer.decoder)


logout : Cmd msg
logout =
    Viewer.storeCache Nothing


decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        Cred.credDecoder
