module Page.RegisterConfirm exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

--import Debug

import Api
import Api.Endpoint as Endpoint
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Page.Utils exposing (..)
import Route
import Session exposing (Session)


type alias Model =
    { session : Session
    , modalMessage : ModalMessage
    }


type Msg
    = GotSession Session
    | CompletedRegister (Api.Response ())
    | CloseModal


init : Session -> Maybe String -> ( Model, Cmd Msg )
init session maybeToken =
    case maybeToken of
        Just token ->
            ( Model session (ModalMessage <| Just "Подождите..."), registerComplete token )

        Nothing ->
            ( Model session (ModalMessage <| Just "Плохая ссылка"), Cmd.none )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Главная"
    , content = divWithModal model.modalMessage CloseModal [] []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        CompletedRegister (Ok _) ->
            ( { model | modalMessage = ModalMessage <| Just "Учетная запись подтверждена, можете войти используя введенные данные." }, Cmd.none )

        CompletedRegister (Err error) ->
            ( { model | modalMessage = ModalMessage <| Just error.message }, Cmd.none )

        CloseModal ->
            ( model, Route.replaceUrl (Session.navKey model.session) Route.Login )


registerComplete : String -> Cmd Msg
registerComplete token =
    let
        body =
            Encode.object [ ( "token", Encode.string token ) ]
                |> Http.jsonBody
    in
    Api.postExpectEmpty Endpoint.registerComplete Nothing body CompletedRegister


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


toSession : Model -> Session
toSession model =
    model.session
