module Page.TaskList.AddCategory exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import Api
import Api.Endpoint as Endpoint
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Modal as Modal
import Cred exposing (Cred)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Page.Utils exposing (..)
import Role
import Route
import Session exposing (Session(..))
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , category : String
    , modalMessage : ModalMessage
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , category = ""
            , modalMessage = ModalMessage Nothing
            }

        role =
            Session.role session

        navKey =
            Session.navKey session
    in
    if Role.hasUserAccess role then
        ( model, Cmd.none )

    else
        ( model, Route.replaceUrl navKey Route.NotFound )



-- VIEW


view : Model -> Html Msg
view model =
    divWithModal model.modalMessage
        CloseModal
        []
        [ Form.form []
            [ h4 [] [ text "Добавление категории" ]
            , Form.group []
                [ Input.text
                    [ Input.attrs [ placeholder "Название категории", required True ]
                    , Input.onInput EnteredCategory
                    , Input.value model.category
                    ]
                ]
            , Button.button [ Button.primary, Button.onClick SubmittedForm ] [ text "Добавить" ]
            ]
        ]



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredCategory String
    | Added (Api.Response ())
    | GotSession Session
    | CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        cred =
            Session.cred model.session
    in
    case msg of
        SubmittedForm ->
            ( model
            , sendCategory cred <| String.trim model.category
            )

        EnteredCategory category ->
            ( { model | category = category }, Cmd.none )

        Added (Ok viewer) ->
            ( { model | modalMessage = ModalMessage <| Just ("Категория " ++ model.category ++ " добавлена!"), category = "" }
            , Cmd.none
            )

        Added (Err error) ->
            ( { model | modalMessage = ModalMessage <| Just error.message }
            , Cmd.none
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        CloseModal ->
            ( { model | modalMessage = ModalMessage Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


sendCategory : Maybe Cred -> String -> Cmd Msg
sendCategory cred category =
    let
        body =
            Encode.object
                [ ( "name", Encode.string category )
                ]
                |> Http.jsonBody
    in
    Api.postExpectEmpty Endpoint.addCategory cred body Added



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
