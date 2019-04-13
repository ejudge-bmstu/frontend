module Page.Task exposing
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
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table
import Cred exposing (Cred)
import Data.ReportAccess exposing (..)
import Data.Task exposing (..)
import File exposing (File)
import File.Select as File
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List.Extra as List
import Page.Utils exposing (..)
import Role
import Route
import Session exposing (Session)
import Uuid exposing (Uuid)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , task : Maybe Task
    , file : Maybe File
    , modalMessage : ModalMessage
    , id : Uuid
    }


init : Session -> Uuid -> ( Model, Cmd Msg )
init session id =
    let
        model =
            { session = session
            , modalMessage = ModalMessage Nothing
            , task = Nothing
            , file = Nothing
            , id = id
            }

        role =
            Session.role session

        navKey =
            Session.navKey session
    in
    if Role.hasUserAccess role then
        ( model, getTask (Session.cred session) id )

    else
        ( model, Route.replaceUrl navKey Route.NotFound )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Добавление задачи"
    , content =
        divWithModal model.modalMessage CloseModal [] <|
            [ case model.task of
                Just task ->
                    viewTask model task

                Nothing ->
                    div [] []
            ]
    }


viewTask : Model -> Task -> Html Msg
viewTask model task =
    let
        buttonFiles =
            case model.file of
                Just file ->
                    File.name file

                Nothing ->
                    "Выбрать"
    in
    Grid.container []
        [ Grid.row []
            [ Grid.col [ Col.md12 ]
                [ h2 [] [ text task.name ] ]
            ]
        , Grid.row []
            [ Grid.col [ Col.md12 ]
                [ p [] [ text task.description ] ]
            ]
        , Grid.row []
            [ Grid.col [ Col.md12 ]
                [ p [] [ text <| reportAccessPrettyPrint task.access ] ]
            ]
        , Grid.row []
            [ Grid.col [ Col.md12 ]
                [ case task.category of
                    Just category ->
                        p [] [ text category.name ]

                    Nothing ->
                        p [] [ text "Нет категории" ]
                ]
            ]
        , Grid.row []
            [ Grid.col [ Col.md12 ]
                [ Table.simpleTable
                    ( Table.simpleThead
                        [ Table.th [] [ text "Пример входных данных" ]
                        , Table.th [] [ text "Пример выходных данных" ]
                        ]
                    , Table.tbody [] <|
                        List.map
                            (\x ->
                                Table.tr []
                                    [ Table.td [] [ text x.input ]
                                    , Table.td [] [ text x.output ]
                                    ]
                            )
                            task.examples
                    )
                ]
            ]
        , Grid.row []
            [ Grid.col [ Col.md12 ]
                [ Table.simpleTable
                    ( Table.simpleThead
                        [ Table.th [] [ text "" ]
                        , Table.th [] [ text "Ограничение по времени" ]
                        , Table.th [] [ text "Ограничение по памяти" ]
                        ]
                    , Table.tbody [] <|
                        List.map
                            (\x ->
                                Table.tr []
                                    [ Table.td [] [ text x.language ]
                                    , Table.td [] [ text <| String.fromInt x.time ]
                                    , Table.td [] [ text <| String.fromInt x.memory ]
                                    ]
                            )
                            task.limits
                    )
                ]
            ]
        , Form.row []
            [ Form.colLabel [ Col.sm2 ] [ text "Решение" ]
            , Form.col [ Col.sm2 ]
                [ Button.button
                    [ Button.primary, Button.onClick SolutionEntered ]
                    [ text buttonFiles ]
                ]
            ]
        , Form.row [ Row.rightSm ]
            [ Form.col [ Col.sm2 ]
                [ Button.button
                    [ Button.primary
                    , Button.attrs [ class "float-right" ]
                    , Button.onClick SendSolution
                    ]
                    [ text "Отправить" ]
                ]
            ]
        ]



-- UPDATE


type Language
    = Python
    | C
    | Cpp


type LimitType
    = Memory
    | Time


type Msg
    = GotSession Session
    | GotTask (Api.Response Task)
    | SolutionEntered
    | SolutionSelected File
    | SendSolution
    | SendSolutionResponse (Api.Response ())
    | CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        GotTask (Ok task) ->
            ( { model | task = Just task }
            , Cmd.none
            )

        GotTask (Err err) ->
            ( { model | modalMessage = ModalMessage <| Just err.message }
            , Cmd.none
            )

        SolutionEntered ->
            ( model, File.file [] SolutionSelected )

        SolutionSelected file ->
            ( { model | file = Just file }, Cmd.none )

        SendSolution ->
            case model.file of
                Just file ->
                    ( model, sendSolution (Session.cred model.session) file model.id )

                Nothing ->
                    ( model, Cmd.none )

        SendSolutionResponse (Ok _) ->
            ( { model | modalMessage = ModalMessage <| Just "Решение отправлено" }, Cmd.none )

        SendSolutionResponse (Err err) ->
            ( { model | modalMessage = ModalMessage <| Just err.message }, Cmd.none )

        CloseModal ->
            ( { model | modalMessage = ModalMessage Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



--HTTP


getTask : Maybe Cred -> Uuid -> Cmd Msg
getTask cred id =
    Api.get (Endpoint.getTask id) cred GotTask taskDecoder


sendSolution : Maybe Cred -> File -> Uuid -> Cmd Msg
sendSolution cred file id =
    let
        body =
            Http.multipartBody <|
                [ Http.filePart "solution" file
                , Http.stringPart "id" <| Uuid.toString id
                ]
    in
    Api.postExpectEmpty Endpoint.taskSolution cred body SendSolutionResponse



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
