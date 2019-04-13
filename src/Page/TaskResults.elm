module Page.TaskResults exposing
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
import Bootstrap.Accordion as Accordion
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Cred exposing (Cred)
import Data.Date exposing (..)
import Data.TaskResult exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (Decoder)
import Page.Utils exposing (..)
import Role
import Route
import Session exposing (Session)
import Task
import Time
import Uuid exposing (Uuid)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { zone : Time.Zone
    , session : Session
    , tasksResults : Maybe TasksResults
    , accordionState : Accordion.State
    , modalMessage : ModalMessage
    }


type alias TasksResults =
    List TaskResult


tasksResultsDecoder : Decoder TasksResults
tasksResultsDecoder =
    D.list taskResultDecoder


type alias Limit =
    { language : String
    , memory : Int
    , time : Int
    }


limitDecoder : Decoder Limit
limitDecoder =
    D.map3 Limit
        (D.field "language" D.string)
        (D.field "memory" D.int)
        (D.field "time" D.int)


init : Uuid -> Session -> ( Model, Cmd Msg )
init id session =
    let
        model =
            { session = session
            , tasksResults = Nothing
            , accordionState = Accordion.initialState
            , zone = Time.utc
            , modalMessage = ModalMessage Nothing
            }

        role =
            Session.role session

        navKey =
            Session.navKey session
    in
    if Role.hasUserAccess role then
        ( model
        , Cmd.batch
            [ taskResults (Session.cred session) id
            , Task.perform AdjustTimeZone Time.here
            ]
        )

    else
        ( model, Route.replaceUrl navKey Route.NotFound )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Результаты"
    , content =
        divWithModal model.modalMessage CloseModal [ Spacing.m1 ] <|
            [ case model.tasksResults of
                Just task ->
                    viewResults model task

                Nothing ->
                    div [] []
            ]
    }


viewResults : Model -> TasksResults -> Html Msg
viewResults model tasksResults =
    Grid.container [ class "content", Spacing.p3 ]
        [ Grid.row []
            [ Grid.col [ Col.md12 ]
                [ Table.table
                    { options = [ Table.striped, Table.hover ]
                    , thead =
                        Table.simpleThead
                            [ Table.th [] [ text "Пользователь" ]
                            , Table.th [] [ text "Задача" ]
                            , Table.th [] [ text "Пройдено" ]
                            , Table.th [] [ text "Результат" ]
                            , Table.th [] [ text "Сообщение" ]
                            , Table.th [] [ text "Дата" ]
                            ]
                    , tbody =
                        Table.tbody [] <| List.indexedMap (taskView model) tasksResults
                    }
                ]
            ]
        ]


taskView : Model -> Int -> TaskResult -> Table.Row Msg
taskView model ix task =
    let
        messageBlock =
            case task.message of
                Just message ->
                    [ Accordion.block []
                        [ Block.text [] [ text <| "Сообщение: " ++ message ] ]
                    ]

                Nothing ->
                    []
    in
    Table.tr []
        [ Table.td [] [ text task.userName ]
        , Table.td [] [ a [ Route.href <| Route.Task task.taskId ] [ text task.taskName ] ]
        , Table.td [] [ text <| mkResult task.passed task.total ]
        , Table.td [] [ text task.result ]
        , Table.td [] [ text <| Maybe.withDefault "не доступно" task.message ]
        , Table.td [] [ text <| mkDate model.zone task.date ]
        ]


mkResult : Maybe Int -> Int -> String
mkResult passed_ total_ =
    let
        passed =
            Maybe.withDefault "?" <| Maybe.map String.fromInt passed_

        total =
            String.fromInt total_
    in
    passed ++ "/" ++ total



-- UPDATE


type Msg
    = GotTasksResults (Api.Response TasksResults)
    | GotSession Session
    | CloseModal
    | AccordionMsg Accordion.State
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTasksResults (Ok task) ->
            ( { model | tasksResults = Just task }
            , Cmd.none
            )

        GotTasksResults (Err error) ->
            ( { model | modalMessage = ModalMessage <| Just error.message }
            , Cmd.none
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        CloseModal ->
            ( { model | modalMessage = ModalMessage Nothing }, Cmd.none )

        AccordionMsg state ->
            ( { model | accordionState = state }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Session.changes GotSession (Session.navKey model.session)
        , Accordion.subscriptions model.accordionState AccordionMsg
        ]



-- HTTP


taskResults : Maybe Cred -> Uuid -> Cmd Msg
taskResults cred id =
    Api.get (Endpoint.taskResults id) cred GotTasksResults tasksResultsDecoder



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
