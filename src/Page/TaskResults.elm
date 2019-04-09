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
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Table as Table
import Bootstrap.Text as Text
import Bootstrap.Utilities.Size as Size
import Cred exposing (Cred)
import Data.Date exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Page.Utils exposing (..)
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


decodeTime : D.Decoder Time.Posix
decodeTime =
    D.int
        |> D.andThen
            (\ms ->
                D.succeed <| Time.millisToPosix ms
            )


type alias TaskResult =
    { name : String
    , id : Uuid
    , passed : Maybe Int
    , total : Int
    , message : Maybe String
    , result : String
    , date : Time.Posix
    }


taskResultDecoder : Decoder TaskResult
taskResultDecoder =
    D.map7 TaskResult
        (D.field "name" D.string)
        (D.field "id" Uuid.decoder)
        (D.field "passed" (D.maybe D.int))
        (D.field "total" D.int)
        (D.field "message" (D.maybe D.string))
        (D.field "result" D.string)
        (D.field "date" decodeTime)


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


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , tasksResults = Nothing
      , accordionState = Accordion.initialState
      , zone = Time.utc
      , modalMessage = ModalMessage Nothing
      }
    , Cmd.batch
        [ getResults (Session.cred session)
        , Task.perform AdjustTimeZone Time.here
        ]
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Задача"
    , content =
        divWithModal model.modalMessage CloseModal [] <|
            [ case model.tasksResults of
                Just task ->
                    viewTask model task

                Nothing ->
                    div [] []
            ]
    }


viewTask : Model -> TasksResults -> Html Msg
viewTask model tasksResults =
    Grid.container []
        [ Grid.row []
            [ Grid.col [ Col.md12 ]
                [ Accordion.config AccordionMsg
                    |> Accordion.withAnimation
                    |> (Accordion.cards <| List.indexedMap (taskView model) tasksResults)
                    |> Accordion.view model.accordionState
                ]
            ]
        ]


taskView : Model -> Int -> TaskResult -> Accordion.Card Msg
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
    Accordion.card
        { id = String.fromInt ix
        , options = []
        , header =
            Accordion.header []
                (Accordion.toggle [] [ text task.name ])
        , blocks =
            [ Accordion.block []
                [ Block.text [] [ text <| "Дата прохождения: " ++ mkDate model.zone task.date ] ]
            , Accordion.block []
                [ Block.text [] [ text <| "Статус: " ++ task.result ] ]
            , Accordion.block []
                [ Block.text [] [ text <| "Число пройденных тестов: " ++ mkResult task.passed task.total ] ]
            ]
                ++ messageBlock
                ++ [ Accordion.block []
                        [ Block.link [ Route.href <| Route.Task task.id ] [ text "Перейти к задаче" ] ]
                   ]
        }


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


getResults : Maybe Cred -> Cmd Msg
getResults cred =
    Api.get Endpoint.getResults cred GotTasksResults tasksResultsDecoder



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
