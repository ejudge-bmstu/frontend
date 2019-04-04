module Page.SendSolution exposing
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
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Table as Table
import Bootstrap.Text as Text
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Cred exposing (Cred)
import File exposing (File)
import File.Select as File
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import List.Extra as List
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
    , errorMessage : Maybe String
    }


type alias Task =
    { name : String
    , description : String
    , category : Category
    , access : ReportAccess
    , limits : List Limit
    , examples : List ExampleTest
    }


type alias Category =
    { id : Uuid
    , name : String
    }


categoryDecoder : Decoder Category
categoryDecoder =
    D.map2 Category
        (D.field "id" Uuid.decoder)
        (D.field "name" D.string)


taskDecoder : Decoder Task
taskDecoder =
    D.map6 Task
        (D.field "name" D.string)
        (D.field "description" D.string)
        (D.field "category" categoryDecoder)
        (D.field "access" (D.map (Maybe.withDefault FullAccess << reportAccessFromString) D.string))
        (D.field "limits" (D.list limitDecoder))
        (D.field "examples" (D.list exampleDecoder))


type ReportAccess
    = FullAccess
    | NoAccess


type alias ExampleTest =
    { input : String
    , output : String
    }


exampleDecoder : Decoder ExampleTest
exampleDecoder =
    D.map2 ExampleTest
        (D.field "input" D.string)
        (D.field "output" D.string)


reportAccessToString : ReportAccess -> String
reportAccessToString ra =
    case ra of
        FullAccess ->
            "full_access"

        NoAccess ->
            "no_access"


reportAccessPrettyPrint : ReportAccess -> String
reportAccessPrettyPrint ra =
    case ra of
        FullAccess ->
            "Полный доступ"

        NoAccess ->
            "Ограниченный доступ"


reportAccessFromString : String -> Maybe ReportAccess
reportAccessFromString ra =
    case ra of
        "full_access" ->
            Just FullAccess

        "no_access" ->
            Just NoAccess

        _ ->
            Nothing


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


init : Session -> Uuid -> ( Model, Cmd Msg )
init session id =
    let
        model =
            { session = session
            , errorMessage = Nothing
            , task = Nothing
            , file = Nothing
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
        div []
            [ case model.task of
                Just task ->
                    viewTask model task

                Nothing ->
                    div [] []
            , showModal model.errorMessage
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
                [ p [] [ text task.category.name ] ]
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


showModal : Maybe String -> Html Msg
showModal maybeMessage =
    let
        ( modalVisibility, message ) =
            case maybeMessage of
                Just message_ ->
                    ( Modal.shown, message_ )

                Nothing ->
                    ( Modal.hidden, "" )
    in
    Modal.config CloseModal
        |> Modal.small
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Ошибка" ]
        |> Modal.body [] [ p [] [ text message ] ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick CloseModal ]
                ]
                [ text "Закрыть" ]
            ]
        |> Modal.view modalVisibility



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
            ( { model | errorMessage = Just err.message }
            , Cmd.none
            )

        SolutionEntered ->
            ( model, File.file [] SolutionSelected )

        SolutionSelected file ->
            ( { model | file = Just file }, Cmd.none )

        SendSolution ->
            case model.file of
                Just file ->
                    ( model, sendSolution (Session.cred model.session) file )

                Nothing ->
                    ( model, Cmd.none )

        SendSolutionResponse (Ok _) ->
            ( { model | errorMessage = Just "Решиние отправлено" }, Cmd.none )

        SendSolutionResponse (Err err) ->
            ( { model | errorMessage = Just err.message }, Cmd.none )

        CloseModal ->
            ( { model | errorMessage = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



--HTTP


getTask : Maybe Cred -> Uuid -> Cmd Msg
getTask cred id =
    Api.get (Endpoint.getTask id) cred GotTask taskDecoder


sendSolution : Maybe Cred -> File -> Cmd Msg
sendSolution cred file =
    let
        body =
            Http.multipartBody <|
                [ Http.filePart "solution" file
                ]
    in
    Api.postExpectEmpty Endpoint.taskSolution cred body SendSolutionResponse



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
