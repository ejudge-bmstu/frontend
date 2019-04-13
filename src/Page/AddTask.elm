module Page.AddTask exposing
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
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Spacing as Spacing
import Cred exposing (Cred)
import Data.ReportAccess exposing (..)
import File exposing (File)
import File.Select as File
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
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
    , task : Task
    , categories : List Category
    , modalMessage : ModalMessage
    }


type alias Task =
    { name : String
    , description : String
    , category : Maybe Uuid
    , access : Maybe ReportAccess
    , pythonLimits : Limit
    , cLimits : Limit
    , cppLimits : Limit
    , tests : Maybe File
    , examples : List ExampleTest
    }


type alias ValidTask =
    { name : String
    , description : String
    , category : Uuid
    , access : ReportAccess
    , pythonLimits : Limit
    , cLimits : Limit
    , cppLimits : Limit
    , tests : File
    , examples : List ExampleTest
    }


type alias ExampleTest =
    { input : String
    , output : String
    }


type alias Category =
    { id : Uuid
    , name : String
    }


type alias Limit =
    { memory : Int
    , time : Int
    }


validateTask : Task -> Maybe ValidTask
validateTask task =
    case ( task.category, task.tests, task.access ) of
        ( Just category, Just tests, Just access ) ->
            Just
                { name = task.name
                , description = task.description
                , category = category
                , access = access
                , pythonLimits = task.pythonLimits
                , cLimits = task.cLimits
                , cppLimits = task.cppLimits
                , tests = tests
                , examples = task.examples
                }

        _ ->
            Nothing


categoryDecoder : Decoder Category
categoryDecoder =
    D.map2 Category
        (D.field "id" Uuid.decoder)
        (D.field "name" D.string)


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , categories = []
            , modalMessage = ModalMessage Nothing
            , task =
                { name = ""
                , description = ""
                , category = Nothing
                , access = Nothing
                , pythonLimits = { memory = 0, time = 0 }
                , cLimits = { memory = 0, time = 0 }
                , cppLimits = { memory = 0, time = 0 }
                , tests = Nothing
                , examples = []
                }
            }

        role =
            Session.role session

        navKey =
            Session.navKey session
    in
    if Role.hasAdminAccess role then
        ( model, getCategories (Session.cred session) )

    else
        ( model, Route.replaceUrl navKey Route.NotFound )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Добавление задачи"
    , content =
        case model.categories of
            [] ->
                divWithModal
                    (ModalMessage (Just "Добавьте хотя бы одну категорию"))
                    CloseModalRedirect
                    []
                    []

            _ ->
                divWithModal model.modalMessage CloseModal [ Spacing.mt3 ] <|
                    [ viewForm model
                    ]
    }


viewForm : Model -> Html Msg
viewForm model =
    let
        task =
            model.task

        selectedCategoryId =
            Maybe.withDefault [] <|
                Maybe.map (List.singleton << Select.id << Uuid.toString) task.category

        selectedAccessId =
            Maybe.withDefault [] <|
                Maybe.map (List.singleton << Select.id << reportAccessToString) task.access

        buttonFiles =
            case task.tests of
                Just file ->
                    File.name file

                Nothing ->
                    "Выбрать"

        examplesView =
            List.indexedMap
                (\ix ex ->
                    Form.row []
                        [ Form.col [ Col.offsetSm2, Col.sm4 ]
                            [ Textarea.textarea
                                [ Textarea.value <|
                                    Maybe.withDefault "" <|
                                        Maybe.map .input <|
                                            List.getAt ix model.task.examples
                                , Textarea.onInput <| EnterInput ix
                                ]
                            ]
                        , Form.col [ Col.sm4 ]
                            [ Textarea.textarea
                                [ Textarea.value <|
                                    Maybe.withDefault "" <|
                                        Maybe.map .output <|
                                            List.getAt ix model.task.examples
                                , Textarea.onInput <| EnterOutput ix
                                ]
                            ]
                        , Form.col [ Col.sm2 ] [ Button.button [ Button.warning, Button.onClick (DeleteExample ix) ] [ text "Удалить" ] ]
                        ]
                )
                model.task.examples
    in
    Grid.container []
        [ Form.form [] <|
            [ Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Название задачи" ]
                , Form.col [ Col.sm10 ]
                    [ Input.text
                        [ Input.attrs [ required True, minlength 1 ]
                        , Input.value task.name
                        , Input.onInput NameEntered
                        ]
                    ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Описание" ]
                , Form.col [ Col.sm10 ]
                    [ Textarea.textarea
                        [ Textarea.value task.description
                        , Textarea.onInput DescriptionEntered
                        ]
                    ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Категория" ]
                , Form.col [ Col.sm4 ]
                    [ Select.select
                        ([ Select.onChange CategorySelected ] ++ selectedCategoryId)
                      <|
                        [ Select.item [] [ text "" ] ]
                            ++ List.map
                                (\cat ->
                                    Select.item
                                        [ value <| Uuid.toString cat.id ]
                                        [ text cat.name ]
                                )
                                model.categories
                    ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Доступ к отчету" ]
                , Form.col [ Col.sm4 ]
                    [ Select.select
                        ([ Select.onChange ReportAccessSelected ] ++ selectedAccessId)
                      <|
                        [ Select.item [] [ text "" ]
                        , Select.item
                            [ value <| reportAccessToString FullAccess ]
                            [ text <| reportAccessPrettyPrint FullAccess ]
                        , Select.item
                            [ value <| reportAccessToString NoAccess ]
                            [ text <| reportAccessPrettyPrint NoAccess ]
                        ]
                    , Form.help []
                        [ text <|
                            "При полном доступе пользователь увидит отчет о проблемах, "
                                ++ "возникших на этапе проверки.\n"
                                ++ "При ограниченном пользователь увидит только "
                                ++ "число пройденных задач."
                        ]
                    ]
                ]
            , Form.row []
                [ Form.col [ Col.offsetSm2, Col.sm4 ] [ text "Пример входных даннных" ]
                , Form.col [ Col.sm4 ] [ text "Пример выходных даннных" ]
                , Form.col [ Col.sm2 ] [ Button.button [ Button.primary, Button.onClick AddExample ] [ text "Добавить" ] ]
                ]
            ]
                ++ examplesView
                ++ [ Form.row
                        []
                        [ Form.col [ Col.offsetSm2, Col.sm2 ] [ text "Ограничение по времени (c)" ]
                        , Form.col [ Col.sm2 ] [ text "Ограничение по памяти (мб)" ]
                        ]
                   , Form.row []
                        [ Form.colLabel [ Col.sm2 ] [ text "Python" ]
                        , Form.col [ Col.sm2 ]
                            [ Input.number
                                [ Input.value <| String.fromInt task.pythonLimits.time
                                , Input.onInput <| LimitEneterd Python Time
                                ]
                            ]
                        , Form.col [ Col.sm2 ]
                            [ Input.number
                                [ Input.value <| String.fromInt task.pythonLimits.memory
                                , Input.onInput <| LimitEneterd Python Memory
                                ]
                            ]
                        ]
                   , Form.row []
                        [ Form.colLabel [ Col.sm2 ] [ text "C" ]
                        , Form.col [ Col.sm2 ]
                            [ Input.number
                                [ Input.value <| String.fromInt task.cLimits.time
                                , Input.onInput <| LimitEneterd C Time
                                ]
                            ]
                        , Form.col [ Col.sm2 ]
                            [ Input.number
                                [ Input.value <| String.fromInt task.cLimits.memory
                                , Input.onInput <| LimitEneterd C Memory
                                ]
                            ]
                        ]
                   , Form.row []
                        [ Form.colLabel [ Col.sm2 ] [ text "C++" ]
                        , Form.col [ Col.sm2 ]
                            [ Input.number
                                [ Input.value <| String.fromInt task.cppLimits.time
                                , Input.onInput <| LimitEneterd Cpp Time
                                ]
                            ]
                        , Form.col [ Col.sm2 ]
                            [ Input.number
                                [ Input.value <| String.fromInt task.cppLimits.memory
                                , Input.onInput <| LimitEneterd Cpp Memory
                                ]
                            ]
                        ]
                   , Form.row []
                        [ Form.colLabel [ Col.sm2 ] [ text "Тесты" ]
                        , Form.col [ Col.sm4 ]
                            [ Button.button
                                [ Button.primary, Button.onClick TestsEntered ]
                                [ text buttonFiles ]
                            , Form.help []
                                [ text <|
                                    "Архив должен быть валидным. Валидным считается "
                                        ++ "архив, в котором содержится N папок, в "
                                        ++ "каждой из которых два файла: in.txt и "
                                        ++ "out.txt. Названия папок произвольные."
                                ]
                            ]
                        ]
                   , Form.row [ Row.rightSm ]
                        [ Form.col [ Col.sm2 ]
                            [ Button.button
                                [ Button.primary
                                , Button.attrs [ class "float-right" ]
                                , Button.onClick SendTask
                                ]
                                [ text "Отправить" ]
                            ]
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
        |> Modal.h3 [] [ text "Сообщение" ]
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
    = GotCategories (Api.Response (List Category))
    | GotSession Session
    | NameEntered String
    | DescriptionEntered String
    | CategorySelected String
    | ReportAccessSelected String
    | LimitEneterd Language LimitType String
    | TestsEntered
    | TestsSelected File
    | SendTask
    | SendTaskResponse (Api.Response ())
    | CloseModal
    | CloseModalRedirect
    | EnterInput Int String
    | EnterOutput Int String
    | DeleteExample Int
    | AddExample


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCategories (Ok categories) ->
            ( { model | categories = categories }
            , Cmd.none
            )

        GotCategories (Err error) ->
            ( { model | modalMessage = ModalMessage <| Just error.message }
            , Cmd.none
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        NameEntered val ->
            updateForm (\task -> { task | name = val }) model

        DescriptionEntered val ->
            updateForm (\task -> { task | description = val }) model

        CategorySelected val ->
            updateForm (\task -> { task | category = Uuid.fromString val }) model

        ReportAccessSelected val ->
            updateForm (\task -> { task | access = reportAccessFromString val }) model

        LimitEneterd language limitType val ->
            let
                limitMemoryUpdate limit =
                    { limit | memory = Maybe.withDefault 0 <| String.toInt val }

                limitTimeUpdate limit =
                    { limit | time = Maybe.withDefault 0 <| String.toInt val }
            in
            case ( language, limitType ) of
                ( Python, Memory ) ->
                    let
                        limits =
                            limitMemoryUpdate model.task.pythonLimits
                    in
                    updateForm (\task -> { task | pythonLimits = limits }) model

                ( Python, Time ) ->
                    let
                        limits =
                            limitTimeUpdate model.task.pythonLimits
                    in
                    updateForm (\task -> { task | pythonLimits = limits }) model

                ( C, Memory ) ->
                    let
                        limits =
                            limitMemoryUpdate model.task.cLimits
                    in
                    updateForm (\task -> { task | cLimits = limits }) model

                ( C, Time ) ->
                    let
                        limits =
                            limitTimeUpdate model.task.cLimits
                    in
                    updateForm (\task -> { task | cLimits = limits }) model

                ( Cpp, Memory ) ->
                    let
                        limits =
                            limitMemoryUpdate model.task.cppLimits
                    in
                    updateForm (\task -> { task | cppLimits = limits }) model

                ( Cpp, Time ) ->
                    let
                        limits =
                            limitTimeUpdate model.task.cppLimits
                    in
                    updateForm (\task -> { task | cppLimits = limits }) model

        TestsEntered ->
            ( model, File.file [ "application/zip" ] TestsSelected )

        TestsSelected file ->
            updateForm (\task -> { task | tests = Just file }) model

        SendTask ->
            let
                mValidTask =
                    validateTask model.task
            in
            case mValidTask of
                Just validTask ->
                    ( model, sendTask (Session.cred model.session) validTask )

                Nothing ->
                    ( { model | modalMessage = ModalMessage <| Just "Не все данные введены" }, Cmd.none )

        SendTaskResponse (Ok _) ->
            ( model, Cmd.none )

        SendTaskResponse (Err err) ->
            ( { model | modalMessage = ModalMessage <| Just err.message }, Cmd.none )

        CloseModal ->
            ( { model | modalMessage = ModalMessage Nothing }, Cmd.none )

        CloseModalRedirect ->
            ( { model | modalMessage = ModalMessage Nothing }, Route.replaceUrl (Session.navKey model.session) Route.TaskList )

        EnterInput ix inp ->
            let
                task =
                    model.task

                examples =
                    task.examples

                mExample =
                    List.getAt ix examples

                updExample =
                    Maybe.map (\ex -> { ex | input = inp }) mExample

                updExamples =
                    case updExample of
                        Just example ->
                            List.setAt ix example examples

                        Nothing ->
                            examples

                updTask =
                    { task | examples = updExamples }
            in
            ( { model | task = updTask }, Cmd.none )

        EnterOutput ix outp ->
            let
                task =
                    model.task

                examples =
                    task.examples

                mExample =
                    List.getAt ix examples

                updExample =
                    Maybe.map (\ex -> { ex | output = outp }) mExample

                updExamples =
                    case updExample of
                        Just example ->
                            List.setAt ix example examples

                        Nothing ->
                            examples

                updTask =
                    { task | examples = updExamples }
            in
            ( { model | task = updTask }, Cmd.none )

        AddExample ->
            let
                task =
                    model.task

                examples =
                    task.examples

                updExamples =
                    List.append examples [ { input = "", output = "" } ]

                updTask =
                    { task | examples = updExamples }
            in
            ( { model | task = updTask }, Cmd.none )

        DeleteExample ix ->
            let
                task =
                    model.task

                examples =
                    task.examples

                updExamples =
                    List.removeAt ix examples

                updTask =
                    { task | examples = updExamples }
            in
            ( { model | task = updTask }, Cmd.none )


updateForm : (Task -> Task) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | task = transform model.task }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- HTTP


categoriesDecoder : Decoder (List Category)
categoriesDecoder =
    D.field "categories" (D.list categoryDecoder)


getCategories : Maybe Cred -> Cmd Msg
getCategories cred =
    Api.get Endpoint.listCategories cred GotCategories categoriesDecoder


sendTask : Maybe Cred -> ValidTask -> Cmd Msg
sendTask cred task =
    let
        body =
            Http.multipartBody <|
                [ Http.stringPart "name" task.name
                , Http.stringPart "description" task.description
                , Http.stringPart "category" (Uuid.toString task.category)
                , Http.stringPart "time_limit_c" (String.fromInt task.cLimits.time)
                , Http.stringPart "memory_limit_c" (String.fromInt task.cLimits.memory)
                , Http.stringPart "time_limit_cpp" (String.fromInt task.cppLimits.time)
                , Http.stringPart "memory_limit_cpp" (String.fromInt task.cppLimits.memory)
                , Http.stringPart "time_limit_python" (String.fromInt task.pythonLimits.time)
                , Http.stringPart "memory_limit_python" (String.fromInt task.pythonLimits.memory)
                , Http.stringPart "access_report" (reportAccessToString task.access)
                , Http.filePart "tests" task.tests
                ]
                    ++ inputs
                    ++ outputs

        inputs =
            List.map (Http.stringPart "inputs[]" << .input) task.examples

        outputs =
            List.map (Http.stringPart "ouputs[]" << .output) task.examples
    in
    Api.postExpectEmpty Endpoint.addTask cred body SendTaskResponse



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
