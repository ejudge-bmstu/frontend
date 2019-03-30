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
    , task : Task
    , categories : List Category
    , errorMessage : Maybe String
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
    }


type ReportAccess
    = FullAccess
    | NoAccess


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
            , errorMessage = Nothing
            , task =
                { name = ""
                , description = ""
                , category = Nothing
                , access = Nothing
                , pythonLimits = { memory = 0, time = 0 }
                , cLimits = { memory = 0, time = 0 }
                , cppLimits = { memory = 0, time = 0 }
                , tests = Nothing
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
        div [ class "content" ]
            [ case model.categories of
                [] ->
                    div [] []

                _ ->
                    viewForm model
            , showModal model.errorMessage
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
    in
    Grid.container []
        [ Form.form []
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
                    ]
                ]
            , Form.row []
                [ Form.col [ Col.offsetSm2, Col.sm2 ] [ text "Ограничение по времени" ]
                , Form.col [ Col.sm2 ] [ text "Ограничение по памяти" ]
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
                , Form.col [ Col.sm2 ]
                    [ Button.button
                        [ Button.primary, Button.onClick TestsEntered ]
                        [ text buttonFiles ]
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCategories (Ok categories) ->
            ( { model | categories = categories }
            , Cmd.none
            )

        GotCategories (Err error) ->
            ( { model | errorMessage = Just error.message }
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
                    ( { model | errorMessage = Just "Не все данные введены" }, Cmd.none )

        SendTaskResponse (Ok _) ->
            ( model, Cmd.none )

        SendTaskResponse (Err err) ->
            ( { model | errorMessage = Just err.message }, Cmd.none )

        CloseModal ->
            ( { model | errorMessage = Nothing }, Cmd.none )


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
            Http.multipartBody
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
    in
    Api.postExpectEmpty Endpoint.addTask cred body SendTaskResponse



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session