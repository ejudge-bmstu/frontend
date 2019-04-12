module Page.TaskList.Category exposing
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
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Pagination as Pagination
import Bootstrap.Pagination.Item as Item
import Bootstrap.Utilities.Spacing as Spacing
import Cred exposing (Cred)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import List.Extra as List
import Page.Utils exposing (..)
import Role
import Route
import Session exposing (Session(..))
import Uuid exposing (Uuid)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , tasks : List Task
    , getTasks : Int -> Cmd Msg
    , page : Int
    , pages : Int
    , id : Maybe Uuid
    , modalMessage : ModalMessage
    , name : Maybe String
    , edit : Bool
    , editName : String
    }


init : Session -> Maybe Uuid -> ( Model, Cmd Msg )
init session mId =
    let
        getTasks_ =
            case mId of
                Just id ->
                    getTasks (Session.cred session) id

                Nothing ->
                    getTasksNonCategorized (Session.cred session)

        model =
            { session = session
            , tasks = []
            , getTasks = getTasks_
            , page = 1
            , pages = 0
            , id = mId
            , modalMessage = ModalMessage Nothing
            , name = Just ""
            , edit = False
            , editName = ""
            }

        role =
            Session.role session

        navKey =
            Session.navKey session
    in
    if Role.hasUserAccess role then
        ( model, model.getTasks 1 )

    else
        ( model, Route.replaceUrl navKey Route.NotFound )



-- VIEW


view : Model -> Html Msg
view model =
    let
        pages =
            List.filter (\x -> x >= 1 && x <= model.pages) <| List.range (model.page - 2) (model.page + 2)

        index =
            Maybe.withDefault 1 <| List.elemIndex model.page pages
    in
    Grid.container []
        [ categoryHeader model
        , Grid.row []
            [ Grid.col [ Col.md12 ]
                [ Card.columns <|
                    List.map taskView model.tasks
                , customPagination model
                , divWithModal model.modalMessage CloseModal [] []
                ]
            ]
        ]


categoryHeader : Model -> Html Msg
categoryHeader model =
    if model.edit then
        categoryEditHeader model

    else
        categoryTextHeader model


categoryTextHeader : Model -> Html Msg
categoryTextHeader model =
    let
        role =
            Session.role model.session
    in
    Grid.row []
        ([ Grid.col [ Col.md9 ]
            [ h4
                [ Spacing.mb3
                , style "line-height" "1.5"
                , style "border" "1px solid transparent"
                ]
                [ text <| Maybe.withDefault "Без категории" model.name ]
            ]
         ]
            ++ (case ( model.id, Role.hasAdminAccess role ) of
                    ( Just _, True ) ->
                        [ Grid.col [ Col.md3 ]
                            [ Button.button [ Button.onClick EditCategory ] [ text "Редактировать" ] ]
                        ]

                    ( _, _ ) ->
                        []
               )
        )


categoryEditHeader : Model -> Html Msg
categoryEditHeader model =
    Grid.row
        [ Row.attrs
            [ Spacing.mb3
            , style "line-height" "1.5"
            , style "border" "1px solid transparent"
            ]
        ]
        [ Grid.col [ Col.md6 ]
            [ Input.text [ Input.value model.editName, Input.onInput EnterCategory ] ]
        , Grid.col [ Col.md2 ]
            [ Button.button [ Button.primary, Button.onClick Save ] [ text "Сохранить" ] ]
        , Grid.col [ Col.md2 ]
            [ Button.button [ Button.danger, Button.onClick Delete ] [ text "Удалить" ] ]
        , Grid.col [ Col.md2 ]
            [ Button.button [ Button.outlinePrimary, Button.onClick Cancel ] [ text "Отмена" ] ]
        ]


customPagination : Model -> Html Msg
customPagination model =
    let
        pages =
            List.filter (\x -> x >= 1 && x <= model.pages) <| List.range (model.page - 2) (model.page + 2)

        index =
            Maybe.withDefault 1 <| List.elemIndex model.page pages
    in
    if model.pages /= 0 then
        div []
            [ Pagination.defaultConfig
                |> Pagination.ariaLabel "Pagination"
                |> Pagination.items
                    ([ Item.item
                        |> Item.disabled (model.page == 1)
                        |> Item.span []
                            [ span
                                [ class "fa fa-fast-backward"
                                , attribute "aria-hidden" "true"
                                , onClick (PaginationMsg 1)
                                ]
                                []
                            , span [ class "sr-only" ]
                                [ text "First page" ]
                            ]
                     , Item.item
                        |> Item.disabled (model.page == 1)
                        |> Item.span []
                            [ span
                                [ class "fa fa-arrow-left"
                                , attribute "aria-hidden" "true"
                                , onClick (PaginationMsg <| model.page - 1)
                                ]
                                []
                            , span [ class "sr-only" ] [ text "Previous" ]
                            ]
                     ]
                        ++ List.indexedMap
                            (\idx item ->
                                Item.item
                                    |> Item.disabled (model.page == item)
                                    |> Item.span
                                        [ onClick (PaginationMsg item) ]
                                        [ text <| String.fromInt item
                                        ]
                            )
                            pages
                        ++ [ Item.item
                                |> Item.disabled (model.page == model.pages)
                                |> Item.span []
                                    [ span
                                        [ class "fa fa-arrow-right"
                                        , attribute "aria-hidden" "true"
                                        , onClick (PaginationMsg <| model.page + 1)
                                        ]
                                        []
                                    , span [ class "sr-only" ] [ text "Next" ]
                                    ]
                           , Item.item
                                |> Item.disabled (model.page == model.pages)
                                |> Item.span []
                                    [ span
                                        [ class "fa fa-fast-forward"
                                        , attribute "aria-hidden" "true"
                                        , onClick (PaginationMsg model.pages)
                                        ]
                                        []
                                    , span [ class "sr-only" ] [ text "Last page" ]
                                    ]
                           ]
                    )
                |> Pagination.view
            ]

    else
        div [] []


taskView : Task -> Card.Config Msg
taskView task =
    Card.config []
        |> Card.block []
            [ Block.titleH5 [] [ text task.name ]
            , Block.link [ Route.href <| Route.Task task.id ] [ text "Перейти к задаче" ]
            ]


viewTaskList : Model -> Html Msg
viewTaskList model =
    div [] []



-- UPDATE


type Msg
    = GotSession Session
    | CloseModal
    | GotTasks (Api.Response TaskList)
    | PaginationMsg Int
    | EditCategory
    | EnterCategory String
    | Cancel
    | Save
    | SaveResponse (Api.Response ())
    | Delete
    | DeleteResponse (Api.Response ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        cred =
            Session.cred model.session
    in
    case msg of
        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        CloseModal ->
            ( { model | modalMessage = ModalMessage Nothing }, Cmd.none )

        GotTasks (Ok tasks) ->
            ( { model | tasks = tasks.tasks, pages = tasks.pages, name = tasks.name }, Cmd.none )

        GotTasks (Err err) ->
            ( { model | modalMessage = ModalMessage <| Just err.message }, Cmd.none )

        PaginationMsg page ->
            ( { model | page = page }, model.getTasks page )

        EditCategory ->
            ( { model | edit = True, editName = Maybe.withDefault "" model.name }, Cmd.none )

        EnterCategory name ->
            ( { model | editName = name }, Cmd.none )

        Cancel ->
            ( { model | editName = "", edit = False }, Cmd.none )

        Save ->
            case model.id of
                Just id ->
                    ( model, updateName (Session.cred model.session) id model.editName )

                Nothing ->
                    ( model, Cmd.none )

        SaveResponse (Ok _) ->
            ( { model | edit = False, name = Just model.editName, editName = "" }, Cmd.none )

        SaveResponse (Err err) ->
            ( { model | modalMessage = ModalMessage <| Just err.message }, Cmd.none )

        Delete ->
            case model.id of
                Just id ->
                    ( model, deleteCategory (Session.cred model.session) id )

                Nothing ->
                    ( model, Cmd.none )

        DeleteResponse (Ok _) ->
            ( model, Cmd.none )

        DeleteResponse (Err err) ->
            ( { model | modalMessage = ModalMessage <| Just err.message }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Task =
    { id : Uuid
    , name : String
    }


taskDecoder : Decoder Task
taskDecoder =
    D.map2 Task
        (D.field "id" Uuid.decoder)
        (D.field "name" D.string)


type alias TaskList =
    { pages : Int
    , name : Maybe String
    , tasks : List Task
    }


taskListDecoder : Decoder TaskList
taskListDecoder =
    D.map3 TaskList
        (D.field "total" D.int)
        (D.field "name" (D.maybe D.string))
        (D.field "tasks" (D.list taskDecoder))


getTasks : Maybe Cred -> Uuid -> Int -> Cmd Msg
getTasks cred id page =
    Api.get (Endpoint.listTasks (Just id) (page - 1)) cred GotTasks taskListDecoder


getTasksNonCategorized : Maybe Cred -> Int -> Cmd Msg
getTasksNonCategorized cred page =
    Api.get (Endpoint.listTasks Nothing (page - 1)) cred GotTasks taskListDecoder


updateName : Maybe Cred -> Uuid -> String -> Cmd Msg
updateName cred id name =
    let
        body =
            E.object
                [ ( "id", Uuid.encode id )
                , ( "name", E.string name )
                ]
                |> Http.jsonBody
    in
    Api.postExpectEmpty Endpoint.editCategory cred body SaveResponse


deleteCategory : Maybe Cred -> Uuid -> Cmd Msg
deleteCategory cred id =
    let
        body =
            E.object
                [ ( "id", Uuid.encode id )
                ]
                |> Http.jsonBody
    in
    Api.postExpectEmpty Endpoint.deleteCategory cred body DeleteResponse



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
