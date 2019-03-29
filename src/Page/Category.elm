module Page.Category exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api
import Api.Endpoint as Endpoint
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Bootstrap.Text as Text
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser.Navigation as Nav
import Cred exposing (Cred)
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Page.Category.Add as Add
import Page.Category.Tasks as Tasks
import Role
import Route exposing (Route)
import Session exposing (Session(..))
import Uuid exposing (Uuid)
import Viewer exposing (Viewer)



-- MODEL


type SubPage
    = Tasks Tasks.Model
    | Add Add.Model


type alias Model =
    { session : Session
    , categories : List Category
    , message : Maybe String
    , subpage : Maybe SubPage
    }


init : Session -> Maybe Uuid -> Maybe Int -> ( Model, Cmd Msg )
init session categoryId page =
    let
        subpageInit =
            Maybe.map (Tasks.init session page) categoryId

        ( subpage, subCmd ) =
            case subpageInit of
                Just ( subpage_, subCmd_ ) ->
                    ( Just (Tasks subpage_), subCmd_ )

                Nothing ->
                    ( Nothing, Cmd.none )

        model =
            { session = session
            , categories = []
            , message = Nothing
            , subpage = subpage
            }

        role =
            Session.role session

        navKey =
            Session.navKey session
    in
    if Role.hasUserAccess role then
        ( model, Cmd.batch [ getCategories, Cmd.map TasksMsg subCmd ] )

    else
        ( model, Route.replaceUrl navKey Route.NotFound )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Задачи"
    , content =
        div [ Spacing.m1 ]
            [ Grid.container
                [ class "content", Spacing.p3 ]
                [ Grid.row
                    []
                    [ Grid.col
                        [ Col.lg4 ]
                        [ viewAddCategory model
                        , viewCategoryList model
                        ]
                    , Grid.col
                        [ Col.lg8 ]
                        [ viewSubpage model ]
                    ]
                ]
            , showModal model.message
            ]
    }


viewAddCategory : Model -> Html Msg
viewAddCategory model =
    let
        role =
            Session.role model.session
    in
    if Role.hasAdminAccess role then
        Button.button
            [ Button.primary, Button.attrs [ Spacing.mb3, Size.w100 ], Button.onClick ShowAddTask ]
            [ text "Добавить категорию" ]

    else
        div [] []


viewCategoryList : Model -> Html Msg
viewCategoryList model =
    let
        activeId =
            case model.subpage of
                Just (Tasks tasks) ->
                    Just tasks.id

                _ ->
                    Nothing
    in
    ListGroup.custom <|
        List.map (viewCategoryItem activeId) model.categories


viewSubpage : Model -> Html Msg
viewSubpage model =
    case model.subpage of
        Nothing ->
            div [] [ text "выберите категорию" ]

        Just (Tasks tasks) ->
            Html.map TasksMsg <| Tasks.view tasks

        Just (Add add) ->
            Html.map AddMsg <| Add.view add


viewCategoryItem : Maybe Uuid -> Category -> ListGroup.CustomItem Msg
viewCategoryItem id category =
    ListGroup.button
        ([ ListGroup.attrs
            [ Flex.block
            , Flex.justifyBetween
            , Flex.alignItemsCenter
            , onClick (ShowTasks category.id)
            ]
         ]
            ++ (if Just category.id == id then
                    [ ListGroup.active ]

                else
                    []
               )
        )
        [ text category.name
        , Badge.pillLight [] [ text <| String.fromInt category.count ]
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


type Msg
    = GotSession Session
    | CloseModal
    | GotCategories (Api.Response (List Category))
    | ShowTasks Uuid
    | TasksMsg Tasks.Msg
    | ShowAddTask
    | AddMsg Add.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        cred =
            Session.cred model.session
    in
    case ( msg, model.subpage ) of
        ( GotSession session, _ ) ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        ( CloseModal, _ ) ->
            ( { model | message = Nothing }, Cmd.none )

        ( GotCategories (Ok categories), _ ) ->
            ( { model | categories = categories }, Cmd.none )

        ( GotCategories (Err err), _ ) ->
            ( { model | message = Just err.message }, Cmd.none )

        ( ShowTasks id, _ ) ->
            let
                ( subpage, cmd ) =
                    Tasks.init model.session Nothing id
            in
            ( { model | subpage = Just <| Tasks subpage }
            , Cmd.batch
                [ Cmd.map TasksMsg cmd
                , Route.replaceUrl (Session.navKey model.session) <| Route.Category (Just id) Nothing
                ]
            )

        ( TasksMsg tasksMsg, Just (Tasks tasks) ) ->
            let
                ( subpage, cmd ) =
                    Tasks.update tasksMsg tasks
            in
            ( { model | subpage = Just <| Tasks subpage }, Cmd.map TasksMsg cmd )

        ( ShowAddTask, _ ) ->
            let
                ( subpage, cmd ) =
                    Add.init model.session
            in
            ( { model | subpage = Just <| Add subpage }
            , Cmd.batch
                [ Cmd.map AddMsg cmd
                , Route.replaceUrl (Session.navKey model.session) <| Route.Category Nothing Nothing
                ]
            )

        ( AddMsg addMsg, Just (Add add) ) ->
            let
                ( subpage, cmd ) =
                    Add.update addMsg add
            in
            ( { model | subpage = Just <| Add subpage }, Cmd.map AddMsg cmd )

        ( _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


type alias Category =
    { id : Uuid
    , name : String
    , count : Int
    }


categoryDecoder : Decoder Category
categoryDecoder =
    D.map3 Category
        (D.field "id" Uuid.decoder)
        (D.field "name" D.string)
        (D.field "count" D.int)


categoriesDecoder : Decoder (List Category)
categoriesDecoder =
    D.field "categories" (D.list categoryDecoder)


getCategories : Cmd Msg
getCategories =
    Api.get Endpoint.listCategories Nothing GotCategories categoriesDecoder



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
