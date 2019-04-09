module Page.TaskList exposing
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
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Modal as Modal
import Bootstrap.Text as Text
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Cred exposing (Cred)
import Data.Category exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (Decoder)
import Page.TaskList.AddCategory as AddCategory
import Page.TaskList.Category as Category
import Page.Utils exposing (..)
import Role
import Route
import Session exposing (Session(..))
import Uuid exposing (Uuid)
import Viewer exposing (Viewer)



-- MODEL


type SubPage
    = Category Category.Model
    | AddCategory AddCategory.Model


type alias Model =
    { session : Session
    , categories : List Category
    , modalMessage : ModalMessage
    , subpage : Maybe SubPage
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , categories = []
            , modalMessage = ModalMessage Nothing
            , subpage = Nothing
            }

        role =
            Session.role session

        navKey =
            Session.navKey session
    in
    if Role.hasUserAccess role then
        ( model
        , getCategories <| Session.cred session
        )

    else
        ( model, Route.replaceUrl navKey Route.NotFound )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Задачи"
    , content =
        divWithModal model.modalMessage CloseModal [ Spacing.m1 ] <|
            [ Grid.container
                [ class "content", Spacing.p3 ]
                [ Grid.row
                    []
                    [ Grid.col
                        [ Col.lg4 ]
                        [ viewAddCategory model
                        , viewNonCategory model
                        , viewCategoryList model
                        ]
                    , Grid.col
                        [ Col.lg8 ]
                        [ viewSubpage model ]
                    ]
                ]
            ]
    }


viewNonCategory : Model -> Html Msg
viewNonCategory model =
    Button.button
        [ Button.light
        , Button.attrs [ Spacing.mb3, Size.w100 ]
        , Button.onClick (ShowTasks Nothing)
        ]
        [ text "Без категории" ]


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
                Just (Category tasks) ->
                    tasks.id

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

        Just (Category tasks) ->
            Html.map CategoryMsg <| Category.view tasks

        Just (AddCategory add) ->
            Html.map AddCategoryMsg <| AddCategory.view add


viewCategoryItem : Maybe Uuid -> Category -> ListGroup.CustomItem Msg
viewCategoryItem id category =
    ListGroup.button
        ([ ListGroup.attrs
            [ Flex.block
            , Flex.justifyBetween
            , Flex.alignItemsCenter
            , onClick (ShowTasks (Just category.id))
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



-- UPDATE


type Msg
    = GotSession Session
    | CloseModal
    | GotCategories (Api.Response (List Category))
    | ShowTasks (Maybe Uuid)
    | CategoryMsg Category.Msg
    | ShowAddTask
    | AddCategoryMsg AddCategory.Msg


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
            ( { model | modalMessage = ModalMessage Nothing }, Cmd.none )

        ( GotCategories (Ok categories), _ ) ->
            ( { model | categories = categories }, Cmd.none )

        ( GotCategories (Err err), _ ) ->
            ( { model | modalMessage = ModalMessage <| Just err.message }, Cmd.none )

        ( ShowTasks id, _ ) ->
            let
                ( subpage, cmd ) =
                    Category.init model.session id
            in
            ( { model | subpage = Just <| Category subpage }
            , Cmd.map CategoryMsg cmd
            )

        ( CategoryMsg (Category.SaveResponse (Ok x)), Just (Category tasks) ) ->
            let
                ( subpage, cmd ) =
                    Category.update (Category.SaveResponse (Ok x)) tasks
            in
            ( { model | subpage = Just <| Category subpage }
            , Cmd.batch
                [ getCategories <| Session.cred model.session
                , Cmd.map CategoryMsg cmd
                ]
            )

        ( CategoryMsg (Category.DeleteResponse (Ok x)), Just (Category tasks) ) ->
            ( { model | subpage = Nothing }
            , getCategories <| Session.cred model.session
            )

        ( CategoryMsg tasksMsg, Just (Category tasks) ) ->
            let
                ( subpage, cmd ) =
                    Category.update tasksMsg tasks
            in
            ( { model | subpage = Just <| Category subpage }, Cmd.map CategoryMsg cmd )

        ( ShowAddTask, _ ) ->
            let
                ( subpage, cmd ) =
                    AddCategory.init model.session
            in
            ( { model | subpage = Just <| AddCategory subpage }
            , Cmd.batch
                [ Cmd.map AddCategoryMsg cmd
                ]
            )

        ( AddCategoryMsg (AddCategory.Added (Ok cat)), Just (AddCategory add) ) ->
            let
                addMsg =
                    AddCategory.Added (Ok cat)

                ( subpage, cmd ) =
                    AddCategory.update addMsg add
            in
            ( { model | subpage = Just <| AddCategory subpage }
            , Cmd.batch
                [ getCategories <| Session.cred model.session
                , Cmd.map AddCategoryMsg cmd
                ]
            )

        ( AddCategoryMsg addMsg, Just (AddCategory add) ) ->
            let
                ( subpage, cmd ) =
                    AddCategory.update addMsg add
            in
            ( { model | subpage = Just <| AddCategory subpage }, Cmd.map AddCategoryMsg cmd )

        ( _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


categoriesDecoder : Decoder (List Category)
categoriesDecoder =
    D.field "categories" (D.list categoryDecoder)


getCategories : Maybe Cred -> Cmd Msg
getCategories cred =
    Api.get Endpoint.listCategories cred GotCategories categoriesDecoder



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
