module Page.Category.Tasks exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api
import Api.Endpoint as Endpoint
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Modal as Modal
import Bootstrap.Pagination as Pagination
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (Decoder)
import List.Extra as List
import Role
import Route
import Session exposing (Session(..))
import Uuid exposing (Uuid)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , tasks : List Task
    , id : Uuid
    , page : Int
    , pages : Int
    , message : Maybe String
    , name : String
    }


init : Session -> Maybe Int -> Uuid -> ( Model, Cmd Msg )
init session mPage id =
    let
        page =
            Maybe.withDefault 1 mPage

        model =
            { session = session
            , tasks = []
            , id = id
            , page = page
            , pages = 0
            , message = Nothing
            , name = ""
            }

        role =
            Session.role session

        navKey =
            Session.navKey session
    in
    if Role.hasUserAccess role then
        ( model, getTasks id page )

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
    div []
        [ h4
            [ Spacing.mb3
            , style "line-height" "1.5"
            , style "border" "1px solid transparent"
            ]
            [ text model.name ]
        , Card.columns <|
            List.map taskView model.tasks
        , Pagination.defaultConfig
            |> Pagination.ariaLabel "Pagination"
            |> Pagination.itemsList
                { selectedMsg = PaginationMsg
                , prevItem = Just <| Pagination.ListItem [] [ text "<" ]
                , nextItem = Just <| Pagination.ListItem [] [ text ">" ]
                , activeIdx = index
                , data = pages
                , itemFn = \_ page -> Pagination.ListItem [] [ text <| String.fromInt page ]
                , urlFn = \_ page -> Route.routeToString (Route.Category (Just model.id) (Just page))
                }
            |> Pagination.view
        ]


taskView : Task -> Card.Config Msg
taskView task =
    Card.config []
        |> Card.block []
            [ Block.titleH5 [] [ text task.name ]
            , Block.link [ href "#" ] [ text "Перейти к задаче" ]
            ]


viewTaskList : Model -> Html Msg
viewTaskList model =
    div [] []


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
    | GotTasks (Api.Response TaskList)
    | PaginationMsg Int


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
            ( { model | message = Nothing }, Cmd.none )

        GotTasks (Ok tasks) ->
            ( { model | tasks = tasks.tasks, pages = tasks.pages, name = tasks.name }, Cmd.none )

        GotTasks (Err err) ->
            ( { model | message = Just err.message }, Cmd.none )

        PaginationMsg _ ->
            ( model, Cmd.none )



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
    , name : String
    , tasks : List Task
    }


taskListDecoder : Decoder TaskList
taskListDecoder =
    D.map3 TaskList
        (D.field "pages" D.int)
        (D.field "name" D.string)
        (D.field "tasks" (D.list taskDecoder))


getTasks : Uuid -> Int -> Cmd Msg
getTasks id page =
    Api.get (Endpoint.listTasks id (page - 1)) Nothing GotTasks taskListDecoder



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
