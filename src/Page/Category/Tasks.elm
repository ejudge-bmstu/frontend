module Page.Category.Tasks exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

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
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Role
import Route exposing (Route)
import Session exposing (Session(..))
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , tasks : List Task
    , id : String
    , message : Maybe String
    }


init : Session -> String -> ( Model, Cmd Msg )
init session id =
    let
        model =
            { session = session
            , tasks = []
            , id = id
            , message = Nothing
            }

        role =
            Session.role session

        navKey =
            Session.navKey session
    in
    if Role.hasUserAccess role then
        ( model, Cmd.none )

    else
        ( model, Route.replaceUrl navKey Route.NotFound )



-- VIEW


view : Model -> Html Msg
view model =
    div [ Spacing.m1 ]
        [ Grid.container
            [ class "content" ]
            [ Grid.row
                []
                [ Grid.col
                    [ Col.lg3 ]
                    [ viewTaskList model ]
                ]
            ]
        ]


viewTaskList : Model -> Html Msg
viewTaskList model =
    div [] [ text "типо задачи" ]


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
    | GotTasks (Api.Response (List Task))


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
            ( { model | tasks = tasks }, Cmd.none )

        GotTasks (Err err) ->
            ( { model | message = Just err.message }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Task =
    { id : String
    , name : String
    }


taskDecoder : Decoder Task
taskDecoder =
    D.map2 Task
        (D.field "id" D.string)
        (D.field "name" D.string)


getCategories : String -> Cmd Msg
getCategories id =
    Api.get (Endpoint.listTasks id) Nothing GotTasks (D.list taskDecoder)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
