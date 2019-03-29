module Page.Task exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api
import Api.Endpoint as Endpoint
import Bootstrap.Button as Button
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
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Route
import Session exposing (Session)
import Uuid exposing (Uuid)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , id : Uuid
    , task : Maybe Task
    , errorMessage : Maybe String
    }


type alias Task =
    { name : String
    , description : String
    , category : Category
    , limits : List Limit
    }


taskDecoder : Decoder Task
taskDecoder =
    D.map4 Task
        (D.field "name" D.string)
        (D.field "description" D.string)
        (D.field "category" categoryDecoder)
        (D.field "limits" (D.list limitDecoder))


type alias Category =
    { id : Uuid
    , name : String
    }


categoryDecoder : Decoder Category
categoryDecoder =
    D.map2 Category
        (D.field "id" Uuid.decoder)
        (D.field "name" D.string)


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
    ( { session = session
      , id = id
      , task = Nothing
      , errorMessage = Nothing
      }
    , getTask id (Session.cred session)
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Задача"
    , content =
        case model.task of
            Just task ->
                viewTask task

            Nothing ->
                div [] []
    }


viewTask : Task -> Html Msg
viewTask task =
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ h3 [] [ text task.name ]
                , p [] [ text task.description ]
                , p []
                    [ text "Категория: "
                    , a [ Route.href (Route.Category (Just task.category.id) Nothing) ]
                        [ text task.category.name ]
                    ]
                , h4 [] [ text "Ограничения" ]
                , Table.table
                    { options = [ Table.striped, Table.hover ]
                    , thead =
                        Table.simpleThead
                            [ Table.th [] [ text "" ]
                            , Table.th [] [ text "Ограничение по времени (с)" ]
                            , Table.th [] [ text "Ограничние по памяти (Мб)" ]
                            ]
                    , tbody =
                        Table.tbody [] <|
                            List.map mkTr task.limits
                    }
                ]
            ]
        ]


mkTr : Limit -> Table.Row Msg
mkTr limit =
    Table.tr []
        [ Table.td [] [ text limit.language ]
        , Table.td [] [ text <| String.fromInt limit.time ]
        , Table.td [] [ text <| String.fromInt limit.memory ]
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
    = GotTask (Api.Response Task)
    | GotSession Session
    | CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTask (Ok task) ->
            ( { model | task = Just task }
            , Cmd.none
            )

        GotTask (Err error) ->
            ( { model | errorMessage = Just error.message }
            , Cmd.none
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        CloseModal ->
            ( { model | errorMessage = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- HTTP


getTask : Uuid -> Maybe Cred -> Cmd Msg
getTask id cred =
    Api.get (Endpoint.getTask id) cred GotTask taskDecoder



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
