module Page.AddCategory exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api
import Cred exposing (Cred)
import Api.Endpoint as Endpoint
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Bootstrap.Text as Text
import Bootstrap.Utilities.Size as Size
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session(..))
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , viewer : Viewer
    , category : String
    , message : Maybe String
    }




init : Session -> Viewer -> ( Model, Cmd Msg )
init session viewer =
        ( { session = session
          , viewer = viewer
          , category = ""
          , message = Nothing
          }
        , Cmd.none
        )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Добавление категории"
    , content =
        div [ class "vertical-center-wrapper" ]
            [ div [ class "vertical-center" ]
                [ Grid.container
                    []
                    [ Grid.row
                        [ Row.middleXs
                        , Row.centerXs
                        , Row.attrs []
                        ]
                        [ Grid.col
                            [ Col.lg4
                            , Col.textAlign Text.alignMdCenter
                            ]
                            [ h1 [] [ text "Вход" ]
                            , Button.linkButton
                                [ Button.attrs [ Route.href Route.Register ] ]
                                [ text <| "Нет аккаунта?" ]
                            -- , viewForm model.form
                            ]
                        ]
                    ]
                ]
            , showModal model.message
            ]
    }


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
    = SubmittedForm
    | EnteredCategory String
    | Added (Api.Response ())
    | GotSession Session
    | CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        cred = Viewer.cred model.viewer
    in
    case msg of
        SubmittedForm ->
            ( model
            , sendCategory cred <| String.trim model.category
            )

        EnteredCategory category ->
            ( {model | category = category} , Cmd.none)

        Added (Ok viewer) ->
            ( { model | message = Just ("Категория " ++ model.category ++" добавлена!"), category = "" }
            , Cmd.none
            )

        Added (Err error) ->
            ( { model | message = Just error.message }
            , Cmd.none
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        CloseModal ->
            ( { model | message = Nothing }, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- HTTP


sendCategory : Cred -> String -> Cmd Msg
sendCategory cred category =
    let
        body =
            Encode.object
                [ ( "category", Encode.string category )
                ]
                |> Http.jsonBody
    in
    Api.postExpectEmpty Endpoint.addCategory Nothing body Added



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
