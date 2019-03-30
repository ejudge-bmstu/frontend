module Page.Register exposing
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
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Text as Text
import Bootstrap.Utilities.Size as Size
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Route
import Session exposing (Session)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , form : Form
    , errorMessage : String
    , errorModalVisibility : Modal.Visibility
    , okModalVisibility : Modal.Visibility
    }


type alias Form =
    { username : String
    , email : String
    , password : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , form =
            { username = ""
            , email = ""
            , password = ""
            }
      , errorMessage = ""
      , errorModalVisibility = Modal.hidden
      , okModalVisibility = Modal.hidden
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Регистрация"
    , content = viewProcess model
    }


viewProcess : Model -> Html Msg
viewProcess model =
    div [ class "vertical-center-wrapper" ]
        [ div [ class "vertical-center" ]
            [ Grid.container
                [ class "signin-page" ]
                [ Grid.row
                    [ Row.middleXs
                    , Row.centerXs
                    , Row.attrs []
                    ]
                    [ Grid.col
                        [ Col.lg4
                        , Col.textAlign Text.alignMdCenter
                        ]
                        [ h1 [] [ text "Регистрация" ]
                        , Button.linkButton
                            [ Button.attrs [ Route.href Route.Login ] ]
                            [ text <| "Есть аккаунт?" ]
                        , viewForm model.form
                        ]
                    ]
                ]
            ]
        , showErrorModal model
        , showOkModal model
        ]


showErrorModal : Model -> Html Msg
showErrorModal model =
    mkModal
        model.errorModalVisibility
        "Ошибка"
        model.errorMessage
        CloseErrorModal


showOkModal : Model -> Html Msg
showOkModal model =
    mkModal
        model.okModalVisibility
        "Подтверждение регистрации"
        "На указанный вами email выслано подтверждение."
        CloseOkModal


mkModal : Modal.Visibility -> String -> String -> Msg -> Html Msg
mkModal visibility header message msg =
    Modal.config msg
        |> Modal.small
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text header ]
        |> Modal.body [] [ p [] [ text message ] ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick msg ]
                ]
                [ text "Закрыть" ]
            ]
        |> Modal.view visibility


viewForm : Form -> Html Msg
viewForm form =
    Form.form
        [ class "form-signup"
        , onSubmit SubmittedForm
        ]
        [ Form.group []
            [ Input.text
                [ Input.id "myusername_auth"
                , Input.placeholder "Логин"
                , Input.onInput EnteredUsername
                , Input.attrs [ minlength 6, required True ]
                , Input.value form.username
                ]
            , Input.email
                [ Input.id "mypwd_email"
                , Input.onInput EnteredEmail
                , Input.placeholder "Email"
                , Input.attrs [ required True ]
                , Input.value form.email
                ]
            , Input.password
                [ Input.id "mypwd_auth"
                , Input.onInput EnteredPassword
                , Input.placeholder "Пароль"
                , Input.attrs [ minlength 6, required True ]
                , Input.value form.password
                ]
            ]
        , Button.button
            [ Button.primary, Button.attrs [ Size.w100 ] ]
            [ text "Зарегистрироваться" ]
        ]



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredUsername String
    | EnteredEmail String
    | EnteredPassword String
    | CompletedRegister (Api.Response ())
    | GotSession Session
    | CloseErrorModal
    | CloseOkModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            ( model
            , register (trimFields model.form)
            )

        EnteredUsername username ->
            updateForm (\form -> { form | username = username }) model

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedRegister (Ok _) ->
            ( { model | okModalVisibility = Modal.shown }
            , Cmd.none
            )

        CompletedRegister (Err error) ->
            ( { model | errorMessage = error.message, errorModalVisibility = Modal.shown }
            , Cmd.none
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        CloseErrorModal ->
            ( { model | errorModalVisibility = Modal.hidden }, Cmd.none )

        CloseOkModal ->
            ( { model | okModalVisibility = Modal.hidden }
            , Route.replaceUrl (Session.navKey model.session) Route.Root
            )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- FORM


type TrimmedForm
    = Trimmed Form


type ValidatedField
    = Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Email
    , Password
    ]


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { username = String.trim form.username
        , email = String.trim form.email
        , password = String.trim form.password
        }



-- HTTP


register : TrimmedForm -> Cmd Msg
register (Trimmed form) =
    let
        body =
            Encode.object
                [ ( "username", Encode.string form.username )
                , ( "email", Encode.string form.email )
                , ( "password", Encode.string form.password )
                ]
                |> Http.jsonBody
    in
    Api.postExpectEmpty Endpoint.register Nothing body CompletedRegister



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
