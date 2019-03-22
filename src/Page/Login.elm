module Page.Login exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api
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
import Session exposing (Session)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    , navbarState : Navbar.State
    , errorMessage : Maybe String
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type alias Form =
    { username : String
    , password : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { session = session
      , problems = []
      , form =
            { username = ""
            , password = ""
            }
      , navbarState = navbarState
      , errorMessage = Nothing
      }
    , navbarCmd
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Вход"
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
                            , viewForm model.form
                            ]
                        ]
                    ]
                ]
            , showModal model.errorMessage
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
                [ text "Close" ]
            ]
        |> Modal.view modalVisibility


viewForm : Form -> Html Msg
viewForm form =
    Form.form
        [ class "form-signin"
        , onSubmit SubmittedForm
        ]
        [ Form.group []
            [ Input.text
                [ Input.id "myusername_auth"
                , Input.placeholder "Логин"
                , Input.onInput EnteredUsername
                , Input.value form.username
                ]
            , Input.password
                [ Input.id "mypwd_auth"
                , Input.onInput EnteredPassword
                , Input.placeholder "Пароль"
                , Input.value form.password
                ]
            ]
        , Button.button
            [ Button.primary, Button.attrs [ Size.w100 ] ]
            [ text "Войти" ]
        ]



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredUsername String
    | EnteredPassword String
    | CompletedLogin (Api.Response Viewer)
    | GotSession Session
    | NavbarMsg Navbar.State
    | CloseModal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , login validForm
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredUsername username ->
            updateForm (\form -> { form | username = username }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedLogin (Ok viewer) ->
            ( model
            , Viewer.store viewer
            )

        CompletedLogin (Err error) ->
            ( { model | errorMessage = Just error.message }
            , Cmd.none
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        CloseModal ->
            ( { model | errorMessage = Nothing }, Cmd.none )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Session.changes GotSession (Session.navKey model.session)
        , Navbar.subscriptions model.navbarState NavbarMsg
        ]



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


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Email ->
                if String.isEmpty form.username then
                    [ "email can't be blank." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]

                else
                    []


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { username = String.trim form.username
        , password = String.trim form.password
        }



-- HTTP


login : TrimmedForm -> Cmd Msg
login (Trimmed form) =
    let
        body =
            Encode.object
                [ ( "email", Encode.string form.username )
                , ( "password", Encode.string form.password )
                ]
                |> Http.jsonBody
    in
    Api.login body CompletedLogin



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
