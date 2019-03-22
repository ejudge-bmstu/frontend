module Page.Register exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Text as Text
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser.Navigation as Nav
import Cred exposing (Cred)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    , navbarState : Navbar.State
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type alias Form =
    { username : String
    , email : String
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
            , email = ""
            , password = ""
            }
      , navbarState = navbarState
      }
    , navbarCmd
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Регистрация"
    , content =
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
            ]
    }


viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                InvalidEntry _ str ->
                    str

                ServerError str ->
                    str
    in
    li [] [ text errorMessage ]


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
                , Input.value form.username
                ]
            , Input.email
                [ Input.id "mypwd_email"
                , Input.onInput EnteredEmail
                , Input.placeholder "Email"
                , Input.value form.email
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
    | EnteredEmail String
    | EnteredPassword String
    | CompletedLogin (WebData Viewer)
    | GotSession Session
    | NavbarMsg Navbar.State


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

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedLogin (Success viewer) ->
            ( model
            , Viewer.store viewer
            )

        CompletedLogin _ ->
            ( model
            , Cmd.none
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )


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
                if String.isEmpty form.email then
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
        , email = String.trim form.email
        , password = String.trim form.password
        }



-- HTTP


login : TrimmedForm -> Cmd Msg
login (Trimmed form) =
    let
        user =
            Encode.object
                [ ( "email", Encode.string form.email )
                , ( "password", Encode.string form.password )
                ]

        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    Api.login body CompletedLogin



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
