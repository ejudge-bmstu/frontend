module Main exposing (main)

import Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import Cred exposing (Cred)
import Debug exposing (..)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.Blank as Blank
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Register as Register
import Page.RegisterConfirm as RegisterConfirm
import Page.RegisterContinue as RegisterContinue
import Page.Root as Root
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)
import Viewer exposing (Viewer)


type Model
    = Init Session
    | NotFound Session
    | Root Root.Model
    | Login Login.Model
    | Register Register.Model
    | RegisterContinue RegisterContinue.Model
    | RegisterConfirm RegisterConfirm.Model



-- MODEL


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    changeRouteTo (Route.fromUrl url)
        (Init (Session.fromViewer navKey maybeViewer))



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewPage page toMsg config maybeNavbar =
            case maybeNavbar of
                Just ( navbarState, toNavbarMsg ) ->
                    let
                        { title, body } =
                            Page.view (Session.viewer (toSession model)) page config navbarState toNavbarMsg
                    in
                    { title = title
                    , body = List.map (Html.map toMsg) body
                    }

                Nothing ->
                    let
                        { title, body } =
                            Page.viewWithoutHeader (Session.viewer (toSession model)) page config
                    in
                    { title = title
                    , body = List.map (Html.map toMsg) body
                    }
    in
    case model of
        Init _ ->
            viewPage Page.Other (\_ -> Ignored) Blank.view Nothing

        NotFound _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view Nothing

        Root root ->
            viewPage Page.Root GotRootMsg (Root.view root) <|
                Just ( root.navbarState, Root.NavbarMsg )

        Login login ->
            viewPage Page.Login GotLoginMsg (Login.view login) <|
                Just ( login.navbarState, Login.NavbarMsg )

        Register register ->
            viewPage Page.Register GotRegisterMsg (Register.view register) <|
                Just ( register.navbarState, Register.NavbarMsg )

        RegisterContinue register ->
            viewPage Page.RegisterContinue GotRegisterContinueMsg (RegisterContinue.view register) <|
                Just ( register.navbarState, RegisterContinue.NavbarMsg )

        RegisterConfirm register ->
            viewPage Page.RegisterConfirm GotRegisterConfirmMsg (RegisterConfirm.view register) <|
                Just ( register.navbarState, RegisterConfirm.NavbarMsg )



-- UPDATE


type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotRootMsg Root.Msg
    | GotLoginMsg Login.Msg
    | GotRegisterMsg Register.Msg
    | GotRegisterContinueMsg RegisterContinue.Msg
    | GotRegisterConfirmMsg RegisterConfirm.Msg


toSession : Model -> Session
toSession page =
    case page of
        Init session ->
            session

        NotFound session ->
            session

        Root root ->
            Root.toSession root

        Login model ->
            Login.toSession model

        Register model ->
            Register.toSession model

        RegisterContinue model ->
            RegisterContinue.toSession model

        RegisterConfirm model ->
            RegisterConfirm.toSession model


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            Root.init session
                |> updateWith Root GotRootMsg

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg

        Just Route.Register ->
            Register.init session
                |> updateWith Register GotRegisterMsg

        Just Route.RegisterContinue ->
            RegisterContinue.init session
                |> updateWith RegisterContinue GotRegisterContinueMsg

        Just (Route.RegisterConfirm token) ->
            RegisterConfirm.init session token
                |> updateWith RegisterConfirm GotRegisterConfirmMsg

        Just Route.Logout ->
            ( model, Api.logout )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotRootMsg subMsg, Root home ) ->
            Root.update subMsg home
                |> updateWith Root GotRootMsg

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg

        ( GotRegisterMsg subMsg, Register register ) ->
            Register.update subMsg register
                |> updateWith Register GotRegisterMsg

        ( GotRegisterContinueMsg subMsg, RegisterContinue register ) ->
            RegisterContinue.update subMsg register
                |> updateWith RegisterContinue GotRegisterContinueMsg

        ( GotRegisterConfirmMsg subMsg, RegisterConfirm register ) ->
            RegisterConfirm.update subMsg register
                |> updateWith RegisterConfirm GotRegisterConfirmMsg

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Init _ ->
            Sub.none

        Root root ->
            Sub.map GotRootMsg (Root.subscriptions root)

        Login login ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        Register register ->
            Sub.map GotRegisterMsg (Register.subscriptions register)

        RegisterContinue register ->
            Sub.map GotRegisterContinueMsg (RegisterContinue.subscriptions register)

        RegisterConfirm register ->
            Sub.map GotRegisterConfirmMsg (RegisterConfirm.subscriptions register)



-- MAIN


main : Program Value Model Msg
main =
    Api.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
