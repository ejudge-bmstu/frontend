module Main exposing (main)

import Api
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Navbar as Navbar
import Browser exposing (Document)
import Browser.Navigation as Nav
import Cred exposing (Cred)
import Debug exposing (..)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.AddCategory as AddCategory
import Page.Blank as Blank
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Register as Register
import Page.RegisterConfirm as RegisterConfirm
import Page.Root as Root
import Route exposing (Route)
import Session exposing (Session(..))
import Url exposing (Url)
import Viewer exposing (Viewer)


type alias Model =
    { page : Page
    , navbar : Navbar.State
    , userDropdown : Dropdown.State
    }


type Page
    = Init Session
    | NotFound Session
    | Root Root.Model
    | Login Login.Model
    | Register Register.Model
    | RegisterConfirm RegisterConfirm.Model
    | AddCategory AddCategory.Model



-- MODEL


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState GotNavbarMsg

        initModel =
            { page = Init (Session.fromViewer navKey maybeViewer)
            , navbar = navbarState
            , userDropdown = Dropdown.initialState
            }

        ( model, cmd ) =
            changeRouteTo (Route.fromUrl url) initModel
    in
    ( model, Cmd.batch [ cmd, navbarCmd ] )



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewer =
            Session.viewer (toSession model)

        pageSettings =
            { navbar = model.navbar
            , toNavbarMsg = GotNavbarMsg
            , userDropdown = model.userDropdown
            , toUserDropdownMsg = GotDropdownMsg
            }

        viewPage page toMsg config withNavbar =
            case withNavbar of
                True ->
                    Page.view viewer page config toMsg pageSettings

                False ->
                    Page.viewWithoutHeader viewer page config toMsg
    in
    case model.page of
        Init _ ->
            viewPage Page.Other (\_ -> Ignored) Blank.view False

        NotFound _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view False

        Root root ->
            viewPage Page.Root GotRootMsg (Root.view root) True

        Login login ->
            viewPage Page.Login GotLoginMsg (Login.view login) True

        Register register ->
            viewPage Page.Register GotRegisterMsg (Register.view register) True

        RegisterConfirm register ->
            viewPage Page.RegisterConfirm GotRegisterConfirmMsg (RegisterConfirm.view register) True

        AddCategory category ->
            viewPage Page.AddCategory GotAddCategoryMsg (AddCategory.view category) True



-- UPDATE


type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotRootMsg Root.Msg
    | GotLoginMsg Login.Msg
    | GotRegisterMsg Register.Msg
    | GotRegisterConfirmMsg RegisterConfirm.Msg
    | GotAddCategoryMsg AddCategory.Msg
    | GotNavbarMsg Navbar.State
    | GotDropdownMsg Dropdown.State



toSession : Model -> Session
toSession model =
    case model.page of
        Init session ->
            session

        NotFound session ->
            session

        Root root ->
            Root.toSession root

        Login login ->
            Login.toSession login

        Register register ->
            Register.toSession register

        RegisterConfirm register ->
            RegisterConfirm.toSession register

        AddCategory category ->
            AddCategory.toSession category


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound session }, Cmd.none )

        Just Route.Root ->
            Root.init session
                |> updateWith model Root GotRootMsg

        Just Route.Login ->
            Login.init session
                |> updateWith model Login GotLoginMsg

        Just Route.Register ->
            Register.init session
                |> updateWith model Register GotRegisterMsg

        Just (Route.RegisterConfirm token) ->
            RegisterConfirm.init session token
                |> updateWith model RegisterConfirm GotRegisterConfirmMsg

        Just Route.Logout ->
            ( model, Api.logout )

        Just Route.AddCategory ->
            case session of
                LoggedIn _ viewer ->
                    AddCategory.init session viewer
                        |> updateWith model AddCategory GotAddCategoryMsg
                Guest _ -> changeRouteTo Nothing model



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
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
                |> updateWith model Root GotRootMsg

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith model Login GotLoginMsg

        ( GotRegisterMsg subMsg, Register register ) ->
            Register.update subMsg register
                |> updateWith model Register GotRegisterMsg

        ( GotRegisterConfirmMsg subMsg, RegisterConfirm register ) ->
            RegisterConfirm.update subMsg register
                |> updateWith model RegisterConfirm GotRegisterConfirmMsg

        ( GotAddCategoryMsg subMsg, AddCategory category ) ->
            AddCategory.update subMsg category
                |> updateWith model AddCategory GotAddCategoryMsg

        ( GotNavbarMsg state, _ ) ->
            ( { model | navbar = state }, Cmd.none )

        ( GotDropdownMsg state, _ ) ->
            ( { model | userDropdown = state }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : Model -> (subModel -> Page) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith model toModel toMsg ( subModel, subCmd ) =
    ( { model
        | page = toModel subModel
      }
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pageSub =
            case model.page of
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

                RegisterConfirm register ->
                    Sub.map GotRegisterConfirmMsg (RegisterConfirm.subscriptions register)

                AddCategory category ->
                    Sub.map GotAddCategoryMsg (AddCategory.subscriptions category)
    in
    Sub.batch
        [ Dropdown.subscriptions model.userDropdown GotDropdownMsg
        , Navbar.subscriptions model.navbar GotNavbarMsg
        , pageSub
        ]



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
