module Main exposing (main)

-- import Debug exposing (..)

import Api
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Navbar as Navbar
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.AddTask as AddTask
import Page.Blank as Blank
import Page.Category as Category
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Register as Register
import Page.RegisterConfirm as RegisterConfirm
import Page.Root as Root
import Page.SendSolution as SendSolution
import Page.Task as Task
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
    | Category Category.Model
    | Task Task.Model
    | AddTask AddTask.Model
    | SendSolution SendSolution.Model



-- MODEL


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

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
            , toNavbarMsg = NavbarMsg
            , userDropdown = model.userDropdown
            , toUserDropdownMsg = DropdownMsg
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
            viewPage Page.Root RootMsg (Root.view root) True

        Login login ->
            viewPage Page.Login LoginMsg (Login.view login) True

        Register register ->
            viewPage Page.Register RegisterMsg (Register.view register) True

        RegisterConfirm register ->
            viewPage Page.RegisterConfirm RegisterConfirmMsg (RegisterConfirm.view register) True

        Category category ->
            viewPage Page.Category CategoryMsg (Category.view category) True

        Task task ->
            viewPage Page.Task TaskMsg (Task.view task) True

        AddTask task ->
            viewPage Page.AddTask AddTaskMsg (AddTask.view task) True

        SendSolution task ->
            viewPage Page.TaskSolution SendSolutionMsg (SendSolution.view task) True



-- UPDATE


type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | RootMsg Root.Msg
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | RegisterConfirmMsg RegisterConfirm.Msg
    | CategoryMsg Category.Msg
    | NavbarMsg Navbar.State
    | DropdownMsg Dropdown.State
    | TaskMsg Task.Msg
    | AddTaskMsg AddTask.Msg
    | SendSolutionMsg SendSolution.Msg


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

        Category category ->
            Category.toSession category

        Task task ->
            Task.toSession task

        AddTask task ->
            AddTask.toSession task

        SendSolution sendSolution ->
            SendSolution.toSession sendSolution


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound session }, Cmd.none )

        Just Route.NotFound ->
            ( { model | page = NotFound session }, Cmd.none )

        Just Route.Root ->
            Root.init session
                |> updateWith model Root RootMsg

        Just Route.Login ->
            Login.init session
                |> updateWith model Login LoginMsg

        Just Route.Register ->
            Register.init session
                |> updateWith model Register RegisterMsg

        Just (Route.RegisterConfirm token) ->
            RegisterConfirm.init session token
                |> updateWith model RegisterConfirm RegisterConfirmMsg

        Just Route.Logout ->
            ( model, Api.logout )

        Just Route.Category ->
            Category.init session
                |> updateWith model Category CategoryMsg

        Just (Route.Task id) ->
            Task.init id session
                |> updateWith model Task TaskMsg

        Just Route.AddTask ->
            AddTask.init session
                |> updateWith model AddTask AddTaskMsg

        Just (Route.TaskSolution id) ->
            SendSolution.init session id
                |> updateWith model SendSolution SendSolutionMsg


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

        ( RootMsg subMsg, Root home ) ->
            Root.update subMsg home
                |> updateWith model Root RootMsg

        ( LoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith model Login LoginMsg

        ( RegisterMsg subMsg, Register register ) ->
            Register.update subMsg register
                |> updateWith model Register RegisterMsg

        ( RegisterConfirmMsg subMsg, RegisterConfirm register ) ->
            RegisterConfirm.update subMsg register
                |> updateWith model RegisterConfirm RegisterConfirmMsg

        ( CategoryMsg subMsg, Category category ) ->
            Category.update subMsg category
                |> updateWith model Category CategoryMsg

        ( TaskMsg subMsg, Task task ) ->
            Task.update subMsg task
                |> updateWith model Task TaskMsg

        ( AddTaskMsg subMsg, AddTask task ) ->
            AddTask.update subMsg task
                |> updateWith model AddTask AddTaskMsg

        ( SendSolutionMsg subMsg, SendSolution task ) ->
            SendSolution.update subMsg task
                |> updateWith model SendSolution SendSolutionMsg

        ( NavbarMsg state, _ ) ->
            ( { model | navbar = state }, Cmd.none )

        ( DropdownMsg state, _ ) ->
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
                    Sub.map RootMsg (Root.subscriptions root)

                Login login ->
                    Sub.map LoginMsg (Login.subscriptions login)

                Register register ->
                    Sub.map RegisterMsg (Register.subscriptions register)

                RegisterConfirm register ->
                    Sub.map RegisterConfirmMsg (RegisterConfirm.subscriptions register)

                Category category ->
                    Sub.map CategoryMsg (Category.subscriptions category)

                Task task ->
                    Sub.map TaskMsg (Task.subscriptions task)

                AddTask task ->
                    Sub.map AddTaskMsg (AddTask.subscriptions task)

                SendSolution task ->
                    Sub.map SendSolutionMsg (SendSolution.subscriptions task)
    in
    Sub.batch
        [ Dropdown.subscriptions model.userDropdown DropdownMsg
        , Navbar.subscriptions model.navbar NavbarMsg
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
