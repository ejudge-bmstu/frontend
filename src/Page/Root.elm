module Page.Root exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Bootstrap.Navbar as Navbar
import Html exposing (Html)
import Route
import Session exposing (Session)


type alias Model =
    { session : Session
    , navbarState : Navbar.State
    }


type Msg
    = GotSession Session
    | NavbarMsg Navbar.State


init : Session -> ( Model, Cmd Msg )
init session =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( Model session navbarState, navbarCmd )


view : Model -> { title : String, content : Html msg }
view _ =
    { title = "Главная"
    , content = Html.text ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Session.changes GotSession (Session.navKey model.session)
        , Navbar.subscriptions model.navbarState NavbarMsg
        ]


toSession : Model -> Session
toSession model =
    model.session
