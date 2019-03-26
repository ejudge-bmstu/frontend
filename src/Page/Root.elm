module Page.Root exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Bootstrap.Navbar as Navbar
import Debug
import Html exposing (Html)
import Route
import Session exposing (Session)


type alias Model =
    { session : Session
    }


type Msg
    = GotSession Session


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session, Cmd.none )


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)


toSession : Model -> Session
toSession model =
    model.session
