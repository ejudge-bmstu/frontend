module Page.Root exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (Html)
import Route
import Session exposing (Session)


type alias Model =
    Session


type Msg
    = GotSession Session


init : Session -> ( Model, Cmd Msg )
init session =
    ( session, Cmd.none )


view : Model -> { title : String, content : Html msg }
view _ =
    { title = ""
    , content = Html.text "root"
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( session
            , Route.replaceUrl (Session.navKey session) Route.Root
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model)


toSession : Model -> Session
toSession model =
    model
