module Page.RegisterTokenSend exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

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
    , content =
        div [ class "vertical-center-wrapper" ]
            [ div [ class "vertical-center" ]
                [ text "На указанный вами email выслано подтверждение."
                ]
            ]
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
