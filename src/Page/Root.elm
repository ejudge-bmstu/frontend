module Page.Root exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (Html)
import Session exposing (Session)


type alias Model =
    Session


type Msg
    = None


init : Session -> ( Model, Cmd Msg )
init session =
    ( session, Cmd.none )


view : Model -> { title : String, content : Html msg }
view _ =
    { title = ""
    , content = Html.text "root"
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toSession : Model -> Session
toSession model =
    model
