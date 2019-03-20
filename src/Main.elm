module Main exposing (Model(..), Msg(..), changeRouteTo, init, main, subscriptions, toSession, update, updateWith, view)

import Api exposing (Cred)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Page exposing (Page)
import Page.Blank as Blank
import Page.NotFound as NotFound
import Page.Root as Root
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)
import Viewer exposing (Viewer)


type Model
    = Init Session
    | NotFound Session
    | Root Root.Model



-- MODEL


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    changeRouteTo (Route.fromUrl url)
        (Init (Session.fromViewer navKey maybeViewer))



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view (Session.viewer (toSession model)) page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Init _ ->
            viewPage Page.Other (\_ -> Ignored) Blank.view

        NotFound _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view

        Root root ->
            viewPage Page.Root GotRootMsg (Root.view root)



-- UPDATE


type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotRootMsg Root.Msg



-- | GotSession Session


toSession : Model -> Session
toSession page =
    case page of
        Init session ->
            session

        NotFound session ->
            session

        Root root ->
            Root.toSession root


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



-- MAIN


main : Program Value Model Msg
main =
    Api.application Viewer.decoder
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
