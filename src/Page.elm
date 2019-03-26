module Page exposing (Page(..), view, viewWithoutHeader)

import Api
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (Document)
import Cred exposing (Cred)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route exposing (Route)
import Viewer exposing (Viewer)


type Page
    = Other
    | Root
    | Login
    | Register
    | RegisterConfirm


view : Maybe Viewer -> Page -> { title : String, content : Html msg } -> Document msg
view maybeViewer page { title, content } =
    { title = title
    , body = viewHeader page maybeViewer :: content :: [ viewFooter ]
    }


viewWithoutHeader : Maybe Viewer -> Page -> { title : String, content : Html msg } -> Document msg
viewWithoutHeader maybeViewer page { title, content } =
    { title = title
    , body = content :: [ viewFooter ]
    }


viewHeader : Page -> Maybe Viewer -> Html msg
viewHeader page maybeViewer =
    case maybeViewer of
        Just viewer ->
            viewViewerHeader page viewer

        Nothing ->
            viewGuestHeader page


viewViewerHeader : Page -> Viewer -> Html msg
viewViewerHeader page viewer =
    nav [ class "navbar navbar-expand-lg navbar-dark bg-dark" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", href "#" ]
                [ text "Container" ]
            , button [ attribute "aria-controls" "navbarsExample07", attribute "aria-expanded" "false", attribute "aria-label" "Toggle navigation", class "navbar-toggler", attribute "data-target" "#navbarsExample07", attribute "data-toggle" "collapse", type_ "button" ]
                [ span [ class "navbar-toggler-icon" ]
                    []
                ]
            , div [ class "collapse navbar-collapse", id "navbarsExample07" ]
                [ ul [ class "navbar-nav mr-auto" ]
                    [ li [ class "nav-item active" ]
                        [ a [ class "nav-link", href "#" ]
                            [ text "Home "
                            , span [ class "sr-only" ]
                                [ text "(current)" ]
                            ]
                        ]
                    , li [ class "nav-item" ]
                        [ a [ class "nav-link", href "#" ]
                            [ text "Link" ]
                        ]
                    , li [ class "nav-item" ]
                        [ a [ class "nav-link disabled", href "#" ]
                            [ text "Disabled" ]
                        ]
                    , li [ class "nav-item dropdown" ]
                        [ a [ attribute "aria-expanded" "false", attribute "aria-haspopup" "true", class "nav-link dropdown-toggle", attribute "data-toggle" "dropdown", href "#", id "dropdown07" ]
                            [ text "Dropdown" ]
                        , div [ attribute "aria-labelledby" "dropdown07", class "dropdown-menu" ]
                            [ a [ class "dropdown-item", href "#" ]
                                [ text "Action" ]
                            , a [ class "dropdown-item", href "#" ]
                                [ text "Another action" ]
                            , a [ class "dropdown-item", href "#" ]
                                [ text "Something else here" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewGuestHeader : Page -> Html msg
viewGuestHeader page =
    nav [ class "navbar navbar-expand-lg navbar-dark bg-dark" ]
        [ div [ class "container" ]
            [ a
                [ class "navbar-brand"
                , Route.href Route.Root
                , style "font-family" "'Lobster', cursive"
                ]
                [ text "Еджудж" ]
            , button
                [ attribute "data-target" "#ejudje-navbar"
                , attribute "aria-expanded" "false"
                , attribute "aria-label" "Toggle navigation"
                , class "navbar-toggler"
                , attribute "data-toggle" "collapse"
                , type_ "button"
                ]
                [ span [ class "navbar-toggler-icon" ] []
                ]
            , div [ class "collapse navbar-collapse", id "ejudje-navbar" ]
                [ ul [ class "navbar-nav ml-auto" ]
                    [ li [ class "nav-item" ]
                        [ Button.linkButton
                            [ Button.light
                            , Button.attrs [ Spacing.mx2, Route.href Route.Register ]
                            ]
                            [ text "Регистрация" ]
                        ]
                    , li [ class "nav-item dropdown" ]
                        [ Button.linkButton
                            [ Button.outlineLight
                            , Button.attrs [ Spacing.mx2, Route.href Route.Login ]
                            ]
                            [ text "Вход" ]
                        ]
                    ]
                ]
            ]
        ]


viewFooter : Html msg
viewFooter =
    footer []
        []
