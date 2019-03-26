module Page exposing (Page(..), Settings, view, viewWithoutHeader)

import Api
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (Document)
import Cred exposing (Cred)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Role exposing (Role(..))
import Route exposing (Route)
import Username
import Viewer exposing (Viewer)


type Page
    = Other
    | Root
    | Login
    | Register
    | RegisterConfirm
    | AddCategory


type alias Settings msg =
    { navbar : Navbar.State
    , toNavbarMsg : Navbar.State -> msg
    , userDropdown : Dropdown.State
    , toUserDropdownMsg : Dropdown.State -> msg
    }


view :
    Maybe Viewer
    -> Page
    -> { title : String, content : Html msg }
    -> (msg -> rootMsg)
    -> Settings rootMsg
    -> Document rootMsg
view maybeViewer page { title, content } toRootMsg ps =
    { title = title
    , body = viewHeader page maybeViewer ps :: Html.map toRootMsg content :: [ viewFooter ]
    }


viewWithoutHeader :
    Maybe Viewer
    -> Page
    -> { title : String, content : Html msg }
    -> (msg -> rootMsg)
    -> Document rootMsg
viewWithoutHeader maybeViewer page { title, content } toRootMsg =
    { title = title
    , body = Html.map toRootMsg content :: [ viewFooter ]
    }


viewHeader : Page -> Maybe Viewer -> Settings msg -> Html msg
viewHeader page viewer ps =
    let
        items =
            mkMenu viewer ps
    in
    div [ class "bg-dark" ]
        [ Navbar.config ps.toNavbarMsg
            |> Navbar.withAnimation
            |> Navbar.collapseMedium
            |> Navbar.container
            |> Navbar.dark
            |> Navbar.brand
                [ href "/", style "font-family" "'Lobster', cursive" ]
                [ text " Еджудж"
                ]
            |> Navbar.items []
            |> Navbar.customItems items
            |> Navbar.view ps.navbar
        ]


mkMenu : Maybe Viewer -> Settings msg -> List (Navbar.CustomItem msg)
mkMenu mViewer ps =
    case mViewer of
        Just viewer ->
            mkViewerMenu viewer ps

        Nothing ->
            mkGuestMenu ps


mkViewerMenu : Viewer -> Settings msg -> List (Navbar.CustomItem msg)
mkViewerMenu viewer ps =
    let
        role =
            Viewer.cred >> Api.role <| viewer

        username =
            Viewer.cred >> Api.username <| viewer

        usernameStr =
            Username.toString username

        adminMenu =
            case role of
                Admin ->
                    mkAdminMenu ps

                User ->
                    []

        items =
            [ Navbar.customItem <|
                Dropdown.dropdown
                    ps.userDropdown
                    { options = []
                    , toggleMsg = ps.toUserDropdownMsg
                    , toggleButton =
                        Dropdown.toggle [ Button.dark ] [ text usernameStr ]
                    , items =
                        [ Dropdown.anchorItem [] [ text "Профиль" ]
                        , Dropdown.divider
                        , Dropdown.anchorItem [ Route.href Route.Logout ] [ text "Выход" ]
                        ]
                    }
            ]
    in
    items


mkAdminMenu : Settings msg -> List (Html msg)
mkAdminMenu ps =
    [ Dropdown.dropdown
        ps.userDropdown
        { options = []
        , toggleMsg = ps.toUserDropdownMsg
        , toggleButton =
            Dropdown.toggle [ Button.primary ] [ text "My dropdown" ]
        , items = []
        }
    ]


mkGuestMenu : Settings msg -> List (Navbar.CustomItem msg)
mkGuestMenu ps =
    [ Navbar.customItem <|
        Button.linkButton [ Button.light, Button.attrs [ Spacing.mx2, Route.href Route.Register ] ] [ text "Регистрация" ]
    , Navbar.customItem <|
        Button.linkButton [ Button.outlineLight, Button.attrs [ Spacing.mx2, Route.href Route.Login ] ] [ text "Вход" ]
    ]


viewFooter : Html msg
viewFooter =
    footer []
        []
