module Page exposing (Page(..), view, viewWithoutHeader)

import Api
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (Document)
import Cred exposing (Cred)
import Html exposing (Html, a, button, div, footer, form, i, img, input, li, nav, p, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src, style, type_)
import Html.Events exposing (onClick, onSubmit)
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)


type Page
    = Other
    | Root
    | Login
    | Register
    | RegisterTokenSend


view : Maybe Viewer -> Page -> { title : String, content : Html msg } -> Navbar.State -> (Navbar.State -> msg) -> Document msg
view maybeViewer page { title, content } navbarState msg =
    { title = title
    , body = viewHeader page maybeViewer navbarState msg :: content :: [ viewFooter ]
    }


viewWithoutHeader : Maybe Viewer -> Page -> { title : String, content : Html msg } -> Document msg
viewWithoutHeader maybeViewer page { title, content } =
    { title = title
    , body = content :: [ viewFooter ]
    }


viewHeader : Page -> Maybe Viewer -> Navbar.State -> (Navbar.State -> msg) -> Html msg
viewHeader page maybeViewer navbarState msg =
    case maybeViewer of
        Just viewer ->
            viewViewerHeader page viewer navbarState msg

        Nothing ->
            viewGuestHeader page navbarState msg


viewViewerHeader : Page -> Viewer -> Navbar.State -> (Navbar.State -> msg) -> Html msg
viewViewerHeader page viewer navbarState toNavbarMsg =
    Navbar.config toNavbarMsg
        |> Navbar.withAnimation
        |> Navbar.collapseMedium
        -- Collapse menu at the medium breakpoint
        |> Navbar.dark
        -- Customize coloring
        |> Navbar.brand
            -- Add logo to your brand with a little styling to align nicely
            [ href "/" ]
            [ text " Еджудж"
            ]
        |> Navbar.customItems
            [ Navbar.customItem <|
                Button.linkButton [ Button.light, Button.attrs [ Spacing.mx2, Route.href Route.Logout ] ] [ text "Выход" ]
            ]
        |> Navbar.view navbarState


viewGuestHeader : Page -> Navbar.State -> (Navbar.State -> msg) -> Html msg
viewGuestHeader page navbarState toNavbarMsg =
    -- Wrap in a container to center the navbar
    Navbar.config toNavbarMsg
        |> Navbar.withAnimation
        |> Navbar.collapseMedium
        -- Collapse menu at the medium breakpoint
        |> Navbar.dark
        -- Customize coloring
        |> Navbar.brand
            -- Add logo to your brand with a little styling to align nicely
            [ href "/", style "font-family" "'Lobster', cursive" ]
            [ text " Еджудж"
            ]
        |> Navbar.customItems
            [ Navbar.customItem <|
                Button.linkButton [ Button.light, Button.attrs [ Spacing.mx2, Route.href Route.Register ] ] [ text "Регистрация" ]
            , Navbar.customItem <|
                Button.linkButton [ Button.outlineLight, Button.attrs [ Spacing.mx2, Route.href Route.Login ] ] [ text "Вход" ]
            ]
        |> Navbar.view navbarState


viewFooter : Html msg
viewFooter =
    footer []
        []
