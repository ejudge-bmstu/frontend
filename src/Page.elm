module Page exposing (Page(..), view)

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
import Html exposing (Html, a, button, div, footer, i, img, li, nav, p, span, text, ul)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)


type Page
    = Other
    | Root
    | Login


view : Maybe Viewer -> Page -> { title : String, content : Html msg } -> Document msg
view maybeViewer page { title, content } =
    { title = title
    , body = CDN.stylesheet :: viewHeader page maybeViewer :: content :: [ viewFooter ]
    }


viewHeader : Page -> Maybe Viewer -> Html msg
viewHeader page maybeViewer =
    div [] [ a [ Route.href Route.Login ] [ text "login" ] ]


viewFooter : Html msg
viewFooter =
    footer []
        [ Grid.container
            []
            [ text "footer" ]
        ]
