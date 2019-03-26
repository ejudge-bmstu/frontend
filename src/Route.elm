module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, query, s, string, top)
import Url.Parser.Query as Query exposing (string)



-- ROUTING


type Route
    = Root
    | Login
    | Logout
    | Register
    | RegisterConfirm (Maybe String)
    | AddCategory


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Root top
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map Register (s "register")
        , Parser.map RegisterConfirm (s "register" </> s "confirm" <?> string "token")
        , Parser.map AddCategory (s "category" </> s "add")
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Root ->
                    []

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Register ->
                    [ "register" ]

                RegisterConfirm _ ->
                    [ "register", "confirm" ]

                AddCategory ->
                    [ "category", "add"]
    in
    "/" ++ String.join "/" pieces
