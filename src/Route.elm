module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, query, s, string)
import Url.Parser.Query as Query



-- ROUTING


type Route
    = Root
    | Login
    | Logout
    | Register
    | RegisterContinue
    | RegisterConfirm (Maybe String)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Root Parser.top
        , Parser.map Login (Parser.s "login")
        , Parser.map Logout (Parser.s "logout")
        , Parser.map Register (Parser.s "register")
        , Parser.map RegisterContinue (Parser.s "register" </> Parser.s "continue")
        , Parser.map RegisterConfirm (Parser.s "register" </> Parser.s "confirm" <?> Query.string "token")
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

                RegisterContinue ->
                    [ "register", "continue" ]

                RegisterConfirm _ ->
                    [ "register", "confirm" ]
    in
    "/" ++ String.join "/" pieces
