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
    | Register (Maybe String)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Root top
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map (Register Nothing) (s "register")
        , Parser.map Register (s "register" </> s "confirm" <?> string "token")
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

                Register _ ->
                    [ "register" ]
    in
    "/" ++ String.join "/" pieces
