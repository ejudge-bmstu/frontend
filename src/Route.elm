module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, query, s, string, top)
import Url.Parser.Query as Query exposing (int, string)
import Uuid exposing (Uuid)



-- ROUTING


type Route
    = Root
    | NotFound
    | Login
    | Logout
    | Register
    | RegisterConfirm (Maybe String)
    | Category (Maybe Uuid) (Maybe Int)


uuid : String -> Query.Parser (Maybe Uuid)
uuid name =
    Query.map (Uuid.fromString << Maybe.withDefault "") (string name)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Root top
        , Parser.map NotFound (s "404")
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map Register (s "register")
        , Parser.map RegisterConfirm (s "register" </> s "confirm" <?> string "token")
        , Parser.map Category (s "category" <?> uuid "id" <?> int "page")
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

                NotFound ->
                    [ "404" ]

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Register ->
                    [ "register" ]

                RegisterConfirm _ ->
                    [ "register", "confirm" ]

                Category _ _ ->
                    [ "category" ]
    in
    "/" ++ String.join "/" pieces
