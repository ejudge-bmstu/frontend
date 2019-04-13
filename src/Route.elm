module Route exposing
    ( Route(..)
    , catMaybes
    , fromUrl
    , href
    , replaceUrl
    , routeToString
    )

import Browser.Navigation as Nav
import Dict
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s, string, top)
import Url.Parser.Query as Query exposing (string)
import Uuid exposing (Uuid)



-- ROUTING


type Route
    = Root
    | NotFound
    | Login
    | Logout
    | Register
    | RegisterConfirm (Maybe String)
    | TaskList
    | Task Uuid
    | AddTask
    | UserResults
    | TaskResults Uuid


uuid : Parser.Parser (Maybe Uuid -> a) a
uuid =
    Parser.map Uuid.fromString Parser.string


bool : String -> Query.Parser (Maybe Bool)
bool name =
    Query.enum name (Dict.fromList [ ( "true", True ), ( "false", False ) ])


parser : Parser (Route -> a) a
parser =
    let
        mkTaskRoute x =
            case x of
                Just y ->
                    Task y

                Nothing ->
                    NotFound

        mkTaskResultRoute x =
            case x of
                Just y ->
                    TaskResults y

                Nothing ->
                    NotFound
    in
    oneOf
        [ Parser.map Root top
        , Parser.map Root (s "home")
        , Parser.map NotFound (s "404")
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map Register (s "register")
        , Parser.map RegisterConfirm (s "register" </> s "confirm" <?> string "token")
        , Parser.map TaskList (s "task" </> s "list")
        , Parser.map AddTask (s "task" </> s "add")
        , Parser.map mkTaskResultRoute (s "task" </> uuid </> s "results")
        , Parser.map mkTaskRoute (s "task" </> uuid)
        , Parser.map UserResults (s "user" </> s "results")
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

                TaskList ->
                    [ "task", "list" ]

                Task id ->
                    [ "task", Uuid.toString id ]

                TaskResults id ->
                    [ "task", Uuid.toString id, "results" ]

                AddTask ->
                    [ "task", "add" ]

                UserResults ->
                    [ "user", "results" ]
    in
    "/" ++ String.join "/" pieces


catMaybes : List (Maybe a) -> List a
catMaybes list =
    case list of
        [] ->
            []

        (Just x) :: xs ->
            x :: catMaybes xs

        Nothing :: xs ->
            catMaybes xs
