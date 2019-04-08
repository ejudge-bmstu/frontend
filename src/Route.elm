module Route exposing
    ( Route(..)
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
import Url.Parser as Parser exposing ((</>), (<?>), Parser, int, oneOf, s, string, top)
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
    | Category
    | Task Uuid
    | AddTask
    | TaskSolution Uuid
    | UserResults


quuid : String -> Query.Parser (Maybe Uuid)
quuid name =
    Query.map (Uuid.fromString << Maybe.withDefault "") (string name)


uuuid : Parser.Parser (Maybe Uuid -> a) a
uuuid =
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

        mkTaskSolutionRoute x =
            case x of
                Just y ->
                    TaskSolution y

                Nothing ->
                    NotFound
    in
    oneOf
        [ Parser.map Root top
        , Parser.map NotFound (s "404")
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map Register (s "register")
        , Parser.map RegisterConfirm (s "register" </> s "confirm" <?> string "token")
        , Parser.map Category (s "tasks")
        , Parser.map AddTask (s "task" </> s "add")
        , Parser.map mkTaskSolutionRoute (s "task" </> s "solution" <?> quuid "id")
        , Parser.map mkTaskRoute (s "task" </> uuuid)
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
    case page of
        Root ->
            Builder.relative [ "/" ] []

        NotFound ->
            Builder.relative [ "404" ] []

        Login ->
            Builder.relative [ "login" ] []

        Logout ->
            Builder.relative [ "logout" ] []

        Register ->
            Builder.relative [ "register" ] []

        RegisterConfirm _ ->
            Builder.relative [ "register", "confirm" ] []

        Category ->
            Builder.relative [ "tasks" ] []

        Task id ->
            Builder.relative [ "task", Uuid.toString id ] []

        AddTask ->
            Builder.relative [ "task", "add" ] []

        TaskSolution x ->
            Builder.relative [ "task", "solution", Uuid.toString x ] []

        UserResults ->
            Builder.relative [ "user", "results" ] []


catMaybes : List (Maybe a) -> List a
catMaybes list =
    case list of
        [] ->
            []

        (Just x) :: xs ->
            x :: catMaybes xs

        Nothing :: xs ->
            catMaybes xs
