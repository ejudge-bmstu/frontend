module RouteTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Route exposing (..)
import Test exposing (..)
import Url exposing (Url)
import Uuid


suite : Test
suite =
    describe "Route Module"
        [ describe "catMaybes"
            [ test "Only Just" <|
                \_ ->
                    let
                        input =
                            [ Just 4, Just 5 ]

                        output =
                            [ 4, 5 ]
                    in
                    Expect.equal (catMaybes input) output
            , test "Only Nothing" <|
                \_ ->
                    let
                        input =
                            [ Nothing, Nothing ]

                        output =
                            []
                    in
                    Expect.equal (catMaybes input) output
            , test "Mixed" <|
                \_ ->
                    let
                        input =
                            [ Nothing, Just 5 ]

                        output =
                            [ 5 ]
                    in
                    Expect.equal (catMaybes input) output
            ]
        , describe "Route.fromUrl"
            [ testUrl "/" Nothing Root
            , testUrl "/login" Nothing Login
            , testUrl "/logout" Nothing Logout
            , testUrl "/register" Nothing Register
            , testUrl "/register/confirm" (Just "token=1234") (RegisterConfirm (Just "1234"))
            , testUrl "/task/list" Nothing TaskList
            , testUrl "/task/add" Nothing AddTask
            , testUrl "/task/123" Nothing NotFound
            , case Uuid.fromString "b15ca88d-f544-4169-87aa-042c7e8d3700" of
                Just uuid ->
                    testUrl "/task/b15ca88d-f544-4169-87aa-042c7e8d3700" Nothing (Task uuid)

                Nothing ->
                    testUrl "/task/123" Nothing NotFound
            , testUrl "/user/results" Nothing UserResults
            ]
        ]


testUrl : String -> Maybe String -> Route -> Test
testUrl path query route =
    test ("Parsing route: \"" ++ path ++ "\"") <|
        \() ->
            fragment path query
                |> Route.fromUrl
                |> Expect.equal (Just route)


fragment : String -> Maybe String -> Url
fragment path query =
    { protocol = Url.Http
    , host = "foo.com"
    , port_ = Nothing
    , path = path
    , query = query
    , fragment = Nothing
    }
