module RouteTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Route
import Test exposing (..)


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
                    Expect.equal (Route.catMaybes input) output
            , test "Only Nothing" <|
                \_ ->
                    let
                        input =
                            [ Nothing, Nothing ]

                        output =
                            []
                    in
                    Expect.equal (Route.catMaybes input) output
            , test "Mixed" <|
                \_ ->
                    let
                        input =
                            [ Nothing, Just 5 ]

                        output =
                            [ 5 ]
                    in
                    Expect.equal (Route.catMaybes input) output
            ]
        ]
