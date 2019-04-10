module Data.CategoryTest exposing (suite)

import Data.Category exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as D
import Route exposing (..)
import Test exposing (..)
import Url exposing (Url)
import Uuid exposing (Uuid)


uuidString : String
uuidString =
    "b15ca88d-f544-4169-87aa-042c7e8d3700"


suite : Test
suite =
    case Uuid.fromString uuidString of
        Just uuid ->
            suite_ uuid

        Nothing ->
            describe "Errored Test" []


suite_ : Uuid -> Test
suite_ uuid =
    describe "Data.Category"
        [ describe "decoding"
            [ test "success" <|
                \_ ->
                    let
                        input =
                            """
                              { "id" : \"""" ++ uuidString ++ "\"" ++ """
                              , "name" : "123"
                              , "count" : 123 }
                            """

                        output =
                            { id = uuid
                            , name = "123"
                            , count = 123
                            }
                    in
                    Expect.equal (D.decodeString categoryDecoder input) <| Ok output
            , test "error" <|
                \_ ->
                    let
                        input =
                            """
                              { "id" : \"""" ++ uuidString ++ "\"" ++ """
                              , "name1" : "123"
                              , "count" : 123 }
                            """

                        output =
                            { id = uuid
                            , name = "123"
                            , count = 123
                            }
                    in
                    Expect.equal (Result.toMaybe (D.decodeString categoryDecoder input)) Nothing
            ]
        ]
