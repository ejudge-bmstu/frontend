module Data.ReportAccessTest exposing (suite)

import Data.Category exposing (..)
import Data.ReportAccess exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as D
import Route exposing (..)
import Test exposing (..)
import Url exposing (Url)
import Uuid exposing (Uuid)


suite : Test
suite =
    describe "Data.ReportAccess"
        [ test "toString1" <|
            \_ ->
                let
                    input =
                        FullAccess

                    output =
                        "full_access"
                in
                Expect.equal (reportAccessToString input) <| output
        , test "toString2" <|
            \_ ->
                let
                    input =
                        NoAccess

                    output =
                        "no_access"
                in
                Expect.equal (reportAccessToString input) <| output
        , test "reportAccessFromStringOk" <|
            \_ ->
                let
                    input =
                        "full_access"

                    output =
                        Just FullAccess
                in
                Expect.equal (reportAccessFromString input) output
        , test "reportAccessFromStringBad" <|
            \_ ->
                let
                    input =
                        "fullaccess"

                    output =
                        Nothing
                in
                Expect.equal (reportAccessFromString input) output
        ]
