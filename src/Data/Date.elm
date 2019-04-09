module Data.Date exposing (mkDate, monthToInt)

import Time


monthToInt m =
    case m of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


mkDate : Time.Zone -> Time.Posix -> String
mkDate zone posix =
    let
        day =
            String.fromInt <| Time.toDay zone posix

        month =
            String.fromInt << monthToInt <| Time.toMonth zone posix

        year =
            String.fromInt <| Time.toYear zone posix
    in
    day ++ "/" ++ month ++ "/" ++ year
