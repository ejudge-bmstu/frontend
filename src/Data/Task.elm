module Data.Task exposing
    ( Category
    , ExampleTest
    , Limit
    , Task
    , categoryDecoder
    , exampleDecoder
    , limitDecoder
    , taskDecoder
    )

import Data.ReportAccess exposing (..)
import Json.Decode as D exposing (Decoder)
import Uuid exposing (Uuid)


type alias Task =
    { name : String
    , description : String
    , category : Category
    , access : ReportAccess
    , limits : List Limit
    , examples : List ExampleTest
    }


type alias Category =
    { id : Uuid
    , name : String
    }


categoryDecoder : Decoder Category
categoryDecoder =
    D.map2 Category
        (D.field "id" Uuid.decoder)
        (D.field "name" D.string)


taskDecoder : Decoder Task
taskDecoder =
    D.map6 Task
        (D.field "name" D.string)
        (D.field "description" D.string)
        (D.field "category" categoryDecoder)
        (D.field "access_report" (D.map (Maybe.withDefault FullAccess << reportAccessFromString) D.string))
        (D.field "limits" (D.list limitDecoder))
        (D.field "examples" (D.list exampleDecoder))


type alias ExampleTest =
    { input : String
    , output : String
    }


exampleDecoder : Decoder ExampleTest
exampleDecoder =
    D.map2 ExampleTest
        (D.field "input" D.string)
        (D.field "output" D.string)


type alias Limit =
    { language : String
    , memory : Int
    , time : Int
    }


limitDecoder : Decoder Limit
limitDecoder =
    D.map3 Limit
        (D.field "language" D.string)
        (D.field "memory" D.int)
        (D.field "time" D.int)
