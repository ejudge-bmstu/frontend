module Data.TaskResult exposing
    ( TaskResult
    , decodeTime
    , taskResultDecoder
    )

import Api
import Api.Endpoint as Endpoint
import Bootstrap.Accordion as Accordion
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Cred exposing (Cred)
import Data.Date exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Page.Utils exposing (..)
import Role
import Route
import Session exposing (Session)
import Task
import Time
import Uuid exposing (Uuid)
import Viewer exposing (Viewer)


decodeTime : D.Decoder Time.Posix
decodeTime =
    D.int
        |> D.andThen
            (\ms ->
                D.succeed <| Time.millisToPosix ms
            )


type alias TaskResult =
    { taskName : String
    , taskId : Uuid
    , userName : String
    , userId : Uuid
    , passed : Maybe Int
    , total : Int
    , message : Maybe String
    , result : String
    , date : Time.Posix
    }


taskResultDecoder : Decoder TaskResult
taskResultDecoder =
    D.succeed TaskResult
        |> D.required "task_name" D.string
        |> D.required "task_id" Uuid.decoder
        |> D.required "user_name" D.string
        |> D.required "user_id" Uuid.decoder
        |> D.required "passed" (D.maybe D.int)
        |> D.required "total" D.int
        |> D.required "message" (D.maybe D.string)
        |> D.required "result" D.string
        |> D.required "date" decodeTime



-- (D.field "task_name" D.string)
-- (D.field "task_id" Uuid.decoder)
-- (D.field "passed" (D.maybe D.int))
-- (D.field "total" D.int)
-- (D.field "message" (D.maybe D.string))
-- (D.field "result" D.string)
-- (D.field "date" decodeTime)
