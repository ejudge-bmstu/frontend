module Api.Endpoint exposing (Endpoint, login, register, request)

import Http
import Url.Builder exposing (QueryParameter)
import Username exposing (Username)


request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , tracker : Maybe String
    }
    -> Cmd a
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , tracker = config.tracker
        , url = unwrap config.url
        }



-- TYPES


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    Url.Builder.crossOrigin host
        paths
        queryParams
        |> Endpoint


host : String
host =
    "http://localhost:3000"


login : Endpoint
login =
    url [ "login" ] []


register : Endpoint
register =
    url [ "register" ] []
