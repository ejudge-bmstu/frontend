module Api.Endpoint exposing
    ( Endpoint
    , addCategory
    , addTask
    , deleteCategory
    , editCategory
    , getTask
    , listCategories
    , listTasks
    , login
    , register
    , registerComplete
    , request
    )

import Http
import Url.Builder exposing (QueryParameter, int)
import Uuid exposing (Uuid, toString)


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


registerComplete : Endpoint
registerComplete =
    url [ "register", "confirm" ] []


addCategory : Endpoint
addCategory =
    url [ "category", "add" ] []


listCategories : Endpoint
listCategories =
    url [ "category", "list" ] []


deleteCategory : Endpoint
deleteCategory =
    url [ "category", "delete" ] []


editCategory : Endpoint
editCategory =
    url [ "category", "edit" ] []


listTasks : Uuid -> Int -> Endpoint
listTasks id page =
    url [ "categories", toString id, "tasks" ] [ int "page" page ]


getTask : Uuid -> Endpoint
getTask id =
    url [ "task", Uuid.toString id ] []


addTask : Endpoint
addTask =
    url [ "task", "add" ] []
