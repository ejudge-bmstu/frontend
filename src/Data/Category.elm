module Data.Category exposing (Category, categoryDecoder)

import Json.Decode as D exposing (Decoder)
import Uuid exposing (Uuid)


type alias Category =
    { id : Uuid
    , name : String
    , count : Int
    }


categoryDecoder : Decoder Category
categoryDecoder =
    D.map3 Category
        (D.field "id" Uuid.decoder)
        (D.field "name" D.string)
        (D.field "count" D.int)
