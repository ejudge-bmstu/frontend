module Page.NotFound exposing (view)

-- import Asset

import Html exposing (Html, div, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)



-- VIEW


view : { title : String, content : Html msg }
view =
    { title = "Не найдено"
    , content = div [] [ text "Не найдено" ]
    }
