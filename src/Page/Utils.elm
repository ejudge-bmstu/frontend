module Page.Utils exposing
    ( ModalMessage(..)
    , divWithModal
    , showModal
    )

import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type ModalMessage
    = ModalMessage (Maybe String)


divWithModal : ModalMessage -> msg -> List (Attribute msg) -> List (Html msg) -> Html msg
divWithModal modalMessage msg attrs htmls =
    case modalMessage of
        ModalMessage (Just message) ->
            div attrs <| htmls ++ [ showModal message msg ]

        _ ->
            div attrs htmls


showModal : String -> msg -> Html msg
showModal message msg =
    Modal.config msg
        |> Modal.small
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [ text "Сообщение" ]
        |> Modal.body [] [ p [] [ text message ] ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick msg ]
                ]
                [ text "Закрыть" ]
            ]
        |> Modal.view Modal.shown
