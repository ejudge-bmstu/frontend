module Page.Utils exposing
    ( ModalMessage(..)
    , divWithModal
    , showModal
    )

import Api
import Api.Endpoint as Endpoint
import Bootstrap.Accordion as Accordion
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Table as Table
import Bootstrap.Text as Text
import Bootstrap.Utilities.Size as Size
import Cred exposing (Cred)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)


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
