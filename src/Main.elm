module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0


plus : Int -> Int -> Int
plus num item =
    item + num



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset
    | Increment10


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            plus 1 model

        Decrement ->
            model - 1

        Reset ->
            init

        Increment10 ->
            plus 10 model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick Increment10 ] [ text "+ 10" ]
        ]
