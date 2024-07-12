module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Point =
    { x : Int
    , y : Int
    }



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { scale : Point,
      canvas : Image List
    }


init : Model
init =
    { scale = { x = 1, y = 1 }
    }



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | scale = { x = model.scale.x + 1, y = model.scale.y + 1 } }

        Decrement ->
            { model | scale = { x = model.scale.x - 1, y = model.scale.y - 1 } }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (model.scale.x |> String.fromInt) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
