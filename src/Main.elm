module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import ShapedArray exposing (..)


-- port shapeList : (( List Int, List a ) -> msg) -> Sub msg
-- port portName : Typename -> Cmd msg



-- MAIN
-- myShapedArray = ShapedArray


main =
    div [] [ text (Debug.toString test) ]
