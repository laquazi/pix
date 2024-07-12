module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Point =
    { x : Int
    , y : Int
    }


type alias Color =
    String


type alias Canvas =
    { scale : Point
    , colors : List Color
    , size : Point
    }



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { canvas : Canvas
    }


init : Model
init =
    { canvas =
        { scale = { x = 1, y = 1 }
        , colors = [ "#fff", "#000", "#fff", "#000" ]
        , size = { x = 2, y = 2 }
        }
    }



-- UPDATE


type Msg
    = Reset
    | Test


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- Increment ->
        --     { model | scale = { x = model.scale.x + 1, y = model.scale.y + 1 } }
        Reset ->
            init

        Test ->
            { canvas =
                { scale = { x = 1, y = 1 }
                , colors = [ "#C0C0C0", "#C0C0C0", "#fff", "#000", "#C0C0C0", "#C0C0C0", "#000", "#fff" ]
                , size = { x = 3, y = 3 }
                }
            }



-- VIEW
-- viewPixel scale color =
--     button [ onClick Test ] [ text "s-" ]
-- viewCanvas canvas =
--     div []
--         (canvas.colors |> List.map (\x -> viewPixel canvas.scale x))

renderPixel

renderCanvasRow row =
    div [ style "display" "flex" ] (\x -> viewPixel canvas.scale x)


renderCanvas canvas =
    div [] (List.map renderCanvasRow canvas)


view model =
    div []
        [ button [ onClick Reset ]
            [ text "Reset" ]
        , viewCanvas
            model.canvas
        ]
-- view : Model -> Html Msg
-- view model =
--     div []
--         [ renderGrid model.grid
--         , text ("Generation: " ++ String.fromInt model.generation)
--         ]


-- renderGrid : Grid -> Html Msg
-- renderGrid grid =
--     div [] (List.map renderRow grid)


-- renderRow : List Cell -> Html Msg
-- renderRow row =
--     div [ style "display" "flex" ] (List.map renderCell row)


-- renderCell : Cell -> Html Msg
-- renderCell cell =
--     div
--         [ style "width" "32px"
--         , style "height" "32px"
--         , style "border" ".5px solid #ccc"

--         -- , style "transition" "background-color 50ms ease-out"
--         , style "background-color"
--             (case cell of
--                 Live ->
--                     "#222"

--                 Dead ->
--                     "#fff"
--             )
--         ]
--         []
