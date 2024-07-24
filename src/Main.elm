module Main exposing (..)

import Browser
import Color exposing (Color)
import Color.Oklab
import Color.Oklch
import Common exposing (..)
import Html exposing (Html, button, div, table, tbody, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Extra
import Quadtree exposing (..)
import Svg exposing (circle, defs, path, pattern, rect, svg)
import Svg.Attributes exposing (cx, cy, d, fill, height, id, patternUnits, r, rx, ry, shapeRendering, stroke, strokeWidth, viewBox, width, x, y)


quad0 =
    quadnode
        (QuadLeaf Color.red)
        (QuadLeaf Color.orange)
        (QuadLeaf Color.yellow)
        (quadnode
            (QuadLeaf Color.green)
            (QuadLeaf Color.blue)
            (QuadLeaf Color.purple)
            QuadEmpty
        )



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { canvas : Quadtree Color
    , scale : Int
    , isRulerVisible : Bool
    , size : Int
    , color : Color
    , colorpalette : List Color
    }


init : Model
init =
    { canvas = quad0
    , scale = 2
    , isRulerVisible = True
    , size = 512
    , color = Color.rgb 255 219 0
    , colorpalette =
        List.Extra.initialize 100
            (\x ->
                Color.Oklch.oklch 0.8 0.5 (10 / (toFloat x + 1))
                    |> Color.Oklch.toColor
            )
    }



-- UPDATE


type Msg
    = Reset
    | ScaleChange Int
    | ColorChange Color
    | RulerVisibleToggle
    | Draw Point


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            init

        ScaleChange newScale ->
            if newScale < 0 then
                { model | scale = 0 }

            else
                { model | scale = newScale }

        ColorChange color ->
            { model | color = color }

        RulerVisibleToggle ->
            { model | isRulerVisible = not model.isRulerVisible }

        Draw { x, y } ->
            { model | canvas = insertAtCoord model.color { x = x, y = y } model.scale model.canvas }



-- VIEW


viewModelDebug model =
    div [ style "margin" "10px" ] [ text (Debug.toString model) ]


viewRuler scale maxSize isVisible =
    let
        halfThickness =
            1

        maxSizeStr =
            String.fromFloat maxSize

        sizeStr =
            String.fromFloat (maxSize / (2 ^ scale))
    in
    div
        [ style "position" "absolute"
        , style "z-index" "1"
        , style "width" (maxSizeStr ++ "px")
        , style "height" (maxSizeStr ++ "px")
        , if isVisible then
            style "visibility" "visible"

          else
            style "visibility" "hidden"
        ]
        [ svg
            [ width "100%"
            , height "100%"
            ]
            [ defs []
                [ pattern
                    [ id "grid"
                    , width sizeStr
                    , height sizeStr
                    , patternUnits "userSpaceOnUse"
                    ]
                    [ rect
                        [ fill "none"
                        , stroke "#333"
                        , strokeWidth (String.fromFloat halfThickness)
                        , width sizeStr
                        , height sizeStr
                        , shapeRendering "crispEdges"
                        ]
                        []
                    ]
                ]
            , rect
                [ width "100%"
                , height "100%"
                , fill "url(#grid)"
                , shapeRendering "crispEdges"
                ]
                []
            , rect
                [ fill "none"
                , stroke "#333"
                , strokeWidth (String.fromFloat (halfThickness * 2))
                , width maxSizeStr
                , height maxSizeStr
                , shapeRendering "crispEdges"
                ]
                []
            ]
        ]


viewCanvas model =
    div
        [ style "position" "relative"
        , style "margin" "10px"
        ]
        [ viewRuler (toFloat model.scale) (toFloat model.size) model.isRulerVisible
        , viewQuadtree model.canvas (toFloat model.size)
        ]


viewColorpaletteColor color =
    div
        [ style "width" "40px"
        , style "height" "40px"
        , style "background-color" (Color.toCssString color)
        , onClick (ColorChange color)
        ]
        [ text "\u{200B}" ]


viewColorpalette model =
    div
        [ style "margin" "10px"
        , style "max-width" (String.fromInt model.size ++ "px")
        , style "display" "flex"
        , style "flex-wrap" "wrap"
        ]
        (model.colorpalette |> List.map (\x -> viewColorpaletteColor x))


viewMsgButtons model =
    div [ style "margin" "10px" ]
        [ button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick (ScaleChange (model.scale + 1)) ] [ text "Increment Scale" ]
        , button [ onClick (ScaleChange (model.scale - 1)) ] [ text "Decrement Scale" ]
        , button [ onClick RulerVisibleToggle ] [ text "Toggle Ruler" ]
        , button [ onClick (Draw { x = 0, y = 0 }) ] [ text "draw(0,0)" ]
        , button [ onClick (Draw { x = 1, y = 0 }) ] [ text "draw(0,1)" ]
        , button [ onClick (Draw { x = 2, y = 0 }) ] [ text "draw(0,2)" ]
        , button [ onClick (Draw { x = 3, y = 0 }) ] [ text "draw(0,3)" ]
        , button [ onClick (Draw { x = 0, y = 1 }) ] [ text "draw(1,0)" ]
        , button [ onClick (Draw { x = 1, y = 1 }) ] [ text "draw(1,1)" ]
        , button [ onClick (Draw { x = 2, y = 1 }) ] [ text "draw(1,2)" ]
        , button [ onClick (Draw { x = 3, y = 1 }) ] [ text "draw(1,3)" ]
        , button [ onClick (Draw { x = 0, y = 2 }) ] [ text "draw(2,0)" ]
        , button [ onClick (Draw { x = 1, y = 2 }) ] [ text "draw(2,1)" ]
        , button [ onClick (Draw { x = 2, y = 2 }) ] [ text "draw(2,2)" ]
        , button [ onClick (Draw { x = 3, y = 2 }) ] [ text "draw(2,3)" ]
        , button [ onClick (Draw { x = 0, y = 3 }) ] [ text "draw(3,0)" ]
        , button [ onClick (Draw { x = 1, y = 3 }) ] [ text "draw(3,1)" ]
        , button [ onClick (Draw { x = 2, y = 3 }) ] [ text "draw(3,2)" ]
        , button [ onClick (Draw { x = 3, y = 3 }) ] [ text "draw(3,3)" ]
        ]


viewSelectedColor model =
    div
        [ style "width" "80px"
        , style "height" "80px"
        , style "background-color" (Color.toCssString model.color)
        , style "margin" "10px"
        ]
        [ text "\u{200B}" ]


view : Model -> Html Msg
view model =
    div
        [ style "position" "absolute" ]
        [ viewMsgButtons model
        , viewCanvas model
        , viewSelectedColor model
        , viewColorpalette model
        , viewModelDebug model
        ]
