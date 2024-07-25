port module Main exposing (..)

import Browser
import Color exposing (Color)
import Color.Oklab
import Color.Oklch
import Common exposing (..)
import Debug exposing (log)
import Html exposing (Html, button, div, table, tbody, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Json.Encode as JE
import List.Extra
import Quadtree exposing (..)
import Result.Extra
import Svg exposing (circle, defs, path, pattern, rect, svg)
import Svg.Attributes exposing (cx, cy, d, fill, height, id, patternUnits, r, rx, ry, shapeRendering, stroke, strokeWidth, viewBox, width, x, y)


port canvasRulerPressed : (JE.Value -> msg) -> Sub msg


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
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { canvas : Quadtree Color
    , scale : Int
    , isRulerVisible : Bool
    , size : Int
    , color : Color
    , colorpalette : List Color
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { canvas = quad0
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
    , Cmd.none
    )



-- UPDATE


type Msg
    = Reset
    | ChangeScale Int
    | ChangeColor Color
    | RulerVisibleToggle
    | Draw Point
    | Log ( String, String )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init ()

        ChangeScale newScale ->
            ( if newScale < 0 then
                { model | scale = 0 }

              else
                { model | scale = newScale }
            , Cmd.none
            )

        ChangeColor color ->
            ( { model | color = color }, Cmd.none )

        RulerVisibleToggle ->
            ( { model | isRulerVisible = not model.isRulerVisible }, Cmd.none )

        Draw { x, y } ->
            ( { model | canvas = insertAtCoord model.color { x = x, y = y } model.scale model.canvas }, Cmd.none )

        Log ( tag, value ) ->
            let
                l =
                    log tag value
            in
            ( model, Cmd.none )



-- SUBSCRIPTIONS


decodePoint : JD.Decoder Point
decodePoint =
    JD.map2 Point (JD.field "x" JD.int) (JD.field "y" JD.int)


subscriptions : Model -> Sub Msg
subscriptions _ =
    canvasRulerPressed
        (\jsPoint ->
            jsPoint
                |> JD.decodeValue decodePoint
                |> Result.Extra.unpack
                    (\error -> Log ( "Error", JD.errorToString error ))
                    Draw
        )



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
        , id "canvasRuler"
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
        , onClick (ChangeColor color)
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
        , button [ onClick (ChangeScale (model.scale + 1)) ] [ text "Increment Scale" ]
        , button [ onClick (ChangeScale (model.scale - 1)) ] [ text "Decrement Scale" ]
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
