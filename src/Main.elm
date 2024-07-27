--port module Main exposing (..)


module Main exposing (..)

import Browser
import Color exposing (Color)
import Color.Oklab
import Color.Oklch
import Common exposing (..)
import Debug exposing (log)
import Html exposing (Html, button, div, table, tbody, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as JD
import Json.Encode as JE
import List.Extra
import Quadtree exposing (..)
import Result.Extra
import Svg exposing (circle, defs, path, pattern, rect, svg)
import Svg.Attributes exposing (cx, cy, d, fill, height, id, patternUnits, r, rx, ry, shapeRendering, stroke, strokeWidth, viewBox, width, x, y)
import Task



--port canvasRulerPressed : (JE.Value -> msg) -> Sub msg


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
    , isDrawing : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { canvas = QuadEmpty
      , scale = 3
      , isRulerVisible = True
      , size = 512
      , color = Color.rgb 255 219 0
      , colorpalette =
            [ Color.black, Color.white ]
                ++ List.Extra.initialize 22
                    (\x ->
                        Color.Oklch.oklch 0.7 0.4 (10 / (toFloat x + 1))
                            |> Color.Oklch.toColor
                    )
      , isDrawing = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Reset
    | Noop
    | Log ( String, String )
    | ChangeScale Int
    | ChangeColor Color
    | RulerVisibleToggle
    | ClearCanvas
    | AllowDraw Bool
    | TryDraw Point


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

        AllowDraw isAllowed ->
            ( { model | isDrawing = isAllowed }, Cmd.none )

        TryDraw { x, y } ->
            if model.isDrawing then
                ( { model
                    | canvas =
                        insertAtCoord model.color
                            { x = (x * (2 ^ model.scale)) // model.size
                            , y = (y * (2 ^ model.scale)) // model.size
                            }
                            model.scale
                            model.canvas
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Log ( tag, value ) ->
            let
                l =
                    log tag value
            in
            ( model, Cmd.none )

        ClearCanvas ->
            ( { model | canvas = QuadEmpty }, Cmd.none )

        Noop ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


decodePoint : JD.Decoder Point
decodePoint =
    JD.map2 Point (JD.field "x" JD.int) (JD.field "y" JD.int)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--canvasRulerPressed
--    (\jsPoint ->
--        jsPoint
--            |> JD.decodeValue decodePoint
--            |> Result.Extra.unpack
--                (\error -> Log ( "Error", JD.errorToString error ))
--                Draw
--    )
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
        , style "touch-action" "none"
        , Pointer.onDown
            (\_ ->
                AllowDraw True
            )
        , Pointer.onUp (\_ -> AllowDraw False)
        , Pointer.onMove
            (\event ->
                let
                    ( x, y ) =
                        event.pointer.offsetPos
                in
                TryDraw
                    { x = ceiling x
                    , y = ceiling y
                    }
            )
        , if isVisible then
            style "opacity" "100%"

          else
            style "opacity" "0%"
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


viewSelectedColor model =
    div
        [ style "width" "80px"
        , style "height" "80px"
        , style "background-color" (Color.toCssString model.color)
        , style "margin" "10px"
        ]
        [ text "\u{200B}" ]


viewMsgButtons model =
    div [ style "margin" "10px" ]
        [ button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick ClearCanvas ] [ text "Clear" ]
        , button [ onClick (ChangeScale (model.scale + 1)) ] [ text "+ Scale" ]
        , button [ onClick (ChangeScale (model.scale - 1)) ] [ text "- Scale" ]
        , button [ onClick RulerVisibleToggle ] [ text "Toggle Ruler" ]
        ]


view : Model -> Html Msg
view model =
    div
        [ style "position" "absolute"
        , style "background-color" "#EEE"
        , style "width" "100%"
        , style "height" "100%"
        ]
        [ viewMsgButtons model
        , viewCanvas model
        , viewSelectedColor model
        , viewColorpalette model

        -- , viewModelDebug model
        ]
