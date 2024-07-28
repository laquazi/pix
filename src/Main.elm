--port module Main exposing (..)


module Main exposing (..)

import Browser
import Canvas exposing (Canvas, CanvasLayer, layerEmpty)
import Color exposing (Color)
import Color.Blending
import Color.Oklch
import Common exposing (..)
import Debug exposing (log)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as JD
import Json.Encode as JE
import List.Extra
import Quadtree exposing (..)
import Result.Extra
import Svg exposing (defs, pattern, rect, svg)
import Svg.Attributes exposing (fill, height, id, patternUnits, shapeRendering, stroke, strokeWidth, width, x, y)



--port canvasRulerPressed : (JE.Value -> msg) -> Sub msg


config =
    { color =
        { background = Color.rgb255 238 238 238
        , ruler = Color.rgb255 238 238 238
        }
    , zIndex =
        { canvas = 0
        , floatingElement = 4
        }
    }


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


type alias PointerData =
    { isInside : Bool
    , isPressed : Bool
    }



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
    { canvas : Canvas
    , scale : Int
    , isRulerVisible : Bool
    , size : Int
    , color : Color
    , colorpalette : List Color
    , canvasPointer : PointerData
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { canvas =
            { selectedLayerIndex = 1
            , layers =
                [ { layerEmpty | data = QuadLeaf Color.white }
                , layerEmpty
                ]
            }
      , scale = 4
      , isRulerVisible = True
      , size = 512
      , color = Color.black
      , colorpalette =
            [ Color.black, Color.white ]
                ++ List.Extra.initialize 22
                    (\x ->
                        Color.Oklch.oklch 0.7 0.4 (10 / (toFloat x + 1))
                            |> Color.Oklch.toColor
                    )
      , canvasPointer = { isInside = False, isPressed = False }
      }
    , Cmd.none
    )


tryDraw { x, y } model =
    if model.canvasPointer.isInside && model.canvasPointer.isPressed then
        let
            newCanvas =
                model.canvas
                    |> Canvas.updateSelectedLayer
                        (\layer ->
                            { layer
                                | data =
                                    insertAtCoord model.color
                                        { x = (x * (2 ^ model.scale)) // model.size
                                        , y = (y * (2 ^ model.scale)) // model.size
                                        }
                                        model.scale
                                        layer.data
                            }
                        )
        in
        ( { model | canvas = newCanvas }, Cmd.none )

    else
        ( model, Cmd.none )



-- UPDATE


type Msg
    = Reset
    | Noop
    | Log ( String, String )
    | ChangeScale Int
    | ChangeColor Color
    | RulerVisibleToggle
    | CanvasClearLayer
    | TryDraw Point
    | CanvasPointerInside Bool Pointer.Event
    | CanvasPointerPressed Bool Pointer.Event
    | ChangeLayer Int


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

        CanvasPointerInside bool _ ->
            let
                canvasPointer =
                    model.canvasPointer

                newCanvasPointer =
                    { canvasPointer | isInside = bool }
            in
            ( { model | canvasPointer = newCanvasPointer }, Cmd.none )

        CanvasPointerPressed bool event ->
            let
                canvasPointer =
                    model.canvasPointer

                newCanvasPointer =
                    { canvasPointer | isPressed = bool }
            in
            if bool then
                let
                    ( x, y ) =
                        event.pointer.offsetPos
                in
                ( { model | canvasPointer = newCanvasPointer }
                , TryDraw
                    { x = ceiling x
                    , y = ceiling y
                    }
                    |> cmd
                )

            else
                ( { model | canvasPointer = newCanvasPointer }, Cmd.none )

        TryDraw canvasCoord ->
            model |> tryDraw canvasCoord

        Log ( tag, value ) ->
            let
                _ =
                    log tag value
            in
            ( model, Cmd.none )

        CanvasClearLayer ->
            let
                newCanvas =
                    model.canvas
                        |> Canvas.updateSelectedLayer (\layer -> { layer | data = QuadEmpty })
            in
            ( { model | canvas = newCanvas }, Cmd.none )

        Noop ->
            ( model, Cmd.none )

        ChangeLayer index ->
            let
                canvas =
                    model.canvas

                newCanvas =
                    { canvas | selectedLayerIndex = index }
            in
            ( { model | canvas = newCanvas }, Cmd.none )



-- SUBSCRIPTIONS
--
--decodePoint : JD.Decoder Point
--decodePoint =
--    JD.map2 Point (JD.field "x" JD.int) (JD.field "y" JD.int)


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
        , style "z-index" (String.fromFloat (config.zIndex.canvas + 1))
        , style "width" (maxSizeStr ++ "px")
        , style "height" (maxSizeStr ++ "px")
        , id "canvasRuler"
        , style "touch-action" "none"
        , Pointer.onDown (CanvasPointerPressed True)
        , Pointer.onUp (CanvasPointerPressed False)
        , Pointer.onEnter (CanvasPointerInside True)
        , Pointer.onLeave (CanvasPointerInside False)
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
                        , stroke (Color.toCssString config.color.ruler)
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
                , stroke (Color.toCssString config.color.ruler)
                , strokeWidth (String.fromFloat (halfThickness * 2))
                , width maxSizeStr
                , height maxSizeStr
                , shapeRendering "crispEdges"
                ]
                []
            ]
        ]


{-| TODO: draw layers separately instead of merging?
-}
viewCanvas model =
    div
        [ style "position" "relative"
        , style "margin" "10px"
        ]
        [ viewRuler (toFloat model.scale) (toFloat model.size) model.isRulerVisible
        , model.canvas
            |> Canvas.mergeLayers
            |> viewQuadtree (toFloat model.size) colorTransparent
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
        (model.colorpalette |> List.map viewColorpaletteColor)


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
        , button [ onClick CanvasClearLayer ] [ text "Clear" ]
        , button [ onClick (ChangeScale (model.scale + 1)) ] [ text "+ Scale" ]
        , button [ onClick (ChangeScale (model.scale - 1)) ] [ text "- Scale" ]
        , button [ onClick RulerVisibleToggle ] [ text "Toggle Ruler" ]
        ]


viewLayer selectedLayerIndex i layer =
    let
        size =
            80
    in
    div
        [ style "width" (String.fromFloat size ++ "px")
        , style "height" (String.fromFloat size ++ "px")
        , onClick (ChangeLayer i)
        , style "box-sizing" "border-box"
        , if selectedLayerIndex == i then
            style "border" "1px solid #EB6"

          else
            style "border" "none"
        ]
        [ layer.data
            |> viewQuadtree
                (if selectedLayerIndex == i then
                    toFloat size - 2

                 else
                    toFloat size
                )
                config.color.background
        ]


viewLayers model =
    div
        [ style "margin" "10px"
        , style "display" "flex"
        , style "flex-wrap" "wrap"
        , style "flex-direction" "column"
        ]
        (model.canvas.layers |> List.indexedMap (viewLayer model.canvas.selectedLayerIndex))


view : Model -> Html Msg
view model =
    div
        [ style "position" "absolute"
        , style "background-color" (Color.toCssString config.color.background)
        , style "width" "100%"
        , style "height" "100%"
        ]
        [ viewMsgButtons model
        , viewCanvas model
        , viewSelectedColor model
        , viewColorpalette model
        , viewLayers model

        -- , viewModelDebug model
        ]
