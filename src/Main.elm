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
import List.Extra
import Quadtree exposing (..)
import Svg exposing (defs, pattern, rect, svg)
import Svg.Attributes exposing (fill, height, id, patternUnits, shapeRendering, stroke, strokeWidth, width, x, y)


{-| TODO: add background for non-transparent things and use it for layer text among other things
-}
config =
    { color =
        { background = Color.rgb255 218 218 218
        , ruler = Color.rgb255 218 218 218
        , opaqueBackground = Color.rgb255 140 140 140
        }
    , zIndex =
        { canvas = 0
        , floatingElement = 4
        }
    , defaultMargin = 10
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


type Tool
    = Pencil
    | Eraser


px : Float -> String
px x =
    String.fromFloat x ++ "px"



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
    , tool : Tool
    , colorpalette : List Color
    , canvasPointer : PointerData
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { canvas =
            { selectedLayerIndex = 1
            , layers =
                [ { layerEmpty | data = QuadLeaf Color.white, name = "Background" }
                , { layerEmpty | name = "Layer 1" }
                ]
            }
      , scale = 4
      , isRulerVisible = True
      , size = 512
      , color = Color.black
      , tool = Pencil
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


coordVisual2Data : Point -> Int -> Int -> Point
coordVisual2Data { x, y } scale size =
    { x = (x * (2 ^ scale)) // size
    , y = (y * (2 ^ scale)) // size
    }


doAtCoord : Point -> (Point -> Int -> Quadtree Color -> Quadtree Color) -> Model -> Canvas
doAtCoord visualCoord f model =
    model.canvas
        |> Canvas.updateSelectedLayer
            (\layer ->
                { layer
                    | data =
                        f (coordVisual2Data visualCoord model.scale model.size) model.scale layer.data
                }
            )



--tryUsePencil { x, y } model =
--    model.canvas
--        |> Canvas.updateSelectedLayer
--            (\layer ->
--                { layer
--                    | data =
--                        insertAtCoord (QuadLeaf model.color)
--                            { x = (x * (2 ^ model.scale)) // model.size
--                            , y = (y * (2 ^ model.scale)) // model.size
--                            }
--                            model.scale
--                            layer.data
--                }
--            )
--
--
--tryUseEraser { x, y } model =
--    model.canvas
--        |> Canvas.updateSelectedLayer
--            (\layer ->
--                { layer
--                    | data =
--                        insertAtCoord QuadEmpty
--                            { x = (x * (2 ^ model.scale)) // model.size
--                            , y = (y * (2 ^ model.scale)) // model.size
--                            }
--                            model.scale
--                            layer.data
--                }
--            )
-- UPDATE


type Msg
    = Reset
    | Noop
    | Log ( String, String )
    | ChangeScale Int
    | ChangeColor Color
    | RulerVisibleToggle
    | CanvasClearLayer
    | CanvasPointerInside Bool Pointer.Event
    | CanvasPointerPressed Bool Pointer.Event
    | ChangeSelectedLayer Int
    | ToggleLayerVisibility Int
    | AddNewLayer
    | RemoveSelectedLayer
    | TryUseTool Point
    | ChangeTool Tool


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

        ChangeTool tool ->
            ( { model | tool = tool }, Cmd.none )

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
                , TryUseTool
                    { x = ceiling x
                    , y = ceiling y
                    }
                    |> cmd
                )

            else
                ( { model | canvasPointer = newCanvasPointer }, Cmd.none )

        TryUseTool visualCoord ->
            if model.canvasPointer.isInside && model.canvasPointer.isPressed then
                let
                    newCanvas =
                        case model.tool of
                            Pencil ->
                                model
                                    |> doAtCoord visualCoord
                                        (\coord scale tree ->
                                            insertAtCoord (QuadLeaf model.color)
                                                coord
                                                scale
                                                tree
                                        )

                            Eraser ->
                                model
                                    |> doAtCoord visualCoord
                                        (\coord scale tree ->
                                            insertAtCoord QuadEmpty
                                                coord
                                                scale
                                                tree
                                        )
                in
                ( { model | canvas = newCanvas }, Cmd.none )

            else
                ( model, Cmd.none )

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

        ChangeSelectedLayer index ->
            let
                canvas =
                    model.canvas

                newCanvas =
                    { canvas | selectedLayerIndex = index }
            in
            ( { model | canvas = newCanvas }, Cmd.none )

        ToggleLayerVisibility index ->
            let
                newCanvas =
                    model.canvas
                        |> Canvas.updateLayer index (\layer -> { layer | isVisible = not layer.isVisible })
            in
            ( { model | canvas = newCanvas }, Cmd.none )

        AddNewLayer ->
            let
                newCanvas =
                    model.canvas
                        |> Canvas.addNewLayer
            in
            ( { model | canvas = newCanvas }
            , ChangeSelectedLayer (List.length model.canvas.layers) |> cmd
            )

        RemoveSelectedLayer ->
            let
                newCanvas =
                    model.canvas
                        |> Canvas.removeSelectedLayer
            in
            ( { model | canvas = newCanvas }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewModelDebug model =
    div [ style "margin" (px config.defaultMargin) ] [ text (Debug.toString model) ]


viewRuler scale maxSize isVisible =
    let
        halfThickness =
            1

        sizeStr =
            String.fromFloat (maxSize / (2 ^ scale))
    in
    div
        [ style "position" "absolute"
        , style "z-index" (String.fromFloat (config.zIndex.canvas + 1))
        , style "width" (px maxSize)
        , style "height" (px maxSize)
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
                TryUseTool
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
                , width (px maxSize)
                , height (px maxSize)
                , shapeRendering "crispEdges"
                ]
                []
            ]
        ]


viewCanvas model =
    let
        canvas =
            model.canvas
    in
    div
        [ style "position" "relative"
        , style "margin" (px config.defaultMargin)
        ]
        [ viewRuler (toFloat model.scale) (toFloat model.size) model.isRulerVisible
        , { canvas | layers = canvas.layers |> List.filter .isVisible }
            |> Canvas.mergeLayers
            |> viewQuadtree (toFloat model.size) colorTransparent
        ]


viewColorpaletteColor color =
    div
        [ style "width" (px 40)
        , style "height" (px 40)
        , style "background-color" (Color.toCssString color)
        , onClick (ChangeColor color)
        ]
        [ text "\u{200B}" ]


viewColorpalette model =
    div
        [ style "margin" (px config.defaultMargin)
        , style "max-width" (String.fromInt model.size ++ "px")
        , style "display" "flex"
        , style "flex-wrap" "wrap"
        ]
        (model.colorpalette |> List.map viewColorpaletteColor)


viewSelectedColor model =
    div
        [ style "width" (px 80)
        , style "height" (px 80)
        , style "background-color" (Color.toCssString model.color)
        , style "margin" (px config.defaultMargin)
        ]
        [ text "\u{200B}" ]


viewMsgButtons model =
    div [ style "margin" (px config.defaultMargin) ]
        [ button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick CanvasClearLayer ] [ text "Clear" ]
        , button [ onClick (ChangeScale (model.scale + 1)) ] [ text "+ Scale" ]
        , button [ onClick (ChangeScale (model.scale - 1)) ] [ text "- Scale" ]
        , button [ onClick RulerVisibleToggle ] [ text "Toggle Ruler" ]
        , button [ onClick (ChangeTool Pencil) ] [ text "✎" ]
        , button [ onClick (ChangeTool Eraser) ] [ text "▱" ]
        ]


viewLayer selectedLayerIndex i layer =
    let
        previewSize =
            80

        selectedBorder =
            2
    in
    div
        [ style "display" "flex" ]
        [ div
            [ style "width" (px previewSize)
            , style "height" (px previewSize)
            , onClick (ChangeSelectedLayer i)
            , style "box-sizing" "border-box"
            , if selectedLayerIndex == i then
                style "border" (px selectedBorder ++ " solid #D58F17")

              else
                style "border" "none"
            ]
            [ layer.data
                |> viewQuadtree
                    (if selectedLayerIndex == i then
                        toFloat previewSize - (selectedBorder * 2)

                     else
                        toFloat previewSize
                    )
                    config.color.background
            ]
        , div
            [ style "overflow" "auto"
            , style "width" (previewSize * 1.5 |> px)
            , style "display" "flex"
            , style "align-items" "center"
            , style "box-sizing" "border-box"
            , style "padding-left" "6px"
            ]
            [ text layer.name ]
        , button [ onClick (ToggleLayerVisibility i) ]
            [ if layer.isVisible then
                text "👁"

              else
                text "‿"
            ]
        ]


viewLayers model =
    div
        [ style "margin" (String.fromFloat config.defaultMargin)
        , style "position" "absolute"
        , style "top" "20px"
        , style "left" (px (toFloat model.size + config.defaultMargin * 2))
        , style "background-color" (Color.toCssString config.color.opaqueBackground)
        ]
        [ button [ onClick AddNewLayer ] [ text "+" ]
        , button [ onClick RemoveSelectedLayer ] [ text "-" ]
        , div
            [ style "display" "flex"
            , style "flex-wrap" "wrap"
            , style "flex-direction" "column-reverse"
            , style "border" "1px solid #000"
            ]
            (model.canvas.layers |> List.indexedMap (viewLayer model.canvas.selectedLayerIndex))
        ]


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
