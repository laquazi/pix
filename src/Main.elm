module Main exposing (..)

import Browser
import Canvas exposing (Canvas, CanvasLayer, layerEmpty)
import Color exposing (Color)
import Color.Blending
import Color.Convert
import Color.Interpolate
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


defaultColorpalette : List Color
defaultColorpalette =
    [ Color.rgb255 255 138 128 -- redA100
    , Color.rgb255 255 128 171 -- pinkA100
    , Color.rgb255 234 128 252 -- purpleA100
    , Color.rgb255 179 136 255 -- deepPurpleA100
    , Color.rgb255 140 158 255 -- indigoA100
    , Color.rgb255 130 177 255 -- blueA100
    , Color.rgb255 128 216 255 -- lightBlueA100
    , Color.rgb255 132 255 255 -- cyanA100
    , Color.rgb255 167 255 235 -- tealA100
    , Color.rgb255 185 246 202 -- greenA100
    , Color.rgb255 204 255 144 -- lightGreenA100
    , Color.rgb255 244 255 129 -- limeA100
    , Color.rgb255 255 255 141 -- yellowA100
    , Color.rgb255 255 229 127 -- amberA100
    , Color.rgb255 255 209 128 -- orangeA100
    , Color.rgb255 255 158 128 -- deepOrangeA100
    , Color.rgb255 255 82 82 -- redA200
    , Color.rgb255 255 64 129 -- pinkA200
    , Color.rgb255 224 64 251 -- purpleA200
    , Color.rgb255 124 77 255 -- deepPurpleA200
    , Color.rgb255 83 109 254 -- indigoA200
    , Color.rgb255 68 138 255 -- blueA200
    , Color.rgb255 64 196 255 -- lightBlueA200
    , Color.rgb255 24 255 255 -- cyanA200
    , Color.rgb255 100 255 218 -- tealA200
    , Color.rgb255 105 240 174 -- greenA200
    , Color.rgb255 178 255 89 -- lightGreenA200
    , Color.rgb255 238 255 65 -- limeA200
    , Color.rgb255 255 255 0 -- yellowA200
    , Color.rgb255 255 215 64 -- amberA200
    , Color.rgb255 255 171 64 -- orangeA200
    , Color.rgb255 255 110 64 -- deepOrangeA200
    , Color.rgb255 255 23 68 -- redA400
    , Color.rgb255 245 0 87 -- pinkA400
    , Color.rgb255 213 0 249 -- purpleA400
    , Color.rgb255 101 31 255 -- deepPurpleA400
    , Color.rgb255 61 90 254 -- indigoA400
    , Color.rgb255 41 121 255 -- blueA400
    , Color.rgb255 0 176 255 -- lightBlueA400
    , Color.rgb255 0 229 255 -- cyanA400
    , Color.rgb255 29 233 182 -- tealA400
    , Color.rgb255 0 230 118 -- greenA400
    , Color.rgb255 118 255 3 -- lightGreenA400
    , Color.rgb255 198 255 0 -- limeA400
    , Color.rgb255 255 234 0 -- yellowA400
    , Color.rgb255 255 196 0 -- amberA400
    , Color.rgb255 255 145 0 -- orangeA400
    , Color.rgb255 255 61 0 -- deepOrangeA400
    , Color.rgb255 213 0 0 -- redA700
    , Color.rgb255 197 17 98 -- pinkA700
    , Color.rgb255 170 0 255 -- purpleA700
    , Color.rgb255 98 0 234 -- deepPurpleA700
    , Color.rgb255 48 79 254 -- indigoA700
    , Color.rgb255 41 98 255 -- blueA700
    , Color.rgb255 0 145 234 -- lightBlueA700
    , Color.rgb255 0 184 212 -- cyanA700
    , Color.rgb255 0 191 165 -- tealA700
    , Color.rgb255 0 200 83 -- greenA700
    , Color.rgb255 100 221 23 -- lightGreenA700
    , Color.rgb255 174 234 0 -- limeA700
    , Color.rgb255 255 214 0 -- yellowA700
    , Color.rgb255 255 171 0 -- amberA700
    , Color.rgb255 255 109 0 -- orangeA700
    , Color.rgb255 221 44 0 -- deepOrangeA700
    ]
        ++ List.Extra.initialize 15 (\x -> Color.Interpolate.interpolate Color.Interpolate.RGB Color.white Color.black (toFloat x / 15))
        ++ [ Color.black ]
        |> List.reverse



--|> List.sortBy (\x -> 1 - (Color.toHsla x |> .lightness))
--let
--    maxSize : Float
--    maxSize =
--        64
--
--    slSize : Float
--    slSize =
--        4
--
--    hueSize : Float
--    hueSize =
--        maxSize / slSize
--
--    hue =
--        List.Extra.initialize (round hueSize)
--            (\x -> toFloat x / hueSize)
--            |> List.Extra.cycle (round maxSize)
--
--    sat =
--        List.Extra.initialize (round slSize)
--            (\x -> toFloat x / slSize)
--            |> List.Extra.unique
--            |> List.Extra.cycle (round maxSize)
--
--    lig =
--        sat
--in
--List.map3
--    (\h s l -> Color.hsl h s l)
--    hue
--    sat
--    lig
--    |> log "colors"


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
      , colorpalette = defaultColorpalette
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
        [ style "width" (px 32)
        , style "height" (px 32)
        , style "background-color" (Color.toCssString color)
        , onClick (ChangeColor color)
        ]
        [ text "\u{200B}" ]


viewColorpalette model =
    div
        [ style "margin" (px config.defaultMargin)
        , style "max-width" (String.fromInt (32 * 16) ++ "px")
        , style "display" "flex"
        , style "flex-wrap" "wrap"
        ]
        (model.colorpalette |> List.map viewColorpaletteColor)


viewSelectedColor model =
    div
        [ style "width" (px 64)
        , style "height" (px 64)
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
            64

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
