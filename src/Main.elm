module Main exposing (..)

import Array
import Array.Extra
import Browser
import Bytes exposing (Bytes)
import Canvas exposing (Canvas, CanvasLayer, layerEmpty)
import Color exposing (Color)
import Color.Blending
import Common exposing (..)
import Config exposing (..)
import Debug exposing (log)
import Dict
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Pointer as Pointer
import Image exposing (Image)
import Image.Color
import List.Extra
import Ports
import Quadtree exposing (..)
import Svg exposing (defs, pattern, rect, svg)
import Svg.Attributes exposing (fill, height, id, patternUnits, shapeRendering, stroke, strokeWidth, width, x, y)
import Svg.String
import Task


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


type alias ImageDownloadData =
    { format : ImageFormat
    , scale : Int
    , filename : String
    }


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


logCmd : String -> a -> Cmd Msg
logCmd msg data =
    data |> Debug.toString |> Log msg |> cmd


downloadRasterImage imageFormatData imageDownloadData image2bytes tree =
    -- max quadtree size = min image size
    tree
        |> Quadtree.scale imageDownloadData.scale
        |> Quadtree.toListWithDefault colorTransparent
        |> (\( minImageSize, imageData ) -> imageData |> Image.Color.fromList minImageSize)
        |> image2bytes
        |> File.Download.bytes (imageDownloadData.filename ++ "." ++ imageFormatData.extension) imageFormatData.mimeType



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
                [ { layerEmpty | name = "Background", data = QuadLeaf Color.white }
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



-- UPDATE


type Msg
    = Reset
    | Noop
    | Log String String
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
    | DownloadCanvas ImageDownloadData
    | UploadCanvas
    | UploadCanvasReady File
    | UploadCanvasLoaded (Maybe (Quadtree Color))
    | Test


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

        Log tag value ->
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
                        |> Canvas.addEmptyLayer
            in
            ( { model | canvas = newCanvas }, Cmd.none )

        RemoveSelectedLayer ->
            let
                newCanvas =
                    model.canvas
                        |> Canvas.removeSelectedLayer
            in
            ( { model | canvas = newCanvas }, Cmd.none )

        DownloadCanvas imageDownloadData ->
            let
                canvas =
                    model.canvas

                optimizedTree =
                    { canvas | layers = canvas.layers |> List.filter .isVisible }
                        |> Canvas.mergeLayers
                        |> Quadtree.optimize

                -- FIXME: do bmp and gif support transparency? choose another color if not. gif and svg dont work with big files. DON'T scale quadtree, it gets slow real fast, maybe ditch support for anything except png, it seems to work best
                downloadCmd =
                    case imageDownloadData.format of
                        Png ->
                            optimizedTree |> downloadRasterImage (imageFormatData Png) imageDownloadData Image.toPng

                        Bmp ->
                            optimizedTree |> downloadRasterImage (imageFormatData Bmp) imageDownloadData Image.toBmp

                        Gif ->
                            optimizedTree |> downloadRasterImage (imageFormatData Gif) imageDownloadData Image.toGif

                        Svg ->
                            let
                                -- max quadtree size = min image size
                                minImageSize =
                                    optimizedTree |> Quadtree.calculateMaxSize

                                formatData =
                                    imageFormatData Svg
                            in
                            optimizedTree
                                |> Quadtree.toSvgString (toFloat (minImageSize * (2 ^ imageDownloadData.scale))) colorTransparent
                                |> File.Download.string (imageDownloadData.filename ++ "." ++ formatData.extension) formatData.mimeType
            in
            ( model
            , downloadCmd
            )

        UploadCanvas ->
            ( model, File.Select.file ([ Png, Bmp ] |> List.map (imageFormatData >> .mimeType)) UploadCanvasReady )

        UploadCanvasReady file ->
            let
                image2quadtree : Image -> Quadtree Color
                image2quadtree image =
                    image
                        |> Image.Color.toList
                        |> List.map
                            (\color ->
                                if (color |> Color.toRgba |> .alpha) == 0 then
                                    Nothing

                                else
                                    Just color
                            )
                        |> Quadtree.fromList

                task =
                    File.toBytes file |> Task.map (\x -> Image.decode x |> Maybe.map image2quadtree)
            in
            ( model, Task.perform UploadCanvasLoaded task )

        UploadCanvasLoaded maybeQuadtree ->
            let
                canvas =
                    model.canvas

                newCanvas =
                    maybeQuadtree
                        |> Maybe.map
                            (\quadtree -> canvas |> Canvas.addLayer quadtree)
                        |> Maybe.withDefault canvas
            in
            ( { model | canvas = newCanvas }, Cmd.none )

        Test ->
            let
                l =
                    List.Extra.initialize 8 identity |> log "l"

                a =
                    l |> Array.fromList |> log "a"

                l1 =
                    l |> listInsertAt 3 69 |> log "l1"

                a1 =
                    a |> Array.Extra.insertAt 3 69 |> log "a1"

                test =
                    l1 == Array.toList a1 |> log "test"
            in
            ( model
              --, [ test ] |> logCmd "Test"
            , Cmd.none
            )



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


viewMsgButtons : Model -> Html Msg
viewMsgButtons model =
    div [ style "margin" (px config.defaultMargin) ]
        [ button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick CanvasClearLayer ] [ text "Clear" ]
        , button [ onClick (ChangeScale (model.scale + 1)) ] [ text "+ Scale" ]
        , button [ onClick (ChangeScale (model.scale - 1)) ] [ text "- Scale" ]
        , button [ onClick RulerVisibleToggle ] [ text "Toggle Ruler" ]
        , button [ onClick (ChangeTool Pencil) ] [ text "✎" ]
        , button [ onClick (ChangeTool Eraser) ] [ text "▱" ]
        , button [ onClick (DownloadCanvas { defaultDownloadImageData | format = Png }) ] [ text "⇓Png" ]
        , button [ onClick (DownloadCanvas { defaultDownloadImageData | format = Svg }) ] [ text "⇓Svg" ]
        , button [ onClick (DownloadCanvas { defaultDownloadImageData | format = Bmp }) ] [ text "⇓Bmp" ]
        , button [ onClick (DownloadCanvas { defaultDownloadImageData | format = Gif }) ] [ text "⇓Gif" ]
        , button [ onClick UploadCanvas ] [ text "⇑" ]
        , button [ onClick Test ] [ text "Test" ]
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
        , style "top" "44px"
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
