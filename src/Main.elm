module Main exposing (..)

import Browser
import Canvas exposing (Canvas, CanvasLayer)
import Color exposing (Color)
import Color.Blending
import Color.Convert
import Common exposing (..)
import Config exposing (..)
import Debug exposing (log)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (on, onClick, onDoubleClick, onInput)
import Html.Events.Extra.Pointer as Pointer
import Image exposing (Image)
import Image.Advanced
import Image.Color
import List.Extra
import Maybe.Extra
import Ports
import Quadtree exposing (..)
import Result.Extra
import Set exposing (Set)
import Svg exposing (defs, pattern, rect, svg)
import Svg.Attributes exposing (fill, height, id, patternUnits, shapeRendering, stroke, strokeWidth, width, x, y)
import Task
import Time


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
    , isDown : Bool
    }


type Tool
    = Pencil
    | Eraser


type alias ImageDownloadData =
    { format : ImageFormat
    , scale : Int
    , filename : String
    }


canvasLayerElementId layer =
    "layer:" ++ layer.name


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


captureLayer capture index event layers =
    layers
        |> List.Extra.getAt index
        |> Maybe.map
            (\layer ->
                Ports.encodeCapturePointerById event.pointerId (canvasLayerElementId layer)
                    |> capture
            )
        |> Maybe.withDefault Cmd.none



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
    , holdingLayerIndices : Set Int
    , maybeRenameLayerIndex : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { canvas = Canvas.default
      , scale = 3
      , isRulerVisible = True
      , size = 512
      , color = Color.black
      , tool = Pencil
      , colorpalette = defaultColorpalette
      , canvasPointer = { isInside = False, isDown = False }
      , holdingLayerIndices = Set.empty
      , maybeRenameLayerIndex = Nothing
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
    | CanvasPointerMoved Pointer.Event
    | ChangeSelectedLayer Int
    | ToggleLayerVisibility Int
    | AddNewLayer
    | AddCompositeLayer
    | RemoveSelectedLayer
    | LayerRenamePointerPressed Int
    | LayerRenameCanceled
    | LayerRename Int String
    | LayerHoldPointerPressed Int Bool Pointer.Event
    | LayerHoldPointerMoved Int Pointer.Event
    | TryUseTool Point
    | ChangeTool Tool
    | DownloadCanvas ImageDownloadData
    | UploadCanvas
    | UploadCanvasReady File
    | UploadCanvasLoaded (Maybe (Quadtree Color))
    | MsgBatch (List Msg)
    | RunCmd (Cmd Msg)
    | WithCmd Msg (Cmd Msg)
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

        CanvasPointerInside isInside _ ->
            let
                canvasPointer =
                    model.canvasPointer

                newCanvasPointer =
                    { canvasPointer | isInside = isInside }
            in
            ( { model | canvasPointer = newCanvasPointer }, Cmd.none )

        CanvasPointerPressed isDown event ->
            let
                canvasPointer =
                    model.canvasPointer

                newCanvasPointer =
                    { canvasPointer | isDown = isDown }
            in
            if isDown then
                let
                    ( x, y ) =
                        event.pointer.offsetPos
                in
                ( { model | canvasPointer = newCanvasPointer }
                , Cmd.batch
                    [ TryUseTool
                        { x = ceiling x
                        , y = ceiling y
                        }
                        |> cmd
                    , Ports.encodeCapturePointerById event.pointerId "canvasRuler"
                        |> Ports.pointerSetCaptureById
                    ]
                )

            else
                ( { model | canvasPointer = newCanvasPointer }
                , Ports.encodeCapturePointerById event.pointerId "canvasRuler"
                    |> Ports.pointerReleaseCaptureById
                )

        CanvasPointerMoved event ->
            let
                visualCoord =
                    event.pointer.offsetPos
                        |> (\( x, y ) ->
                                { x = ceiling x
                                , y = ceiling y
                                }
                           )

                isBounded =
                    visualCoord.x < model.size && visualCoord.y < model.size && visualCoord.x >= 0 && visualCoord.y >= 0

                canvasPointer =
                    model.canvasPointer
            in
            if isBounded then
                let
                    newCanvasPointer =
                        { canvasPointer | isInside = True }
                in
                ( { model | canvasPointer = newCanvasPointer }
                , TryUseTool visualCoord |> cmd
                )

            else
                let
                    newCanvasPointer =
                        { canvasPointer | isInside = False }
                in
                ( { model | canvasPointer = newCanvasPointer }, Cmd.none )

        TryUseTool visualCoord ->
            if model.canvasPointer.isInside && model.canvasPointer.isDown then
                let
                    newCanvas =
                        case model.tool of
                            Pencil ->
                                model
                                    |> doAtCoord visualCoord
                                        (insertAtCoord (QuadLeaf model.color))

                            Eraser ->
                                model
                                    |> doAtCoord visualCoord
                                        (insertAtCoord QuadEmpty)
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

        AddCompositeLayer ->
            let
                newCanvas =
                    model.canvas
                        |> Canvas.addCompositeLayer
            in
            ( { model | canvas = newCanvas }, Cmd.none )

        RemoveSelectedLayer ->
            let
                newCanvas =
                    model.canvas
                        |> Canvas.removeSelectedLayer
            in
            ( { model | canvas = newCanvas }, Cmd.none )

        LayerRenamePointerPressed index ->
            ( { model | maybeRenameLayerIndex = Just index }, Cmd.none )

        LayerRenameCanceled ->
            ( { model | maybeRenameLayerIndex = Nothing }, Cmd.none )

        LayerRename index name ->
            let
                canvas =
                    model.canvas

                newCanvas =
                    canvas
                        |> Canvas.updateLayer index (\layer -> { layer | name = name })
            in
            ( { model | canvas = newCanvas }, Cmd.none )

        LayerHoldPointerPressed index isDown event ->
            let
                holdingLayerIndices =
                    model.holdingLayerIndices

                ( newHoldingLayerIndices, newCmd ) =
                    if isDown then
                        ( holdingLayerIndices |> Set.insert index
                        , captureLayer Ports.pointerSetCaptureById index event model.canvas.layers
                        )

                    else
                        ( holdingLayerIndices |> Set.remove index
                        , captureLayer Ports.pointerReleaseCaptureById index event model.canvas.layers
                        )
            in
            ( { model | holdingLayerIndices = newHoldingLayerIndices }, newCmd )

        LayerHoldPointerMoved index event ->
            let
                visualCoord =
                    event.pointer.offsetPos
                        |> (\( x, y ) ->
                                { x = ceiling x
                                , y = ceiling y
                                }
                           )

                isHolding =
                    model.holdingLayerIndices |> Set.member index

                isHeightBounded =
                    visualCoord.y < config.layerPreviewSize && visualCoord.y >= 0

                indexOffset =
                    if visualCoord.y > config.layerPreviewSize then
                        visualCoord.y // -config.layerPreviewSize

                    else
                        visualCoord.y // -config.layerPreviewSize + 1

                swapIndex =
                    index + indexOffset |> clamp 0 (List.length model.canvas.layers - 1)
            in
            if isHolding && not isHeightBounded && swapIndex /= index then
                let
                    canvas =
                        model.canvas

                    newLayers =
                        canvas.layers
                            |> List.Extra.swapAt index swapIndex

                    newCanvas =
                        if canvas.selectedLayerIndex == index then
                            { canvas | layers = newLayers, selectedLayerIndex = swapIndex }

                        else if canvas.selectedLayerIndex == swapIndex then
                            { canvas | layers = newLayers, selectedLayerIndex = index }

                        else
                            { canvas | layers = newLayers }

                    newHoldingLayerIndices =
                        model.holdingLayerIndices |> Set.insert swapIndex |> Set.remove index
                in
                ( { model | canvas = newCanvas, holdingLayerIndices = newHoldingLayerIndices }
                , Cmd.batch
                    [ captureLayer Ports.pointerSetCaptureById swapIndex event model.canvas.layers
                    , captureLayer Ports.pointerReleaseCaptureById index event model.canvas.layers
                    ]
                )

            else
                ( model, Cmd.none )

        DownloadCanvas imageDownloadData ->
            let
                canvas =
                    model.canvas

                optimizedTree =
                    canvas
                        |> Canvas.mergeVisibleLayers
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
                        |> Quadtree.fromList (image |> Image.dimensions |> .width)

                task =
                    File.toBytes file |> Task.map (\x -> Image.decode x |> Maybe.map (Image.Advanced.eval >> image2quadtree))
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

        MsgBatch msgs ->
            ( model, msgs |> List.map cmd |> Cmd.batch )

        RunCmd cmd ->
            ( model, cmd )

        WithCmd withMsg cmd ->
            let
                ( newModel, newCmd ) =
                    update withMsg model
            in
            ( newModel, Cmd.batch [ newCmd, cmd ] )

        Test ->
            let
                _ =
                    "test"
            in
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewDebug data =
    div [ style "margin" (px config.defaultMargin) ] [ text (Debug.toString data) ]


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
        , Pointer.onEnter (CanvasPointerInside True)
        , Pointer.onLeave (CanvasPointerInside False)
        , Pointer.onOver (CanvasPointerInside True)
        , Pointer.onOut (CanvasPointerInside False)
        , Pointer.onDown (CanvasPointerPressed True)
        , Pointer.onUp (CanvasPointerPressed False)
        , Pointer.onCancel (CanvasPointerPressed False)
        , Pointer.onMove CanvasPointerMoved
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
        , canvas
            |> Canvas.mergeVisibleLayers
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
        [ style "background-color" (Color.toCssString model.color)
        , style "margin" (px config.defaultMargin)
        , style "width" (px 64)
        , style "height" (px 64)
        ]
        [ input
            [ style "width" "100%"
            , style "height" "100%"
            , style "opacity" "0%"
            , type_ "color"
            , value (Color.Convert.colorToHex model.color)
            , onInput (Color.Convert.hexToColor >> Result.Extra.unwrap model.color identity >> ChangeColor)
            ]
            []
        ]


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


viewLayer model i layer =
    let
        selectedBorder =
            2
    in
    div
        [ style "display" "flex"
        ]
        [ div
            [ style "width" (px config.layerPreviewSize)
            , style "height" (px config.layerPreviewSize)
            , onClick (ChangeSelectedLayer i)
            , style "box-sizing" "border-box"
            , if model.canvas.selectedLayerIndex == i then
                style "border" (px selectedBorder ++ " solid #D58F17")

              else
                style "border" "none"
            ]
            [ layer.data
                |> viewQuadtree
                    (if model.canvas.selectedLayerIndex == i then
                        toFloat config.layerPreviewSize - (selectedBorder * 2)

                     else
                        toFloat config.layerPreviewSize
                    )
                    config.color.background
            ]
        , div
            [ style "overflow" "auto"
            , style "width" (config.layerPreviewSize * 1.5 |> px)
            , style "display" "flex"
            , style "align-items" "center"
            , style "box-sizing" "border-box"
            , style "padding-left" "6px"
            , onDoubleClick (LayerRenamePointerPressed i)
            , Pointer.onLeave (always LayerRenameCanceled)

            --, Pointer.onOut (always LayerRenameCanceled)
            , Pointer.onDown (LayerHoldPointerPressed i True)
            , Pointer.onUp (LayerHoldPointerPressed i False)
            , Pointer.onCancel
                (\event ->
                    MsgBatch
                        [ LayerHoldPointerPressed i False event
                        , LayerRenameCanceled
                        ]
                )

            --, Pointer.onCancel (LayerHoldPointerPressed i False)
            , Pointer.onMove (LayerHoldPointerMoved i)
            , id (canvasLayerElementId layer)
            ]
            [ model.maybeRenameLayerIndex
                |> Maybe.Extra.filter ((==) i)
                |> Maybe.Extra.unwrap (text layer.name)
                    (always
                        (input
                            [ value layer.name

                            --, placeholder layer.name
                            , onInput (LayerRename i)

                            --, style "padding" "0"
                            ]
                            []
                        )
                    )
            ]
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
        , button [ onClick AddCompositeLayer ] [ text "add composite" ]
        , div
            [ style "display" "flex"
            , style "flex-wrap" "wrap"
            , style "flex-direction" "column-reverse"
            , style "border" "1px solid #000"
            ]
            (model.canvas.layers |> List.indexedMap (viewLayer model))
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

        --, viewDebug model.maybeRenameLayerTimer
        ]
