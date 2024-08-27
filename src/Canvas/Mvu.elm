module Canvas.Mvu exposing (..)

import Canvas.Layers as Layers exposing (..)
import Color exposing (Color)
import Color.Convert
import Common exposing (..)
import Config exposing (config)
import Debug exposing (log)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (id, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Touch as Touch
import Image exposing (Image)
import Image.Advanced
import Image.Color
import Ports
import Quadtree exposing (..)
import Result.Extra
import SelectArray
import Set exposing (Set)
import Svg exposing (defs, pattern, rect, svg)
import Svg.Attributes exposing (fill, height, patternUnits, shapeRendering, stroke, strokeWidth, width)
import Task
import Time



-- FIXME: long touch does not work
-- FIXME: rename layer ui does not allow actual input
-- TODO: split this file into several
-- FIXME: drawing on mobile is now broken, probably because of the new pointer capture
-- TODO: make naming more consistent
-- TODO: add separate colorpalette that contains all colors already used on all layers (and probably a button to hide those from hidden layers)
-- TODO: add region selection, copy and paste
-- TODO: add a `pattern area` aka latest copied regions
-- TODO: add more `canvas layouts`. quadtree is one of them, add nontree(?)(9), add hex


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


type LayerRenameData
    = LayerRenameReady Int
    | LayerRenameDelay Int Time.Posix
    | LayerRenameCancel


coordVisual2Data : Point -> Int -> Int -> Point
coordVisual2Data { x, y } scale size =
    { x = (x * (2 ^ scale)) // size
    , y = (y * (2 ^ scale)) // size
    }


canvasLayerElementId layer =
    "layer:" ++ layer.name


doAtCoord : Point -> (Point -> Int -> Quadtree Color -> Quadtree Color) -> Model -> Layers
doAtCoord visualCoord f model =
    model.layers
        |> SelectArray.updateSelected
            (\layer ->
                { layer
                    | data =
                        f (coordVisual2Data visualCoord model.scale model.size) model.scale layer.data
                }
            )


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
        |> SelectArray.getAt index
        |> Maybe.map
            (\layer ->
                Ports.encodeCapturePointerById event.pointerId (canvasLayerElementId layer)
                    |> capture
            )
        |> Maybe.withDefault Cmd.none


layerRenameSetDelay index =
    Time.now |> Task.perform (LayerRenameSetDelay index)


layerRenameResolved index =
    Time.now |> Task.perform (LayerRenameResolved index)


drawLine : Point -> Point -> Model -> Model
drawLine a b model =
    let
        newLayers =
            model.layers
                |> SelectArray.updateSelected
                    (\layer ->
                        let
                            tree =
                                case model.tool of
                                    Pencil ->
                                        QuadLeaf model.color

                                    Eraser ->
                                        QuadEmpty
                        in
                        { layer | data = layer.data |> Quadtree.insertLine tree a b model.scale }
                    )
    in
    { model | layers = newLayers }


drawLineTest : (Point -> Point -> List Point) -> Point -> Point -> Model -> Model
drawLineTest lineMethod a b model =
    let
        newLayers =
            model.layers
                |> Layers.addLayer QuadEmpty
                |> SelectArray.updateSelected
                    (\layer ->
                        let
                            tree =
                                case model.tool of
                                    Pencil ->
                                        QuadLeaf model.color

                                    Eraser ->
                                        QuadEmpty
                        in
                        { layer | data = layer.data |> Quadtree.insertLineUsing lineMethod tree a b model.scale }
                    )
    in
    { model | layers = newLayers }



-- MODEL


type alias Model =
    { layers : Layers
    , scale : Int
    , isRulerVisible : Bool
    , size : Int
    , color : Color
    , tool : Tool
    , colorpalette : List Color
    , canvasPointer : PointerData
    , holdingLayerIndices : Set Int
    , layerRenameData : LayerRenameData
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { layers = Layers.default
      , scale = 3
      , isRulerVisible = True
      , size = 512
      , color = Color.black
      , tool = Pencil
      , colorpalette = Config.defaultColorpalette
      , canvasPointer = { isInside = False, isDown = False }
      , holdingLayerIndices = Set.empty
      , layerRenameData = LayerRenameCancel
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
    | LayerRenameSetReady Int
    | LayerRenameSetDelay Int Time.Posix
    | LayerRenameResolved Int Time.Posix
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
                    , Ports.encodeCapturePointerById event.pointerId "canvasContainer"
                        |> Ports.pointerSetCaptureById
                    ]
                )

            else
                ( { model | canvasPointer = newCanvasPointer }
                , Ports.encodeCapturePointerById event.pointerId "canvasContainer"
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
                    newLayers =
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
                ( { model | layers = newLayers }, Cmd.none )

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
                newLayers =
                    model.layers
                        |> SelectArray.updateSelected (\layer -> { layer | data = QuadEmpty })
            in
            ( { model | layers = newLayers }, Cmd.none )

        Noop ->
            ( model, Cmd.none )

        ChangeSelectedLayer index ->
            let
                newLayers =
                    model.layers
                        |> SelectArray.setSelection index
            in
            ( { model | layers = newLayers }, Cmd.none )

        ToggleLayerVisibility index ->
            let
                newLayers =
                    model.layers
                        |> SelectArray.updateAt index (\layer -> { layer | isVisible = not layer.isVisible })
            in
            ( { model | layers = newLayers }, Cmd.none )

        AddNewLayer ->
            let
                newLayers =
                    model.layers
                        |> Layers.addEmpty
            in
            ( { model | layers = newLayers }, Cmd.none )

        AddCompositeLayer ->
            let
                newLayers =
                    model.layers
                        |> Layers.addComposite
            in
            ( { model | layers = newLayers }, Cmd.none )

        RemoveSelectedLayer ->
            ( { model | layers = Layers.removeSelected model.layers }, Cmd.none )

        LayerRenameSetReady index ->
            ( { model | layerRenameData = LayerRenameReady index }, Cmd.none )

        LayerRenameSetDelay index currentTime ->
            ( { model | layerRenameData = LayerRenameDelay index currentTime }, Cmd.none )

        LayerRenameResolved index currentTime ->
            let
                newLayerRenameData =
                    case model.layerRenameData of
                        LayerRenameDelay i t ->
                            if i == index && (currentTime |> Time.posixToMillis) - (t |> Time.posixToMillis) < 300 then
                                LayerRenameReady index

                            else
                                LayerRenameCancel

                        _ ->
                            LayerRenameCancel
            in
            ( { model | layerRenameData = newLayerRenameData }, Cmd.none )

        LayerRenameCanceled ->
            ( { model | layerRenameData = LayerRenameCancel }, Cmd.none )

        LayerRename index name ->
            let
                canvas =
                    model.layers

                newLayers =
                    canvas
                        |> SelectArray.updateAt index (\layer -> { layer | name = name })
            in
            ( { model | layers = newLayers }, Cmd.none )

        LayerHoldPointerPressed index isDown event ->
            let
                holdingLayerIndices =
                    model.holdingLayerIndices

                ( newHoldingLayerIndices, newCmd ) =
                    if isDown then
                        ( holdingLayerIndices |> Set.insert index
                        , captureLayer Ports.pointerSetCaptureById index event model.layers
                        )

                    else
                        ( holdingLayerIndices |> Set.remove index
                        , captureLayer Ports.pointerReleaseCaptureById index event model.layers
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
                    index + indexOffset |> clamp 0 (SelectArray.length model.layers - 1)
            in
            if isHolding && not isHeightBounded && swapIndex /= index then
                let
                    newLayers0 =
                        model.layers
                            |> SelectArray.swapAt index swapIndex

                    newLayers =
                        if model.layers |> SelectArray.isSelection index then
                            newLayers0 |> SelectArray.setSelection swapIndex

                        else if model.layers |> SelectArray.isSelection swapIndex then
                            newLayers0 |> SelectArray.setSelection index

                        else
                            newLayers0

                    newHoldingLayerIndices =
                        model.holdingLayerIndices |> Set.insert swapIndex |> Set.remove index
                in
                ( { model | layers = newLayers, holdingLayerIndices = newHoldingLayerIndices }
                , Cmd.batch
                    [ captureLayer Ports.pointerSetCaptureById swapIndex event model.layers
                    , captureLayer Ports.pointerReleaseCaptureById index event model.layers
                    ]
                )

            else
                ( model, Cmd.none )

        DownloadCanvas imageDownloadData ->
            let
                canvas =
                    model.layers

                optimizedTree =
                    canvas
                        |> Layers.mergeVisible
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
                newLayers =
                    maybeQuadtree
                        |> Maybe.map
                            (\quadtree -> model.layers |> Layers.addLayer quadtree)
                        |> Maybe.withDefault model.layers
            in
            ( { model | layers = newLayers }, Cmd.none )

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



-- VIEW


viewRuler scale maxSize isVisible =
    let
        halfThickness =
            1

        sizeStr =
            String.fromFloat (maxSize / (2 ^ scale))
    in
    div
        [ style "position" "absolute"
        , style "z-index" (String.fromFloat config.zIndex.canvasRuler)
        , style "width" (px maxSize)
        , style "height" (px maxSize)

        --, style "touch-action" "none"
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


viewCanvasContainer model =
    let
        canvas =
            model.layers
    in
    div
        [ style "position" "relative"
        , style "margin" (px config.defaultMargin)
        , Pointer.onEnter (CanvasPointerInside True)
        , Pointer.onLeave (CanvasPointerInside False)
        , Pointer.onOver (CanvasPointerInside True)
        , Pointer.onOut (CanvasPointerInside False)
        , Pointer.onDown (CanvasPointerPressed True)
        , Pointer.onUp (CanvasPointerPressed False)

        --, Pointer.onCancel (CanvasPointerPressed False)
        , Pointer.onMove CanvasPointerMoved
        , id "canvasContainer"

        --, style "touch-action" "none"
        --, style "z-index" (String.fromFloat config.zIndex.canvasContainer)
        ]
        [ viewRuler (toFloat model.scale) (toFloat model.size) model.isRulerVisible
        , canvas
            |> Layers.mergeVisible
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


viewLayer : Model -> Int -> Layer -> Html Msg
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
            , if model.layers |> SelectArray.isSelection i then
                style "border" (px selectedBorder ++ " solid #D58F17")

              else
                style "border" "none"
            ]
            [ layer.data
                |> viewQuadtree
                    (if model.layers |> SelectArray.isSelection i then
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
            , Mouse.onDoubleClick (always (LayerRenameSetReady i))
            , Touch.onStart (always (layerRenameSetDelay i |> RunCmd))
            , Touch.onEnd (always (layerRenameResolved i |> RunCmd))
            , Pointer.onLeave (always LayerRenameCanceled)
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
            [ case model.layerRenameData of
                LayerRenameReady i0 ->
                    if i == i0 then
                        input
                            [ value layer.name
                            , onInput (LayerRename i)
                            ]
                            []

                    else
                        text layer.name

                _ ->
                    text layer.name
            ]
        , button [ onClick (ToggleLayerVisibility i) ]
            [ if layer.isVisible then
                text "👁"

              else
                text "‿"
            ]
        ]


viewLayers : Model -> Html Msg
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
            (model.layers |> SelectArray.toList |> List.indexedMap (viewLayer model))
        ]
