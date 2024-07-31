module Canvas exposing (..)

import Color exposing (Color)
import Common exposing (colorBlendingNormal)
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Quadtree exposing (Quadtree(..))


type alias CanvasLayer =
    { data : Quadtree Color
    , blendingMode : Maybe (Color -> Color -> Color)
    , name : String
    , isVisible : Bool
    }


{-| NOTE: (from top to bottom)
-}
type alias Canvas =
    { layers : List CanvasLayer
    , selectedLayerIndex : Int
    }


mergeLayers : Canvas -> Quadtree Color
mergeLayers canvas =
    canvas.layers
        |> List.foldr
            (\layer ( accTree, accBlend ) ->
                ( Quadtree.merge accBlend accTree layer.data
                , layer.blendingMode |> Maybe.withDefault colorBlendingNormal
                )
            )
            ( QuadEmpty, colorBlendingNormal )
        |> Tuple.first


mergeVisibleLayers : Canvas -> Quadtree Color
mergeVisibleLayers canvas =
    { canvas | layers = canvas.layers |> List.filter .isVisible }
        |> mergeLayers


layerEmpty : CanvasLayer
layerEmpty =
    { data = QuadEmpty
    , blendingMode = Nothing
    , name = ""
    , isVisible = True
    }


canvasEmpty : Canvas
canvasEmpty =
    { layers = [ layerEmpty ]
    , selectedLayerIndex = 0
    }


parserLayerName : Parser Int
parserLayerName =
    Parser.succeed identity
        |. Parser.keyword "Layer"
        |. Parser.spaces
        |= Parser.int


addNewLayer : Canvas -> Canvas
addNewLayer canvas =
    let
        index =
            canvas.layers
                |> List.filterMap
                    (\layer -> Parser.run parserLayerName layer.name |> Result.toMaybe)
                |> List.maximum
                |> Maybe.withDefault 0

        newLayer =
            { layerEmpty | name = "Layer " ++ String.fromInt (index + 1) }
    in
    { canvas | layers = canvas.layers ++ [ newLayer ] }


removeLayer : Int -> Canvas -> Canvas
removeLayer index canvas =
    let
        newLayers =
            canvas.layers
                |> List.Extra.removeAt index

        newLayersLength =
            List.length newLayers
    in
    if index < newLayersLength then
        { canvas | layers = newLayers }

    else
        { canvas | layers = newLayers, selectedLayerIndex = newLayersLength - 1 }


updateLayer : Int -> (CanvasLayer -> CanvasLayer) -> Canvas -> Canvas
updateLayer index f canvas =
    let
        newLayers =
            canvas.layers
                |> List.Extra.updateAt index f
    in
    { canvas | layers = newLayers }


removeSelectedLayer : Canvas -> Canvas
removeSelectedLayer canvas =
    canvas |> removeLayer canvas.selectedLayerIndex


updateSelectedLayer : (CanvasLayer -> CanvasLayer) -> Canvas -> Canvas
updateSelectedLayer f canvas =
    canvas |> updateLayer canvas.selectedLayerIndex f
