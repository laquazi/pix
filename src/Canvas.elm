module Canvas exposing (..)

import Color exposing (Color)
import Common exposing (colorBlendingNormal)
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
