module Canvas.Layers exposing (..)

import Color exposing (Color)
import Common exposing (colorBlendingNormal)
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)
import Quadtree exposing (Quadtree(..))
import SelectArray exposing (SelectArray(..))


type alias Layer =
    { data : Quadtree Color
    , blendingMode : Maybe (Color -> Color -> Color)
    , name : String
    , isVisible : Bool
    }


type alias Layers =
    SelectArray Layer


merge : Layers -> Quadtree Color
merge layers =
    layers
        |> SelectArray.foldr
            (\layer ( accTree, accBlend ) ->
                ( Quadtree.merge accBlend accTree layer.data
                , layer.blendingMode |> Maybe.withDefault colorBlendingNormal
                )
            )
            ( QuadEmpty, colorBlendingNormal )
        |> Tuple.first


mergeVisible : Layers -> Quadtree Color
mergeVisible layers =
    layers
        |> SelectArray.filter .isVisible
        |> merge


emptyLayer : Layer
emptyLayer =
    { data = QuadEmpty
    , blendingMode = Nothing
    , name = ""
    , isVisible = True
    }


default : Layers
default =
    [ { emptyLayer | name = "Background", data = QuadLeaf Color.white } ]
        |> SelectArray.fromList
        |> SelectArray.setSelection 0
        |> addEmpty


parserLayerName : String -> Parser Int
parserLayerName prefix =
    Parser.succeed identity
        |. Parser.keyword prefix
        |. Parser.spaces
        |= Parser.int


addWithPrefix : String -> Quadtree Color -> Layers -> Layers
addWithPrefix prefix data layers =
    let
        nameIndex =
            layers
                |> SelectArray.filterMap
                    (\layer -> Parser.run (parserLayerName prefix) layer.name |> Result.toMaybe)
                |> SelectArray.maximum
                |> Maybe.withDefault 0

        newLayer =
            { emptyLayer
                | name = prefix ++ " " ++ String.fromInt (nameIndex + 1)
                , data = data
            }

        newSelection =
            layers
                |> SelectArray.getSelection
                |> Maybe.Extra.unwrap (SelectArray.length layers) ((+) 1)
    in
    layers
        |> SelectArray.insertAt newSelection newLayer
        |> SelectArray.setSelection newSelection


addLayer : Quadtree Color -> Layers -> Layers
addLayer =
    addWithPrefix "Layer"


addComposite : Layers -> Layers
addComposite layers =
    layers |> addWithPrefix "Composite layer" (layers |> mergeVisible)


addEmpty : Layers -> Layers
addEmpty =
    addLayer emptyLayer.data
