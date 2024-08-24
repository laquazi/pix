module Canvas.Mvu exposing (..)

import Common exposing (..)


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
    , layerRenameData : LayerRenameData
    }
