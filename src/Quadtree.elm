module Quadtree exposing (Quadtree(..), viewQuadtree)

import Browser
import Html exposing (Html, div, table, tbody, td, text, tr)
import Html.Attributes exposing (style)


type Quadtree a
    = QuadLeaf a
    | QuadNode { tl : Quadtree a, tr : Quadtree a, bl : Quadtree a, br : Quadtree a }
    | QuadEmpty


viewQuadleaf color n maxSize =
    let
        sizeStr =
            String.fromFloat (maxSize / (2 ^ n)) ++ "px"
    in
    div
        [ style "width" sizeStr
        , style "height" sizeStr
        , style "background-color" color
        ]
        [ text "\u{200B}" ]


viewQuadtree0 x maxSize n =
    case x of
        QuadEmpty ->
            viewQuadleaf "#FFF" n maxSize

        QuadLeaf color ->
            viewQuadleaf color n maxSize

        QuadNode node ->
            table
                [ style "border-collapse" "collapse" ]
                [ tbody []
                    [ tr []
                        [ td [ style "padding" "0" ] [ viewQuadtree0 node.tl maxSize (n + 1) ]
                        , td [ style "padding" "0" ] [ viewQuadtree0 node.tr maxSize (n + 1) ]
                        ]
                    , tr []
                        [ td [ style "padding" "0" ] [ viewQuadtree0 node.bl maxSize (n + 1) ]
                        , td [ style "padding" "0" ] [ viewQuadtree0 node.br maxSize (n + 1) ]
                        ]
                    ]
                ]


viewQuadtree x maxSize =
    viewQuadtree0 x maxSize 0
