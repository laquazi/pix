module Main exposing (..)

import Browser
import Html exposing (Html, button, div, table, tbody, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import ShapedArray exposing (..)


type Quadtree a
    = QuadLeaf a
    | QuadNode { tl : Quadtree a, tr : Quadtree a, bl : Quadtree a, br : Quadtree a }
    | QuadEmpty



-- TODO: scale, repr


viewPixel color =
    div
        [ style "width" "10px"
        , style "height" "10px"
        , style "background-color" color
        ]
        [ text "\u{200B}" ]


viewQuadtreePixel color =
    td [ style "padding" "0" ]
        [ viewPixel color ]


viewQuadtreePart x =
    case x of
        QuadEmpty ->
            [ tr [] [ viewQuadtreePixel "#FFF" ] ]

        QuadLeaf color ->
            [ tr [] [ viewQuadtreePixel color ] ]

        QuadNode colors ->
            [ tr [] [ viewQuadtreePixel colors.tl, viewQuadtreePixel colors.tr ]
            , tr [] [ viewQuadtreePixel colors.bl, viewQuadtreePixel colors.br ]
            ]


viewQuadtree x =
    table
        [ style "border-collapse" "collapse" ]
        [ tbody [] (viewQuadtreePart x) ]


quad0 =
    QuadNode { tl = QuadLeaf 1, tr = QuadLeaf 2, bl = QuadLeaf 3, br = QuadNode { tl = QuadLeaf 1, tr = QuadLeaf 2, bl = QuadLeaf 3, br = QuadEmpty } }


test =
    quad0


main =
    div []
        [ viewQuadtree test
        , text (Debug.toString test)
        ]
