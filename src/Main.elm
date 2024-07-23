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


viewPixel color size =
    div
        [ style "width" "100%"
        , style "height" "100%"
        , style "background-color" color
        ]
        [ text "\u{200B}" ]


viewQuadtreePixel color size =
    td [ style "padding" "0" ]
        [ viewPixel color size ]



-- viewQuadtree x =
--     case x of
--         QuadEmpty ->
--             viewPixel "#FFF"
--         QuadLeaf color ->
--             viewPixel color
--         QuadNode node ->
--             table
--                 [ style "border-collapse" "collapse" ]
--                 [ tbody []
--                     [ tr [] [ viewQuadtree node.tl, viewQuadtree node.tr ]
--                     , tr [] [ viewQuadtree node.bl, viewQuadtree node.br ]
--                     ]
--                 ]


viewQuadtree0 x n =
    let
        size =
            1 / n
    in
    case x of
        QuadEmpty ->
            viewPixel "#FFF" size

        QuadLeaf color ->
            viewPixel color size

        QuadNode node ->
            table
                [ style "border-collapse" "collapse" ]
                [ tbody []
                    [ tr []
                        [ td [ style "padding" "0" ] [ viewQuadtree node.tl ]
                        , td [ style "padding" "0" ] [ viewQuadtree node.tr ]
                        ]
                    , tr []
                        [ td [ style "padding" "0" ] [ viewQuadtree node.bl ]
                        , td [ style "padding" "0" ] [ viewQuadtree node.br ]
                        ]
                    ]
                ]


viewQuadtree x =
    div [ style "width" "100px", style "height" "100px" ] [ viewQuadtree0 x 1 ]


quad0 =
    QuadNode
        { tl = QuadLeaf "#F66"
        , tr = QuadLeaf "#FF6"
        , bl = QuadLeaf "#6F6"
        , br =
            QuadNode
                { tl = QuadLeaf "#6FF"
                , tr = QuadLeaf "#66F"
                , bl = QuadLeaf "#F6F"
                , br = QuadEmpty
                }
        }


test =
    quad0


main =
    div []
        [ viewQuadtree test
        , text (Debug.toString test)
        ]
