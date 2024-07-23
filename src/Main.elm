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



-- TODO: scale


viewPixel color n maxSize =
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
            viewPixel "#FFF" n maxSize

        QuadLeaf color ->
            viewPixel color n maxSize

        QuadNode node ->
            table
                [ style "border-collapse" "collapse"
                , style "z-index" "0"
                ]
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



-- scale 3 = 3x3


viewRuler scale maxSize =
    let
        n =
            2 ^ scale

        sizeStr =
            String.fromFloat (maxSize / n) ++ "px"
    in
    table
        [ style "border-collapse" "collapse"
        , style "border-right" "1px solid #333"
        , style "border-bottom" "1px solid #333"
        , style "z-index" "1"
        , style "box-sizing" "border-box"
        ]
        [ tbody []
            (List.repeat (round n)
                (tr []
                    (List.repeat (round n)
                        (td
                            [ style "padding" "0"
                            , style "border-left" "1px solid #333"
                            , style "border-top" "1px solid #333"
                            , style "box-sizing" "border-box"
                            ]
                            [ div
                                [ style "width" sizeStr
                                , style "height" sizeStr

                                --style "background-color" "transp"
                                ]
                                [ text "\u{200B}" ]
                            ]
                        )
                    )
                )
            )
        ]


main =
    div
        [ style "position" "absolute"
        , style "top" "10px"
        , style "left" "10px"

        -- , style "box-sizing" "border-box"
        -- , style "-moz-box-sizing" "border-box"
        -- , style "-webkit-box-sizing" "border-box"
        ]
        [ viewRuler 3 256
        , viewQuadtree quad0 256
        , text (Debug.toString quad0)
        ]
