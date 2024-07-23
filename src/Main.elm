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


viewRuler scale maxSize =
    let
        n =
            2 ^ scale

        sizeStr =
            String.fromFloat (maxSize / n) ++ "px"
    in
    table
        [ style "border-collapse" "collapse"
        , style "position" "absolute"
        , style "z-index" "1"
        , style "outline" "0.25px solid #333"
        , style "outline-offset" "-0.25px"
        ]
        [ tbody []
            (List.repeat (round n)
                (tr []
                    (List.repeat (round n)
                        (td
                            [ style "padding" "0"
                            , style "outline" "0.125px solid #333"
                            , style "outline-offset" "-0.125px"
                            ]
                            [ div
                                [ style "width" sizeStr
                                , style "height" sizeStr
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
        ]
        [ viewRuler 4 512
        , viewQuadtree quad0 512
        , text (Debug.toString quad0)
        ]
