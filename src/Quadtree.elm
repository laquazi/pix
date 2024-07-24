module Quadtree exposing (Quadrant, Quadrants, Quadtree(..), coord2quadrant, getQuadrant, getQuadrantId, insertAtCoord, quadnode, repeatQuadtree, scale2halfMaxCoord, viewQuadtree)

import Array exposing (Array)
import Array.Extra
import Browser
import Color exposing (Color)
import Common exposing (..)
import Debug exposing (log)
import Html exposing (Html, div, table, tbody, td, text, tr)
import Html.Attributes exposing (style)


type alias Quadrants a =
    Array (Quadtree a)


type Quadtree a
    = QuadLeaf a
    | QuadNode (Quadrants a)
    | QuadEmpty


type Quadrant
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight


quadnode tl tr bl br =
    [ tl, tr, bl, br ] |> Array.fromList |> QuadNode


repeatQuadtree : Quadtree a -> Quadrants a
repeatQuadtree data =
    Array.repeat 4 data


getQuadrantId : Quadrant -> Int
getQuadrantId quadrant =
    case quadrant of
        TopLeft ->
            0

        TopRight ->
            1

        BottomLeft ->
            2

        BottomRight ->
            3


getQuadrant : Quadrant -> Quadrants a -> Quadtree a
getQuadrant quadrant quadrants =
    quadrants
        |> Array.get (getQuadrantId quadrant)
        |> Maybe.withDefault QuadEmpty



-- setQuadrant : Quadrant -> Quadtree a -> Quadrants a -> Quadtree a
-- setQuadrant quadrant quadtree quadrants =
--     quadrants
--         |> Array.set (getQuadrantId quadrant) quadtree


setQuadrant : Quadrant -> Quadtree a -> Quadrants a -> Quadrants a
setQuadrant quadrant quadtree quadrants =
    quadrants
        |> Array.set (getQuadrantId quadrant) quadtree


scale2halfMaxCoord : Int -> Int
scale2halfMaxCoord scale =
    (2 ^ scale) // 2


coord2quadrant : Point -> Int -> Quadrant
coord2quadrant { x, y } halfMaxCoord =
    if x <= halfMaxCoord && y <= halfMaxCoord then
        TopLeft

    else if x > halfMaxCoord && y <= halfMaxCoord then
        TopRight

    else if x <= halfMaxCoord && y > halfMaxCoord then
        BottomLeft

    else
        BottomRight


insertAtCoord0 : a -> Point -> Int -> Int -> Quadtree a -> Quadtree a
insertAtCoord0 newData ({ x, y } as coord) scale maxScale tree =
    if scale <= 0 then
        QuadLeaf newData |> log "leaf"

    else
        let
            halfMaxCoord =
                scale2halfMaxCoord scale |> log "halfMaxCoord"

            mycoord =
                coord |> log "coord"

            newScale =
                scale - 1

            newCoord =
                { x = x |> remainderBy halfMaxCoord, y = y |> remainderBy halfMaxCoord } |> log "newCoord"

            quadrant =
                coord2quadrant coord halfMaxCoord
        in
        case tree |> log "tree" of
            QuadNode quadrants ->
                let
                    nodeQuadrant =
                        quadrants |> getQuadrant quadrant

                    newQuadrant =
                        insertAtCoord0 newData newCoord newScale maxScale nodeQuadrant
                in
                quadrants
                    |> setQuadrant quadrant newQuadrant
                    |> QuadNode
                    |> log "node"

            _ ->
                let
                    newQuadrant =
                        insertAtCoord0 newData newCoord newScale maxScale tree
                in
                tree
                    |> repeatQuadtree
                    |> setQuadrant quadrant newQuadrant
                    |> QuadNode
                    |> log "node"


insertAtCoord : a -> Point -> Int -> Quadtree a -> Quadtree a
insertAtCoord newData coord scale tree =
    insertAtCoord0 newData coord scale scale tree


viewQuadLeaf0 color n maxSize =
    let
        sizeStr =
            String.fromFloat (maxSize / (2 ^ n)) ++ "px"
    in
    div
        [ style "width" sizeStr
        , style "height" sizeStr
        , style "background-color" (Color.toCssString color)
        ]
        [ text "\u{200B}" ]


viewQuadtree0 x maxSize n =
    case x of
        QuadEmpty ->
            viewQuadLeaf0 Color.white n maxSize

        QuadLeaf color ->
            viewQuadLeaf0 color n maxSize

        QuadNode quadrants ->
            table
                [ style "border-collapse" "collapse" ]
                [ tbody []
                    [ tr []
                        [ td [ style "padding" "0" ] [ viewQuadtree0 (getQuadrant TopLeft quadrants) maxSize (n + 1) ]
                        , td [ style "padding" "0" ] [ viewQuadtree0 (getQuadrant TopRight quadrants) maxSize (n + 1) ]
                        ]
                    , tr []
                        [ td [ style "padding" "0" ] [ viewQuadtree0 (getQuadrant BottomLeft quadrants) maxSize (n + 1) ]
                        , td [ style "padding" "0" ] [ viewQuadtree0 (getQuadrant BottomRight quadrants) maxSize (n + 1) ]
                        ]
                    ]
                ]


viewQuadtree x maxSize =
    viewQuadtree0 x maxSize 0
