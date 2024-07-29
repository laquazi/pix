module Quadtree exposing (Quadrant, Quadrants, Quadtree(..), coord2quadrant, getQuadrant, getQuadrantId, insertAtCoord, merge, quadnode, repeatQuadtree, scale2halfMaxCoord, viewQuadtree)

import Array exposing (Array)
import Array.Extra
import Color exposing (Color)
import Common exposing (..)
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


merge : (a -> a -> a) -> Quadtree a -> Quadtree a -> Quadtree a
merge f aTree bTree =
    case ( aTree, bTree ) of
        ( QuadEmpty, _ ) ->
            bTree

        ( _, QuadEmpty ) ->
            aTree

        ( QuadLeaf a, QuadLeaf b ) ->
            QuadLeaf (f a b)

        ( (QuadLeaf _) as a, _ ) ->
            merge f (QuadNode (repeatQuadtree a)) bTree

        ( _, (QuadLeaf _) as b ) ->
            merge f aTree (QuadNode (repeatQuadtree b))

        ( QuadNode a, QuadNode b ) ->
            Array.Extra.map2 (merge f) a b |> QuadNode


mapQuadrants : (Quadrant -> Quadtree a -> Quadtree b) -> Quadrants a -> Quadrants b
mapQuadrants f quadrants =
    Array.Extra.map2 f
        (Array.fromList
            [ TopLeft
            , TopRight
            , BottomLeft
            , BottomRight
            ]
        )
        quadrants


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


setQuadrant : Quadrant -> Quadtree a -> Quadrants a -> Quadrants a
setQuadrant quadrant quadtree quadrants =
    quadrants
        |> Array.set (getQuadrantId quadrant) quadtree


scale2halfMaxCoord : Int -> Int
scale2halfMaxCoord scale =
    (2 ^ scale) // 2


coord2quadrant : Point -> Int -> Quadrant
coord2quadrant { x, y } halfMaxCoord =
    if x < halfMaxCoord && y < halfMaxCoord then
        TopLeft

    else if x >= halfMaxCoord && y < halfMaxCoord then
        TopRight

    else if x < halfMaxCoord && y >= halfMaxCoord then
        BottomLeft

    else
        BottomRight


insertAtCoord : Quadtree a -> Point -> Int -> Quadtree a -> Quadtree a
insertAtCoord insertTree ({ x, y } as coord) scale tree =
    if scale <= 0 then
        insertTree

    else
        let
            halfMaxCoord =
                scale2halfMaxCoord scale

            newScale =
                scale - 1

            newCoord =
                { x = x |> remainderBy halfMaxCoord, y = y |> remainderBy halfMaxCoord }

            quadrant =
                coord2quadrant coord halfMaxCoord
        in
        case tree of
            QuadNode quadrants ->
                let
                    nodeQuadrant =
                        quadrants |> getQuadrant quadrant

                    newQuadrant =
                        insertAtCoord insertTree newCoord newScale nodeQuadrant
                in
                quadrants
                    |> setQuadrant quadrant newQuadrant
                    |> QuadNode

            _ ->
                let
                    newQuadrant =
                        insertAtCoord insertTree newCoord newScale tree
                in
                tree
                    |> repeatQuadtree
                    |> setQuadrant quadrant newQuadrant
                    |> QuadNode


viewQuadLeaf0 maxSize n color =
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


viewQuadtree0 maxSize n emptyColor x =
    case x of
        QuadEmpty ->
            emptyColor |> viewQuadLeaf0 maxSize n

        QuadLeaf color ->
            color |> viewQuadLeaf0 maxSize n

        QuadNode quadrants ->
            let
                viewTd q =
                    td [ style "padding" "0" ]
                        [ quadrants
                            |> getQuadrant q
                            |> viewQuadtree0 maxSize (n + 1) emptyColor
                        ]
            in
            table
                [ style "border-collapse" "collapse"
                , style "user-select" "none"
                ]
                [ tbody []
                    [ tr [] [ viewTd TopLeft, viewTd TopRight ]
                    , tr [] [ viewTd BottomLeft, viewTd BottomRight ]
                    ]
                ]


viewQuadtree maxSize emptyColor x =
    viewQuadtree0 maxSize 0 emptyColor x
