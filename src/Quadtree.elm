module Quadtree exposing (Quadrant, Quadrants, Quadtree(..), calculateMaxDepth, calculateMaxSize, coord2quadrant, depth2quadrantSize, depth2size, fitToDepth, fitToMaxDepth, getQuadrant, getQuadrantId, insertAtCoord, merge, optimize, quadnode, repeatQuadtree, scale, scaleOnce, toCoordDict, toListWithDefault, toSvgString, viewQuadtree)

import Array exposing (Array)
import Array.Extra
import Color exposing (Color)
import Common exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import List.Extra
import Maybe
import Maybe.Extra
import Svg exposing (g, rect, svg)
import Svg.Attributes exposing (fill, height, shapeRendering, width, x, y)
import Svg.String
import Svg.String.Attributes


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


mapQuadrants : (Quadrant -> Quadtree a -> b) -> Quadrants a -> Array b
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


depth2size : Int -> Int
depth2size depth =
    2 ^ depth


depth2quadrantSize : Int -> Int
depth2quadrantSize depth =
    (2 ^ depth) // 2


coord2quadrant : Point -> Int -> Quadrant
coord2quadrant { x, y } quadrantSize =
    if x < quadrantSize && y < quadrantSize then
        TopLeft

    else if x >= quadrantSize && y < quadrantSize then
        TopRight

    else if x < quadrantSize && y >= quadrantSize then
        BottomLeft

    else
        BottomRight


{-| NOTE: inserts exactly at the coord and depth needed
-}
insertAtCoord : Quadtree a -> Point -> Int -> Quadtree a -> Quadtree a
insertAtCoord insertTree ({ x, y } as coord) depth tree =
    if depth <= 0 then
        insertTree

    else
        let
            quadrantSize =
                depth2quadrantSize depth

            newDepth =
                depth - 1

            newCoord =
                { x = x |> remainderBy quadrantSize, y = y |> remainderBy quadrantSize }

            quadrant =
                coord2quadrant coord quadrantSize
        in
        case tree of
            QuadNode quadrants ->
                let
                    nodeQuadrant =
                        quadrants |> getQuadrant quadrant

                    newQuadrant =
                        insertAtCoord insertTree newCoord newDepth nodeQuadrant
                in
                quadrants
                    |> setQuadrant quadrant newQuadrant
                    |> QuadNode

            _ ->
                let
                    newQuadrant =
                        insertAtCoord insertTree newCoord newDepth tree
                in
                tree
                    |> repeatQuadtree
                    |> setQuadrant quadrant newQuadrant
                    |> QuadNode


calculateMaxDepth0 : Int -> Quadtree a -> Int
calculateMaxDepth0 depth tree =
    case tree of
        QuadNode quadrants ->
            quadrants
                |> Array.map (\quadrant -> calculateMaxDepth0 (depth + 1) quadrant)
                |> Array.foldl max 0

        _ ->
            depth


calculateMaxDepth : Quadtree a -> Int
calculateMaxDepth tree =
    calculateMaxDepth0 0 tree


calculateMaxSize : Quadtree a -> Int
calculateMaxSize tree =
    tree |> calculateMaxDepth |> depth2size


maybeReturnFirstData : Quadtree a -> Maybe a
maybeReturnFirstData tree =
    case tree of
        QuadEmpty ->
            Nothing

        QuadLeaf data ->
            Just data

        QuadNode quadrants ->
            quadrants
                |> Array.map maybeReturnFirstData
                |> Array.foldl Maybe.Extra.or Nothing


optimize : Quadtree a -> Quadtree a
optimize tree =
    case tree of
        QuadNode quadrants ->
            let
                maybeTopLeft =
                    Array.get 0 quadrants
            in
            if quadrants |> Array.Extra.all (\q -> Just q == maybeTopLeft) then
                maybeTopLeft
                    |> Maybe.Extra.unwrap QuadEmpty
                        (\topLeft ->
                            case topLeft of
                                QuadNode topLeftQuadrants ->
                                    topLeftQuadrants
                                        |> Array.map optimize
                                        |> QuadNode

                                _ ->
                                    topLeft
                        )

            else
                quadrants
                    |> Array.map optimize
                    |> QuadNode

        _ ->
            tree


scaleOnce : Quadtree a -> Quadtree a
scaleOnce tree =
    case tree of
        QuadNode quadrants ->
            quadrants |> Array.map scaleOnce |> QuadNode

        _ ->
            tree |> repeatQuadtree |> QuadNode


{-| NOTE: 1 -> x2, 2 -> x4, 3 -> x8
TODO: do not use it for building svg images for download
-}
scale : Int -> Quadtree a -> Quadtree a
scale times tree =
    if times <= 0 then
        tree

    else
        tree |> scaleOnce |> scale (times - 1)


{-| NOTE: adds nodes until all nodes have the same depth
-}
fitToDepth : Int -> Quadtree a -> Quadtree a
fitToDepth depth tree =
    if depth <= 0 then
        tree

    else
        (case tree of
            QuadNode quadrants ->
                quadrants

            _ ->
                tree
                    |> repeatQuadtree
        )
            |> Array.map (fitToDepth (depth - 1))
            |> QuadNode


fitToMaxDepth : Quadtree a -> Quadtree a
fitToMaxDepth tree =
    tree |> fitToDepth (tree |> calculateMaxDepth)


toCoordDict0 : ( Int, Int ) -> Int -> Quadtree a -> Dict ( Int, Int ) (Maybe a)
toCoordDict0 ( x, y ) size tree =
    case tree of
        QuadLeaf data ->
            Dict.singleton ( x, y ) (Just data)

        QuadEmpty ->
            Dict.singleton ( x, y ) Nothing

        QuadNode quadrants ->
            let
                quadrantSize =
                    size // 2
            in
            quadrants
                |> mapQuadrants
                    (\pos quadrant ->
                        let
                            coord =
                                case pos of
                                    TopLeft ->
                                        ( x, y )

                                    TopRight ->
                                        ( x + quadrantSize, y )

                                    BottomLeft ->
                                        ( x, y + quadrantSize )

                                    BottomRight ->
                                        ( x + quadrantSize, y + quadrantSize )
                        in
                        quadrant |> toCoordDict0 coord quadrantSize
                    )
                |> Array.foldl (\dict acc -> Dict.union dict acc) Dict.empty


toCoordDict : Int -> Quadtree a -> Dict ( Int, Int ) (Maybe a)
toCoordDict maxSize tree =
    tree |> toCoordDict0 ( 0, 0 ) maxSize


toListWithDefault : a -> Quadtree a -> ( Int, List a )
toListWithDefault default tree =
    let
        maxDepth =
            tree |> calculateMaxDepth

        maxSize =
            depth2size maxDepth
    in
    tree
        |> fitToDepth maxDepth
        |> toCoordDict maxSize
        |> Dict.toList
        |> List.sortBy (\( ( x, y ), _ ) -> ( y, x ))
        |> List.map (\( _, mv ) -> mv |> Maybe.withDefault default)
        |> (\x -> ( maxSize, x ))


viewQuadLeaf0 offsetX offsetY quadrantSize color =
    let
        sizeStr =
            String.fromFloat quadrantSize
    in
    rect
        [ fill (Color.toCssString color)
        , width sizeStr
        , height sizeStr
        , x (String.fromFloat offsetX)
        , y (String.fromFloat offsetY)
        , shapeRendering "crispEdges"
        ]
        []


viewQuadtree0 offsetX offsetY size emptyColor tree =
    case tree of
        QuadEmpty ->
            emptyColor |> viewQuadLeaf0 offsetX offsetY size

        QuadLeaf color ->
            color |> viewQuadLeaf0 offsetX offsetY size

        QuadNode quadrants ->
            let
                quadrantSize =
                    size / 2
            in
            g
                []
                [ viewQuadtree0 offsetX offsetY quadrantSize emptyColor (getQuadrant TopLeft quadrants)
                , viewQuadtree0 (offsetX + quadrantSize) offsetY quadrantSize emptyColor (getQuadrant TopRight quadrants)
                , viewQuadtree0 offsetX (offsetY + quadrantSize) quadrantSize emptyColor (getQuadrant BottomLeft quadrants)
                , viewQuadtree0 (offsetX + quadrantSize) (offsetY + quadrantSize) quadrantSize emptyColor (getQuadrant BottomRight quadrants)
                ]


viewQuadtree maxSize emptyColor tree =
    div
        [ style "position" "relative"
        , style "width" (px maxSize)
        , style "height" (px maxSize)
        ]
        [ svg
            [ width "100%"
            , height "100%"
            ]
            [ viewQuadtree0 0 0 maxSize emptyColor tree ]
        ]


toSvgStringLeaf0 offsetX offsetY quadrantSize color =
    let
        sizeStr =
            String.fromFloat quadrantSize
    in
    Svg.String.rect
        [ Svg.String.Attributes.fill (Color.toCssString color)
        , Svg.String.Attributes.width sizeStr
        , Svg.String.Attributes.height sizeStr
        , Svg.String.Attributes.x (String.fromFloat offsetX)
        , Svg.String.Attributes.y (String.fromFloat offsetY)
        ]
        []


toSvgString0 offsetX offsetY size emptyColor tree =
    case tree of
        QuadEmpty ->
            emptyColor |> toSvgStringLeaf0 offsetX offsetY size

        QuadLeaf color ->
            color |> toSvgStringLeaf0 offsetX offsetY size

        QuadNode quadrants ->
            let
                quadrantSize =
                    size / 2
            in
            Svg.String.g
                []
                [ toSvgString0 offsetX offsetY quadrantSize emptyColor (getQuadrant TopLeft quadrants)
                , toSvgString0 (offsetX + quadrantSize) offsetY quadrantSize emptyColor (getQuadrant TopRight quadrants)
                , toSvgString0 offsetX (offsetY + quadrantSize) quadrantSize emptyColor (getQuadrant BottomLeft quadrants)
                , toSvgString0 (offsetX + quadrantSize) (offsetY + quadrantSize) quadrantSize emptyColor (getQuadrant BottomRight quadrants)
                ]


toSvgString maxSize emptyColor tree =
    Svg.String.svg
        [ Svg.String.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg"
        , Svg.String.Attributes.width (String.fromFloat maxSize)
        , Svg.String.Attributes.height (String.fromFloat maxSize)
        ]
        [ toSvgString0 0 0 maxSize emptyColor tree ]
        |> Svg.String.toString 0
