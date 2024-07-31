module Quadtree exposing (Quadrant, Quadrants, Quadtree(..), calculateMaxDepth, calculateMaxSize, coord2quadrant, depth2halfSize, depth2size, fitToDepth, fitToMaxDepth, getQuadrant, getQuadrantId, insertAtCoord, merge, optimize, quadnode, repeatQuadtree, scale, scaleOnce, toCoordDict, toListWithDefault, viewQuadtree)

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


depth2halfSize : Int -> Int
depth2halfSize depth =
    (2 ^ depth) // 2


coord2quadrant : Point -> Int -> Quadrant
coord2quadrant { x, y } halfSize =
    if x < halfSize && y < halfSize then
        TopLeft

    else if x >= halfSize && y < halfSize then
        TopRight

    else if x < halfSize && y >= halfSize then
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
            halfSize =
                depth2halfSize depth

            newDepth =
                depth - 1

            newCoord =
                { x = x |> remainderBy halfSize, y = y |> remainderBy halfSize }

            quadrant =
                coord2quadrant coord halfSize
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


depth2size : Int -> Int
depth2size depth =
    2 ^ depth


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


{-| FIXME: optimizes not only data, but also patterns
-}
optimize : Quadtree a -> Quadtree a
optimize tree =
    case tree of
        QuadNode quadrants ->
            let
                optimizedQuadrants =
                    quadrants |> Array.map optimize
            in
            if optimizedQuadrants |> Array.Extra.all (\q -> Just q == Array.get 0 optimizedQuadrants) then
                -- unique quadrants
                Array.get 0 optimizedQuadrants
                    |> Maybe.andThen maybeReturnFirstData
                    |> Maybe.Extra.unwrap QuadEmpty QuadLeaf

            else
                -- distinct quadrants
                QuadNode optimizedQuadrants

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



--toCoordDict0 : ( Int, Int ) -> Float -> Float -> Quadtree a -> Dict ( Int, Int ) (Maybe a)
--toCoordDict0 ( x, y ) maxSize depth tree =
--    case tree of
--        QuadLeaf data ->
--            Dict.singleton ( x, y ) (Just data)
--
--        QuadEmpty ->
--            Dict.singleton ( x, y ) Nothing
--
--        QuadNode quadrants ->
--            let
--                o =
--                    maxSize / (2 ^ (depth + 1)) |> round
--            in
--            quadrants
--                |> Array.indexedMap
--                    (\i quadrant ->
--                        let
--                            coord =
--                                case i of
--                                    0 ->
--                                        ( x, y )
--
--                                    1 ->
--                                        ( x + o, y )
--
--                                    2 ->
--                                        ( x, y + o )
--
--                                    _ ->
--                                        ( x + o, y + o )
--                        in
--                        quadrant |> toCoordDict0 coord maxSize (depth - 1)
--                    )
--                |> Array.foldl (\dict acc -> Dict.union dict acc) Dict.empty
--
--toCoordDict1 :  Int -> Quadtree a -> Dict ( Int, Int ) (Maybe a)
--toCoordDict1 maxDepth tree =
--    tree |> toCoordDict0 ( 0, 0 ) (maxDepth |> depth2size |> toFloat) (toFloat maxDepth)


toCoordDict0 : ( Int, Int ) -> Int -> Quadtree a -> Dict ( Int, Int ) (Maybe a)
toCoordDict0 ( x, y ) offset tree =
    case tree of
        QuadLeaf data ->
            Dict.singleton ( x, y ) (Just data)

        QuadEmpty ->
            Dict.singleton ( x, y ) Nothing

        QuadNode quadrants ->
            quadrants
                |> Array.indexedMap
                    (\i quadrant ->
                        let
                            coord =
                                case i of
                                    0 ->
                                        ( x, y )

                                    1 ->
                                        ( x + offset, y )

                                    2 ->
                                        ( x, y + offset )

                                    _ ->
                                        ( x + offset, y + offset )
                        in
                        quadrant |> toCoordDict0 coord (offset // 2)
                    )
                |> Array.foldl (\dict acc -> Dict.union dict acc) Dict.empty


toCoordDict : Int -> Quadtree a -> Dict ( Int, Int ) (Maybe a)
toCoordDict maxSize tree =
    tree |> toCoordDict0 ( 0, 0 ) (maxSize // 2)



--toList2dWithDefault : a -> Quadtree a -> List (List a)
--toList2dWithDefault default tree =
--    tree
--        |> fitToMaxDepth
--        |> toCoordDict
--        |> Dict.foldr
--            (\( x, y ) mv ( maxYacc, acc ) ->
--                let
--                    v =
--                        mv |> Maybe.withDefault default
--
--                    maxY =
--                        if y > List.length acc then
--                            y
--
--                        else
--                            maxYacc
--
--                    newAcc =
--                        acc
--                in
--                ( maxY, newAcc )
--            )
--            ( 0, [] )
--        |> Tuple.second


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


viewQuadLeaf0 offsetX offsetY maxSize n color =
    let
        sizeStr =
            String.fromFloat (maxSize / (2 ^ n))
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


viewQuadtree0 offsetX offsetY maxSize n emptyColor tree =
    case tree of
        QuadEmpty ->
            emptyColor |> viewQuadLeaf0 offsetX offsetY maxSize n

        QuadLeaf color ->
            color |> viewQuadLeaf0 offsetX offsetY maxSize n

        QuadNode quadrants ->
            let
                o =
                    maxSize / (2 ^ (n + 1))
            in
            g
                []
                [ viewQuadtree0 offsetX offsetY maxSize (n + 1) emptyColor (getQuadrant TopLeft quadrants)
                , viewQuadtree0 (offsetX + o) offsetY maxSize (n + 1) emptyColor (getQuadrant TopRight quadrants)
                , viewQuadtree0 offsetX (offsetY + o) maxSize (n + 1) emptyColor (getQuadrant BottomLeft quadrants)
                , viewQuadtree0 (offsetX + o) (offsetY + o) maxSize (n + 1) emptyColor (getQuadrant BottomRight quadrants)
                ]


viewQuadtree maxSize emptyColor svgId tree =
    div
        [ style "position" "relative"
        , style "width" (String.fromFloat maxSize ++ "px")
        , style "height" (String.fromFloat maxSize ++ "px")
        ]
        [ svg
            [ width "100%"
            , height "100%"
            , Svg.Attributes.id svgId
            ]
            [ viewQuadtree0 0 0 maxSize 0 emptyColor tree ]
        ]
