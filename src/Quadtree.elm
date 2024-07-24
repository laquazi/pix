module Quadtree exposing (QuadNodeData, Quadtree(..), insertAtCoord, node, view)

import Browser
import Color exposing (Color)
import Common exposing (..)
import Debug exposing (log)
import Html exposing (Html, div, table, tbody, td, text, tr)
import Html.Attributes exposing (style)



-- type Quadtree a
--     = QuadLeaf a
--     | QuadNode { tl : Quadtree a, tr : Quadtree a, bl : Quadtree a, br : Quadtree a }
--     | QuadEmpty


type alias QuadNodeData a =
    List (Quadtree a)


type Quadtree a
    = QuadLeaf a
    | QuadNode (QuadNodeData a)
    | QuadEmpty


node tl tr bl br =
    QuadNode [ tl, tr, bl, br ]


repeatNodeData data =
    List.repeat 4 data


nodeId tree =
    if x <= halfMaxCoord && y <= halfMaxCoord then
        { newNodeData0 | tl = newSubnode }

    else if x > halfMaxCoord && y <= halfMaxCoord then
        { newNodeData0 | tr = newSubnode }

    else if x <= halfMaxCoord && y > halfMaxCoord then
        { newNodeData0 | bl = newSubnode }

    else
        { newNodeData0 | br = newSubnode }


insertAtCoord0 : a -> Point -> Int -> Int -> Quadtree a -> Quadtree a
insertAtCoord0 newData ({ x, y } as coord) scale maxScale tree =
    if scale <= 0 then
        QuadLeaf newData |> log "leaf"

    else
        let
            halfMaxCoord =
                (2 ^ scale) // 2 |> log "halfMaxCoord"

            mycoord =
                coord |> log "coord"

            newScale =
                scale - 1

            newCoord =
                { x = x |> remainderBy halfMaxCoord, y = y |> remainderBy halfMaxCoord } |> log "newCoord"
        in
        log "node" <|
            case tree |> log "tree" of
                QuadEmpty ->
                    let
                        newNodeData0 =
                            repeatNodeData QuadEmpty

                        newSubnode =
                            insertAtCoord0 newData newCoord newScale maxScale QuadEmpty
                    in
                    QuadNode <|
                        if x <= halfMaxCoord && y <= halfMaxCoord then
                            { newNodeData0 | tl = newSubnode }

                        else if x > halfMaxCoord && y <= halfMaxCoord then
                            { newNodeData0 | tr = newSubnode }

                        else if x <= halfMaxCoord && y > halfMaxCoord then
                            { newNodeData0 | bl = newSubnode }

                        else
                            { newNodeData0 | br = newSubnode }

                QuadLeaf data ->
                    let
                        newNodeData0 =
                            repeatNodeData (QuadLeaf data)

                        newSubnode =
                            insertAtCoord0 newData newCoord newScale maxScale (QuadLeaf data)
                    in
                    QuadNode <|
                        if x <= halfMaxCoord && y <= halfMaxCoord then
                            { newNodeData0 | tl = newSubnode }

                        else if x > halfMaxCoord && y <= halfMaxCoord then
                            { newNodeData0 | tr = newSubnode }

                        else if x <= halfMaxCoord && y > halfMaxCoord then
                            { newNodeData0 | bl = newSubnode }

                        else
                            { newNodeData0 | br = newSubnode }

                QuadNode node ->
                    QuadNode <|
                        if x <= halfMaxCoord && y <= halfMaxCoord then
                            { node | tl = insertAtCoord0 newData newCoord newScale maxScale node.tl }

                        else if x > halfMaxCoord && y <= halfMaxCoord then
                            { node | tr = insertAtCoord0 newData newCoord newScale maxScale node.tr }

                        else if x <= halfMaxCoord && y > halfMaxCoord then
                            { node | bl = insertAtCoord0 newData newCoord newScale maxScale node.bl }

                        else
                            { node | br = insertAtCoord0 newData newCoord newScale maxScale node.br }


insertAtCoord : a -> Point -> Int -> Quadtree a -> Quadtree a
insertAtCoord newData coord scale tree =
    insertAtCoord0 newData coord scale scale tree


viewQuadleaf color n maxSize =
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
            viewQuadleaf Color.white n maxSize

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
