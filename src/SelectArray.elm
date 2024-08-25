module SelectArray exposing (..)

import Array exposing (Array)
import Debug exposing (log)


{-| Array that has 0 or 1 item selected by index.
Ensures that the index is bounded, and adjusts the index if necessary.
The module contains at least everything from `elm/core` Array.
WARN: untested
-}
type SelectArray a
    = Empty
    | NotSelected (Array a)
    | Selected Int (Array a)


testSelectArrays () =
    [ fromList [] |> select 0
    , fromList [ 1 ] |> select -1
    , fromList [ 1 ] |> select 0
    , fromList [ 1 ] |> select 1
    ]
        |> List.map (log "ensured")


fromArray : Array a -> SelectArray a
fromArray =
    NotSelected >> ensure


fromList : List a -> SelectArray a
fromList =
    Array.fromList >> fromArray


toArray : SelectArray a -> Array a
toArray array =
    case array of
        Empty ->
            Array.empty

        NotSelected x ->
            x

        Selected _ x ->
            x


toList : SelectArray a -> List a
toList =
    toArray >> Array.toList


toIndexedList : SelectArray a -> List ( Int, a )
toIndexedList =
    toArray >> Array.toIndexedList


ensure : SelectArray a -> SelectArray a
ensure array =
    case array of
        Empty ->
            Empty

        NotSelected x ->
            if Array.isEmpty x then
                Empty

            else
                NotSelected x

        Selected i x ->
            if Array.isEmpty x then
                Empty

            else
                x |> Selected (i |> Basics.clamp 0 (Array.length x - 1))


thenEnsure : (SelectArray a -> SelectArray b) -> SelectArray a -> SelectArray b
thenEnsure f =
    f >> ensure


select : Int -> SelectArray a -> SelectArray a
select index array =
    ensure <|
        case array of
            Empty ->
                Empty

            NotSelected x ->
                x |> Selected index

            Selected _ x ->
                x |> Selected index


deselect : SelectArray a -> SelectArray a
deselect array =
    case array of
        Selected _ x ->
            NotSelected x

        _ ->
            array


thenUpdate : (Array a -> Array b) -> SelectArray a -> SelectArray b
thenUpdate f array =
    ensure <|
        case array of
            Empty ->
                f Array.empty |> NotSelected

            NotSelected x ->
                f x |> NotSelected

            Selected i x ->
                f x |> Selected i


thenCast : (Array a -> b) -> SelectArray a -> b
thenCast f =
    toArray >> f


initialize : Int -> (Int -> a) -> SelectArray a
initialize n f =
    Array.initialize n f |> fromArray


repeat : Int -> a -> SelectArray a
repeat n value =
    value |> Array.repeat n |> fromArray


empty : SelectArray a
empty =
    Empty


notSelected : Array a -> SelectArray a
notSelected =
    NotSelected


selected : Int -> Array a -> SelectArray a
selected =
    Selected


isEmpty : SelectArray a -> Bool
isEmpty array =
    case array of
        Empty ->
            True

        _ ->
            False


isNotSelected : SelectArray a -> Bool
isNotSelected array =
    case array of
        NotSelected _ ->
            True

        _ ->
            False


isSelected : SelectArray a -> Bool
isSelected array =
    case array of
        Selected _ _ ->
            True

        _ ->
            False


length : SelectArray a -> Int
length =
    thenCast <| Array.length


get : Int -> SelectArray a -> Maybe a
get index =
    thenCast <| Array.get index


getSelected : SelectArray a -> Maybe a
getSelected array =
    case array of
        Selected i x ->
            Array.get i x

        _ ->
            Nothing


set : Int -> a -> SelectArray a -> SelectArray a
set index value =
    thenUpdate <| Array.set index value


push : a -> SelectArray a -> SelectArray a
push =
    thenUpdate << Array.push


{-| NOTE: same as `thenUpdate <| Array.append toAppend array`, but more efficient
-}
append : Array a -> SelectArray a -> SelectArray a
append toAppend array =
    case array of
        Empty ->
            toAppend |> NotSelected

        NotSelected x ->
            x |> Array.append toAppend |> NotSelected

        Selected i x ->
            x |> Array.append toAppend |> Selected i


slice : Int -> Int -> SelectArray a -> SelectArray a
slice start end =
    thenUpdate <| Array.slice start end


map : (a -> b) -> SelectArray a -> SelectArray b
map f =
    thenUpdate <| Array.map f


filter : (a -> Bool) -> SelectArray a -> SelectArray a
filter f =
    thenUpdate <| Array.filter f


indexedMap : (Int -> a -> b) -> SelectArray a -> SelectArray b
indexedMap f =
    thenUpdate <| Array.indexedMap f


foldl : (a -> b -> b) -> b -> SelectArray a -> b
foldl f acc =
    thenCast <| Array.foldl f acc


foldr : (a -> b -> b) -> b -> SelectArray a -> b
foldr f acc =
    thenCast <| Array.foldr f acc
