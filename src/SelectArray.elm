module SelectArray exposing (..)

import Array exposing (Array)
import Array.Extra
import Common exposing (..)
import List.Extra
import Maybe.Extra


{-| Array that has 0 or 1 item selected by index.
Ensures that the index is bounded, and adjusts the index if necessary.
The module contains at least everything from `elm/core` Array.
WARN: untested
TODO: sanity check every function (especially the case of adding `ensure`)
TODO: consider a more detailed approach: track actual items instead of indices
TODO: consider expanding the type `Selected Int (Array a) -> Selected (Set Int) (Array a)`, just add functions to differentiate behaviors: selectSingle, selectMultiple, etc
-}
type SelectArray a
    = Empty
    | NotSelected (Array a)
    | Selected Int (Array a)


empty : SelectArray a
empty =
    Empty


notSelected : Array a -> SelectArray a
notSelected =
    NotSelected


selected : Int -> Array a -> SelectArray a
selected =
    Selected


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


getSelection : SelectArray a -> Maybe Int
getSelection array =
    case array of
        Selected i _ ->
            Just i

        _ ->
            Nothing


setSelection : Int -> SelectArray a -> SelectArray a
setSelection index array =
    ensure <|
        case array of
            Empty ->
                Empty

            NotSelected x ->
                x |> Selected index

            Selected _ x ->
                x |> Selected index


removeSelection : SelectArray a -> SelectArray a
removeSelection array =
    case array of
        Selected _ x ->
            NotSelected x

        _ ->
            array


updateSelection : (Maybe Int -> Maybe Int) -> SelectArray a -> SelectArray a
updateSelection f array =
    ensure <|
        case array of
            Empty ->
                Empty

            NotSelected x ->
                f Nothing
                    |> Maybe.Extra.unwrap (NotSelected x) (flip Selected x)

            Selected i x ->
                (f <| Just i)
                    |> Maybe.Extra.unwrap (NotSelected x) (flip Selected x)


update : (Maybe Int -> Array a -> SelectArray b) -> SelectArray a -> SelectArray b
update f array =
    ensure <|
        case array of
            Empty ->
                f Nothing Array.empty

            NotSelected x ->
                f Nothing x

            Selected i x ->
                f (Just i) x


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


isSelection : Int -> SelectArray a -> Bool
isSelection index array =
    case array of
        Selected i _ ->
            index == i

        _ ->
            False


length : SelectArray a -> Int
length =
    thenCast <| Array.length


getAt : Int -> SelectArray a -> Maybe a
getAt index =
    thenCast <| Array.get index


setAt : Int -> a -> SelectArray a -> SelectArray a
setAt index value =
    thenUpdate <| Array.set index value


updateAt : Int -> (a -> a) -> SelectArray a -> SelectArray a
updateAt index f =
    thenUpdate <| Array.Extra.update index f


getSelected : SelectArray a -> Maybe a
getSelected array =
    case array of
        Selected i x ->
            Array.get i x

        _ ->
            Nothing


setSelected : a -> SelectArray a -> SelectArray a
setSelected value array =
    case array of
        Selected i x ->
            x |> Array.set i value |> Selected i

        _ ->
            array


updateSelected : (a -> a) -> SelectArray a -> SelectArray a
updateSelected f array =
    case array of
        Selected i x ->
            x |> Array.Extra.update i f |> Selected i

        _ ->
            array


removeSelected : SelectArray a -> SelectArray a
removeSelected array =
    ensure <|
        case array of
            Selected i x ->
                x |> Array.Extra.removeAt i |> NotSelected

            _ ->
                array


insertAt : Int -> a -> SelectArray a -> SelectArray a
insertAt index value =
    thenUpdate <| Array.Extra.insertAt index value


removeAt : Int -> SelectArray a -> SelectArray a
removeAt =
    thenUpdate << Array.Extra.removeAt


push : a -> SelectArray a -> SelectArray a
push =
    thenUpdate << Array.push


pop : SelectArray a -> SelectArray a
pop =
    thenUpdate <| Array.Extra.pop


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


filterMap : (a -> Maybe b) -> SelectArray a -> SelectArray b
filterMap f =
    thenUpdate <| Array.Extra.filterMap f


maximum : SelectArray comparable -> Maybe comparable
maximum =
    toList >> List.maximum


minimum : SelectArray comparable -> Maybe comparable
minimum =
    toList >> List.minimum


swapAt : Int -> Int -> SelectArray a -> SelectArray a
swapAt a b array =
    case array of
        Empty ->
            Empty

        NotSelected x ->
            x
                |> Array.toList
                |> List.Extra.swapAt a b
                |> Array.fromList
                |> NotSelected

        Selected i x ->
            x
                |> Array.toList
                |> List.Extra.swapAt a b
                |> Array.fromList
                |> Selected i
