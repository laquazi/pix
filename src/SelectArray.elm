module SelectArray exposing (..)

import Array exposing (Array)
import Debug exposing (log)


{-| Array that has 0 or 1 item selected by index.
Ensures that the index is bounded, and adjusts the index if necessary.
-}
type SelectArray a
    = Empty
    | NotSelected (Array a)
    | Selected (Array a) Int


testSelectArrays =
    [ fromList [] |> select 0
    , fromList [ 1 ] |> select -1
    , fromList [ 1 ] |> select 0
    , fromList [ 1 ] |> select 1
    ]
        |> List.map (log "before" >> ensure >> log "after")


fromArray : Array a -> SelectArray a
fromArray =
    NotSelected >> ensure


fromList : List a -> SelectArray a
fromList =
    Array.fromList >> fromArray


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

        Selected x i ->
            if Array.isEmpty x then
                Empty

            else
                Selected x (i |> Basics.clamp 0 (Array.length x - 1))


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
                Selected x index

            Selected x _ ->
                Selected x index


deselect : SelectArray a -> SelectArray a
deselect array =
    case array of
        Selected x _ ->
            NotSelected x

        _ ->
            array


thenUpdate : (Array a -> Array b) -> SelectArray a -> SelectArray b
thenUpdate f array =
    ensure <|
        case array of
            Empty ->
                NotSelected (f Array.empty)

            NotSelected x ->
                NotSelected (f x)

            Selected x i ->
                Selected (f x) i


thenCast : (Array a -> b) -> SelectArray a -> b
thenCast f array =
    case array of
        Empty ->
            f Array.empty

        NotSelected x ->
            f x

        Selected x _ ->
            f x


push : a -> SelectArray a -> SelectArray a
push =
    thenUpdate << Array.push


initialize : Int -> (Int -> a) -> SelectArray a
initialize n f =
    Array.initialize n f |> fromArray


repeat : Int -> a -> SelectArray a
repeat n value =
    value |> Array.repeat n |> fromArray


empty : SelectArray a
empty =
    Empty


isEmpty : SelectArray a -> Bool
isEmpty array =
    case array of
        Empty ->
            True

        _ ->
            False


length : SelectArray a -> Int
length =
    thenCast <| Array.length


get : Int -> SelectArray a -> Maybe a
get index =
    thenCast <| Array.get index
