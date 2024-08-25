module SelectArray exposing (..)

import Array exposing (Array)
import Debug exposing (log)


{-| Array that has 0 or 1 item selected by index.
Ensures that the index is bounded, and adjusts the index if necessary.
-}
type alias SelectArray a =
    Maybe ( Array a, Maybe Int )


testSelectArrays =
    [ fromList [] |> select 0
    , fromList [ 1 ] |> select -1
    , fromList [ 1 ] |> select 0
    , fromList [ 1 ] |> select 1
    ]
        |> List.map (log "before" >> clamp >> log "after")


empty : SelectArray a
empty =
    Nothing


fromArray : Array a -> SelectArray a
fromArray array =
    Just ( array, Nothing ) |> clamp


fromList : List a -> SelectArray a
fromList list =
    Just ( Array.fromList list, Nothing ) |> clamp


clamp : SelectArray a -> SelectArray a
clamp =
    Maybe.andThen
        (\( x, i ) ->
            if Array.isEmpty x then
                Nothing

            else
                Just
                    ( x
                    , i
                        |> Maybe.map (Basics.clamp 0 (Array.length x - 1))
                    )
        )


thenClamp : (SelectArray a -> SelectArray b) -> SelectArray a -> SelectArray b
thenClamp f =
    f >> clamp


select : Int -> SelectArray a -> SelectArray a
select index =
    thenClamp <| Maybe.map <| Tuple.mapSecond <| always <| Just index


deselect : SelectArray a -> SelectArray a
deselect =
    Maybe.map <| Tuple.mapSecond <| always Nothing


thenUpdate : (Array a -> Array b) -> SelectArray a -> SelectArray b
thenUpdate f =
    Maybe.withDefault ( Array.empty, Nothing )
        >> Tuple.mapFirst f
        >> Just
        >> clamp


push : a -> SelectArray a -> SelectArray a
push =
    thenUpdate << Array.push


initialize : Int -> (Int -> a) -> SelectArray a
initialize n f =
    Array.initialize n f |> fromArray
