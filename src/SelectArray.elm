module SelectArray exposing (..)

import Array exposing (Array)
import Debug exposing (log)


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
    Just ( array, Nothing )


fromList : List a -> SelectArray a
fromList list =
    Just ( Array.fromList list, Nothing )


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


thenClamp : (SelectArray a -> SelectArray a) -> SelectArray a -> SelectArray a
thenClamp f =
    f >> clamp


select : Int -> SelectArray a -> SelectArray a
select index =
    thenClamp <| Maybe.map <| Tuple.mapSecond <| always <| Just index


deselect : SelectArray a -> SelectArray a
deselect =
    Maybe.map <| Tuple.mapSecond <| always Nothing
