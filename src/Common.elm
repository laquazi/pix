module Common exposing (..)

import Array exposing (Array)
import Color exposing (Color)
import Dict exposing (Dict)
import List.Extra
import Process
import Task
import Time


type alias Point =
    { x : Int
    , y : Int
    }


type alias FileFormatData =
    { extension : String
    , mimeType : String
    }


type ImageFormat
    = Png
    | Bmp
    | Gif
    | Svg


imageFormatData : ImageFormat -> FileFormatData
imageFormatData format =
    case format of
        Png ->
            { extension = "png"
            , mimeType = "image/png"
            }

        Bmp ->
            { extension = "bmp"
            , mimeType = "image/bmp"
            }

        Gif ->
            { extension = "gif"
            , mimeType = "image/gif"
            }

        Svg ->
            { extension = "svg"
            , mimeType = "image/svg+xml"
            }


colorTransparent : Color
colorTransparent =
    Color.rgba 0 0 0 0


colorBlendingNormal : Color -> Color -> Color
colorBlendingNormal a _ =
    a


cmd : msg -> Cmd msg
cmd m =
    Task.perform (always m) (Task.succeed ())


arraySingleton : a -> Array a
arraySingleton data =
    Array.repeat 1 data


arrayConcat : Array (Array a) -> Array a
arrayConcat arrays =
    Array.foldl (\array acc -> Array.append acc array) Array.empty arrays


dictFromArray : Array ( comparable, v ) -> Dict comparable v
dictFromArray assocs =
    Array.foldl (\( key, value ) dict -> Dict.insert key value dict) Dict.empty assocs


px : Float -> String
px x =
    String.fromFloat x ++ "px"


listInsertAt index data list =
    list |> List.Extra.splitAt index |> (\( a, b ) -> a ++ [ data ] ++ b)


tupleThird ( _, _, c ) =
    c


delayMsg : Float -> msg -> Cmd msg
delayMsg timeMs msg =
    Process.sleep timeMs |> Task.perform (\_ -> msg)
