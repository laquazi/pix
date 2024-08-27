module Common exposing (..)

import Array exposing (Array)
import Color exposing (Color)
import Dict exposing (Dict)
import List.Extra
import Process
import Task


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


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b


{-| NOTE: adopted from `https://wiki.haskell.org/Bresenham%27s_line_drawing_algorithm`
NOTE: includes a and b
-}
generateLine : Point -> Point -> List Point
generateLine a b =
    let
        steep =
            abs (b.y - a.y) > abs (b.x - a.x)

        maySwitch =
            if steep then
                \{ x, y } -> { x = y, y = x }

            else
                identity

        ( ( x1, y1 ), ( x2, y2 ) ) =
            case
                [ maySwitch a, maySwitch b ]
                    |> List.map (\{ x, y } -> ( x, y ))
                    |> List.sort
            of
                [ x, y ] ->
                    ( x, y )

                _ ->
                    -- NOTE: this should never happen
                    ( ( 0, 0 ), ( 0, 0 ) )

        dx =
            x2 - x1

        dy =
            abs (y2 - y1)

        stepY =
            if y1 < y2 then
                1

            else
                -1

        go ( xTemp, yTemp, error ) =
            let
                tempError =
                    error + dy

                ( newY, newError ) =
                    if (2 * tempError) >= dx then
                        ( yTemp + stepY, tempError - dx )

                    else
                        ( yTemp, tempError )
            in
            if xTemp > x2 then
                Nothing

            else
                Just ( { x = xTemp, y = yTemp }, ( xTemp + 1, newY, newError ) )
    in
    List.Extra.unfoldr go ( x1, y1, 0 )
        |> List.map maySwitch


{-| NOTE: does not include a and b
-}
generateLineBetween : Point -> Point -> List Point
generateLineBetween a b =
    generateLine a b |> List.filter (\x -> x /= a && x /= b)
