module Common exposing (..)

import Array exposing (Array)
import Color exposing (Color)
import Dict exposing (Dict)
import Task


type alias Point =
    { x : Int
    , y : Int
    }


type alias FileFormatData =
    { extension : String
    , mimeType : String
    }


type RasterImageFormat
    = Png
    | Bmp
    | Gif


type VectorImageFormat
    = Svg


type ImageType
    = RasterImage RasterImageFormat
    | VectorImage VectorImageFormat


imageFormatData : ImageType -> FileFormatData
imageFormatData imageType =
    case imageType of
        RasterImage format ->
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

        VectorImage format ->
            case format of
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
