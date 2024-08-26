module Config exposing (..)

import Color exposing (black, rgb255, white)
import Color.Interpolate
import Common exposing (ImageFormat(..))
import List.Extra


config =
    { color =
        { background = rgb255 218 218 218
        , ruler = rgb255 140 140 140
        , opaqueBackground = rgb255 140 140 140
        , test = rgb255 255 219 0
        }
    , zIndex =
        { canvas = 0
        , canvasRuler = 1

        --, canvasContainer = 4
        --, floatingElement = 8
        }
    , defaultMargin = 10
    , layerPreviewSize = 64
    }


defaultDownloadImageData =
    { format = Png, scale = 0, filename = "pix" }


defaultColorpalette =
    [ rgb255 255 138 128 -- redA100
    , rgb255 255 128 171 -- pinkA100
    , rgb255 234 128 252 -- purpleA100
    , rgb255 179 136 255 -- deepPurpleA100
    , rgb255 140 158 255 -- indigoA100
    , rgb255 130 177 255 -- blueA100
    , rgb255 128 216 255 -- lightBlueA100
    , rgb255 132 255 255 -- cyanA100
    , rgb255 167 255 235 -- tealA100
    , rgb255 185 246 202 -- greenA100
    , rgb255 204 255 144 -- lightGreenA100
    , rgb255 244 255 129 -- limeA100
    , rgb255 255 255 141 -- yellowA100
    , rgb255 255 229 127 -- amberA100
    , rgb255 255 209 128 -- orangeA100
    , rgb255 255 158 128 -- deepOrangeA100
    , rgb255 255 82 82 -- redA200
    , rgb255 255 64 129 -- pinkA200
    , rgb255 224 64 251 -- purpleA200
    , rgb255 124 77 255 -- deepPurpleA200
    , rgb255 83 109 254 -- indigoA200
    , rgb255 68 138 255 -- blueA200
    , rgb255 64 196 255 -- lightBlueA200
    , rgb255 24 255 255 -- cyanA200
    , rgb255 100 255 218 -- tealA200
    , rgb255 105 240 174 -- greenA200
    , rgb255 178 255 89 -- lightGreenA200
    , rgb255 238 255 65 -- limeA200
    , rgb255 255 255 0 -- yellowA200
    , rgb255 255 215 64 -- amberA200
    , rgb255 255 171 64 -- orangeA200
    , rgb255 255 110 64 -- deepOrangeA200
    , rgb255 255 23 68 -- redA400
    , rgb255 245 0 87 -- pinkA400
    , rgb255 213 0 249 -- purpleA400
    , rgb255 101 31 255 -- deepPurpleA400
    , rgb255 61 90 254 -- indigoA400
    , rgb255 41 121 255 -- blueA400
    , rgb255 0 176 255 -- lightBlueA400
    , rgb255 0 229 255 -- cyanA400
    , rgb255 29 233 182 -- tealA400
    , rgb255 0 230 118 -- greenA400
    , rgb255 118 255 3 -- lightGreenA400
    , rgb255 198 255 0 -- limeA400
    , rgb255 255 234 0 -- yellowA400
    , rgb255 255 196 0 -- amberA400
    , rgb255 255 145 0 -- orangeA400
    , rgb255 255 61 0 -- deepOrangeA400
    , rgb255 213 0 0 -- redA700
    , rgb255 197 17 98 -- pinkA700
    , rgb255 170 0 255 -- purpleA700
    , rgb255 98 0 234 -- deepPurpleA700
    , rgb255 48 79 254 -- indigoA700
    , rgb255 41 98 255 -- blueA700
    , rgb255 0 145 234 -- lightBlueA700
    , rgb255 0 184 212 -- cyanA700
    , rgb255 0 191 165 -- tealA700
    , rgb255 0 200 83 -- greenA700
    , rgb255 100 221 23 -- lightGreenA700
    , rgb255 174 234 0 -- limeA700
    , rgb255 255 214 0 -- yellowA700
    , rgb255 255 171 0 -- amberA700
    , rgb255 255 109 0 -- orangeA700
    , rgb255 221 44 0 -- deepOrangeA700
    ]
        ++ List.Extra.initialize 15 (\x -> Color.Interpolate.interpolate Color.Interpolate.RGB white black (toFloat x / 15))
        ++ [ black ]
        |> List.reverse
