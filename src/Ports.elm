port module Ports exposing (..)

import Json.Decode as JD
import Json.Encode as JE



--port canvasRulerPressed : (JE.Value -> msg) -> Sub msg


port pointerSetCapture : JD.Value -> Cmd msg


port pointerSetCaptureById : JD.Value -> Cmd msg


port pointerReleaseCaptureById : JD.Value -> Cmd msg


encodeCapturePointerById : Int -> String -> JD.Value
encodeCapturePointerById pointerId elementId =
    JE.object
        [ ( "pointerId", JE.int pointerId )
        , ( "elementId", JE.string elementId )
        ]



--decodePoint : JD.Decoder Point
--decodePoint =
--    JD.map2 Point (JD.field "x" JD.int) (JD.field "y" JD.int)
--encodePoint : Point -> JD.Value
--encodePoint point =
--    JE.object
--        [ ( "x", JE.int point.x )
--        , ( "y", JE.int point.y )
--        ]
--encodeDownloadSvgAsPng : String -> String -> Int -> JD.Value
--encodeDownloadSvgAsPng elementId filename size =
--    JE.object
--        [ ( "elementId", JE.string elementId )
--        , ( "size", encodePoint { x = size, y = size } )
--        , ( "filename", JE.string filename )
--        ]
--canvasRulerPressed
--    (\jsPoint ->
--        jsPoint
--            |> JD.decodeValue decodePoint
--            |> Result.Extra.unpack
--                (\error -> Log ( "Error", JD.errorToString error ))
--                Draw
--    )
