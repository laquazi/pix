module Common exposing (..)

import Color exposing (Color)
import Task


type alias Point =
    { x : Int
    , y : Int
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
