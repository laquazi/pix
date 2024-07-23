module Main exposing (..)

import Browser
import Html exposing (Html, button, div, table, tbody, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Quadtree exposing (..)


quad0 =
    QuadNode
        { tl = QuadLeaf "#F66"
        , tr = QuadLeaf "#FF6"
        , bl = QuadLeaf "#6F6"
        , br =
            QuadNode
                { tl = QuadLeaf "#6FF"
                , tr = QuadLeaf "#66F"
                , bl = QuadLeaf "#F6F"
                , br = QuadEmpty
                }
        }



-- type alias Point =
--     { x : Int
--     , y : Int
--     }


type alias Color =
    String



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { scale : Int
    , data : Quadtree Color
    , size : Int
    }


init : Model
init =
    { scale = 2
    , data = quad0
    , size = 512
    }



-- UPDATE


type Msg
    = Reset
    | ScaleChange Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            init

        ScaleChange newScale ->
            if newScale < 0 then
                { model | scale = 0 }

            else
                { model | scale = newScale }



-- VIEW


viewModelDebug model =
    div [ style "margin" "10px" ] [ text (Debug.toString model) ]


viewRuler scale maxSize =
    let
        n =
            2 ^ scale

        sizeStr =
            String.fromFloat (maxSize / n) ++ "px"
    in
    table
        [ style "border-collapse" "collapse"
        , style "position" "absolute"
        , style "z-index" "1"
        , style "outline" "0.25px solid #333"
        , style "outline-offset" "-0.25px"
        ]
        [ tbody []
            (List.repeat (round n)
                (tr []
                    (List.repeat (round n)
                        (td
                            [ style "padding" "0"
                            , style "outline" "0.125px solid #333"
                            , style "outline-offset" "-0.125px"
                            ]
                            [ div
                                [ style "width" sizeStr
                                , style "height" sizeStr
                                ]
                                [ text "\u{200B}" ]
                            ]
                        )
                    )
                )
            )
        ]


viewCanvas model =
    div
        [ style "position" "relative"
        , style "margin" "10px"
        ]
        [ viewRuler (toFloat model.scale) (toFloat model.size)
        , viewQuadtree model.data (toFloat model.size)
        ]


viewMsgButtons model =
    div [ style "margin" "10px" ]
        [ button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick (ScaleChange (model.scale + 1)) ] [ text "Increment Scale" ]
        , button [ onClick (ScaleChange (model.scale - 1)) ] [ text "Decrement Scale" ]
        ]


view : Model -> Html Msg
view model =
    div
        [ style "position" "absolute" ]
        [ viewMsgButtons model
        , viewCanvas model
        , viewModelDebug model
        ]
