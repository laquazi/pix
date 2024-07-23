module Main exposing (..)

import Browser
import Color exposing (Color)
import Color.Oklab
import Html exposing (Html, button, div, table, tbody, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Quadtree exposing (..)


quad0 =
    QuadNode
        { tl = QuadLeaf Color.red
        , tr = QuadLeaf Color.orange
        , bl = QuadLeaf Color.yellow
        , br =
            QuadNode
                { tl = QuadLeaf Color.green
                , tr = QuadLeaf Color.blue
                , bl = QuadLeaf Color.purple
                , br = QuadEmpty
                }
        }



-- type alias Point =
--     { x : Int
--     , y : Int
--     }
-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { scale : Int
    , data : Quadtree Color
    , size : Int
    , colorpalette : List Color
    }


init : Model
init =
    { scale = 2
    , data = quad0
    , size = 512
    , colorpalette = []

    -- [ "#FC0"
    -- , "#CF0"
    -- , "#0FC"
    -- , "#F0C"
    -- , "#C0F"
    -- , "#0CF"
    -- ]
    -- [ Color.fromHex "#FFF"
    -- , "#FFC"
    -- , "#FF0"
    -- , "#FCF"
    -- , "#FCC"
    -- , "#FC0"
    -- , "#F0F"
    -- , "#F0C"
    -- , "#F00"
    -- , "#CFF"
    -- , "#CFC"
    -- , "#CF0"
    -- , "#CCF"
    -- , "#CCC"
    -- , "#CC0"
    -- , "#C0F"
    -- , "#C0C"
    -- , "#C00"
    -- , "#0FF"
    -- , "#0FC"
    -- , "#0F0"
    -- , "#0CF"
    -- , "#0CC"
    -- , "#0C0"
    -- , "#00F"
    -- , "#00C"
    -- , "#000"
    -- ]
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


viewColorpaletteColor color =
    div
        [ style "width" "40px"
        , style "height" "40px"
        , style "background-color" (Color.toCssString color)
        ]
        [ text "\u{200B}" ]


viewColorpalette model =
    div
        [ style "margin" "10px"
        , style "max-width" (String.fromInt model.size ++ "px")
        , style "display" "flex"
        , style "flex-wrap" "wrap"
        ]
        (model.colorpalette |> List.map (\x -> viewColorpaletteColor x))


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
        , viewColorpalette model
        , viewModelDebug model
        ]
