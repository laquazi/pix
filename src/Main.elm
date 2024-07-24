module Main exposing (..)

import Browser
import Color exposing (Color)
import Color.Oklab
import Color.Oklch
import Common exposing (..)
import Html exposing (Html, button, div, table, tbody, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Extra
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



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { canvas : Quadtree Color
    , scale : Int
    , isRulerVisible : Bool
    , size : Int
    , color : Color
    , colorpalette : List Color
    }


init : Model
init =
    { canvas = quad0
    , scale = 2
    , isRulerVisible = True
    , size = 512
    , color = Color.rgb 255 219 0
    , colorpalette =
        List.Extra.initialize 100
            (\x ->
                Color.Oklch.oklch 0.8 0.5 (10 / (toFloat x + 1))
                    |> Color.Oklch.toColor
            )
    }



-- UPDATE


type Msg
    = Reset
    | ScaleChange Int
    | ColorChange Color
    | RulerVisibleToggle
    | Draw Point


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

        ColorChange color ->
            { model | color = color }

        RulerVisibleToggle ->
            { model | isRulerVisible = not model.isRulerVisible }

        Draw { x, y } ->
            --TODO
            { model | canvas = model.canvas }



-- VIEW


viewModelDebug model =
    div [ style "margin" "10px" ] [ text (Debug.toString model) ]


viewRuler scale maxSize isVisible =
    let
        n =
            2 ^ scale

        sizeStr =
            String.fromFloat (maxSize / n) ++ "px"
    in
    div
        [ style "z-index" "1"
        , style "position" "absolute"

        --, style "border" "0.00625em solid #333"
        -- , style "box-sizing" "border-box"
        -- , style "box-shadow" "0.5px 0.5px -0.5px 1px #333"
        , style "background-color" "#333"
        , if isVisible then
            style "visibility" "visible"

          else
            style "visibility" "hidden"
        ]
        [ div [] []
        , table
            [ style "border-collapse" "collapse"
            , style "width" "99%"
            , style "height" "99%"
            , style "background-color" "transperent"
            , style "z-index" "2"
            ]
            [ tbody []
                (List.Extra.initialize (round n)
                    (\y ->
                        tr []
                            (List.Extra.initialize (round n)
                                (\x ->
                                    td
                                        [ style "padding" "0"
                                        , style "width" sizeStr
                                        , style "height" sizeStr
                                        , style "background-color" "#333"
                                        ]
                                        [ div
                                            [ style "width" "99.8%"
                                            , style "height" "99.8%"

                                            -- , style "border" "0.00625em solid #333"
                                            -- , style "box-sizing" "border-box"
                                            , onClick (Draw { x = x, y = y })
                                            ]
                                            [ text "\u{200B}" ]
                                        ]
                                )
                            )
                    )
                )
            ]
        ]


viewCanvas model =
    div
        [ style "position" "relative"
        , style "margin" "10px"
        ]
        [ viewRuler (toFloat model.scale) (toFloat model.size) model.isRulerVisible
        , viewQuadtree model.canvas (toFloat model.size)
        ]


viewColorpaletteColor color =
    div
        [ style "width" "40px"
        , style "height" "40px"
        , style "background-color" (Color.toCssString color)
        , onClick (ColorChange color)
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
        , button [ onClick RulerVisibleToggle ] [ text "Toggle Ruler" ]
        ]


viewSelectedColor model =
    div
        [ style "width" "80px"
        , style "height" "80px"
        , style "background-color" (Color.toCssString model.color)
        , style "margin" "10px"
        ]
        [ text "\u{200B}" ]


view : Model -> Html Msg
view model =
    div
        [ style "position" "absolute" ]
        [ viewMsgButtons model
        , viewCanvas model
        , viewSelectedColor model
        , viewColorpalette model
        , viewModelDebug model
        ]
