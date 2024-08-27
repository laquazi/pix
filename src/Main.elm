module Main exposing (..)

import Browser
import Canvas.Mvu exposing (Msg(..), Tool(..))
import Color exposing (Color)
import Common exposing (..)
import Config exposing (..)
import Debug exposing (log)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick)
import Quadtree exposing (Quadtree(..))
import SelectArray


logCmd : String -> a -> Cmd Msg
logCmd msg data =
    data |> Debug.toString |> Log msg |> cmd



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { canvasModel : Canvas.Mvu.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( canvasModel, canvasCmd ) =
            Canvas.Mvu.init ()
    in
    ( { canvasModel = canvasModel }
    , Cmd.batch [ Cmd.map UpdateModelCanvas canvasCmd ]
    )



-- UPDATE


type Msg
    = Reset
    | Noop
    | Log String String
    | MsgBatch (List Msg)
    | RunCmd (Cmd Msg)
    | WithCmd Msg (Cmd Msg)
    | Test
    | UpdateModelCanvas Canvas.Mvu.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Reset ->
            init ()

        Log tag value ->
            let
                _ =
                    log tag value
            in
            ( model, Cmd.none )

        MsgBatch msgs ->
            ( model, msgs |> List.map cmd |> Cmd.batch )

        RunCmd cmd ->
            ( model, cmd )

        WithCmd withMsg cmd ->
            let
                ( newModel, newCmd ) =
                    update withMsg model
            in
            ( newModel, Cmd.batch [ newCmd, cmd ] )

        Test ->
            let
                canvasModel =
                    model.canvasModel

                newCanvasModel =
                    canvasModel
                        |> Canvas.Mvu.drawLineTest { x = 0, y = 0 } { x = 2, y = 3 }
                        |> Canvas.Mvu.drawLineTest { x = 7, y = 7 } { x = 5, y = 6 }
            in
            ( { model | canvasModel = newCanvasModel }, Cmd.none )

        UpdateModelCanvas m ->
            let
                ( newModel, newCmd ) =
                    Canvas.Mvu.update m model.canvasModel
            in
            ( { model | canvasModel = newModel }, Cmd.map UpdateModelCanvas newCmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewDebug data =
    div [ style "margin" (px config.defaultMargin) ] [ text (Debug.toString data) ]


viewMsgButtons : Model -> Html Msg
viewMsgButtons model =
    div [ style "margin" (px config.defaultMargin) ]
        [ button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick (UpdateModelCanvas <| CanvasClearLayer) ] [ text "Clear" ]
        , button [ onClick (UpdateModelCanvas <| ChangeScale (model.canvasModel.scale + 1)) ] [ text "+ Scale" ]
        , button [ onClick (UpdateModelCanvas <| ChangeScale (model.canvasModel.scale - 1)) ] [ text "- Scale" ]
        , button [ onClick (UpdateModelCanvas <| RulerVisibleToggle) ] [ text "Toggle Ruler" ]
        , button [ onClick (UpdateModelCanvas <| ChangeTool Pencil) ] [ text "✎" ]
        , button [ onClick (UpdateModelCanvas <| ChangeTool Eraser) ] [ text "▱" ]
        , button [ onClick (UpdateModelCanvas <| DownloadCanvas { defaultDownloadImageData | format = Png }) ] [ text "⇓Png" ]
        , button [ onClick (UpdateModelCanvas <| DownloadCanvas { defaultDownloadImageData | format = Svg }) ] [ text "⇓Svg" ]
        , button [ onClick (UpdateModelCanvas <| DownloadCanvas { defaultDownloadImageData | format = Bmp }) ] [ text "⇓Bmp" ]
        , button [ onClick (UpdateModelCanvas <| DownloadCanvas { defaultDownloadImageData | format = Gif }) ] [ text "⇓Gif" ]
        , button [ onClick (UpdateModelCanvas <| UploadCanvas) ] [ text "⇑" ]
        , button [ onClick Test ] [ text "Test" ]
        ]


view : Model -> Html Msg
view model =
    div
        [ style "position" "absolute"
        , style "background-color" (Color.toCssString config.color.background)
        , style "width" "100%"
        , style "height" "100%"
        ]
        [ viewMsgButtons model
        , Canvas.Mvu.viewCanvasContainer model.canvasModel |> Html.map UpdateModelCanvas
        , Canvas.Mvu.viewSelectedColor model.canvasModel |> Html.map UpdateModelCanvas
        , Canvas.Mvu.viewColorpalette model.canvasModel |> Html.map UpdateModelCanvas
        , Canvas.Mvu.viewLayers model.canvasModel |> Html.map UpdateModelCanvas

        --, viewDebug model.maybeRenameLayerTimer
        ]
