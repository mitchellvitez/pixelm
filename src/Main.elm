module Main exposing (..)

import Array exposing (Array, repeat, set, get)
import Html exposing (program, Html, div, p, button, img, text, br, input)
import Html.Attributes exposing (style, src, min, max, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseDown, onMouseUp)
import String
import Focus exposing (Focus)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { pixels : Array (Array Color)
    , color : Color
    , mouseDown : Bool
    }


type Msg
    = PencilDraw Point
    | SetMouseDown Bool
    | SetColor ColorName String
    | Fill Point


type ColorName
    = Red
    | Green
    | Blue


type alias Color =
    { r : Int
    , g : Int
    , b : Int
    }


type alias Point =
    { x : Int
    , y : Int
    }


init : ( Model, Cmd Msg )
init =
    { pixels = repeat 40 (repeat 80 { r = 255, g = 255, b = 255 })
    , color = { r = 150, g = 150, b = 150 }
    , mouseDown = False
    }
        ! []


pixelSize : String
pixelSize =
    "1.25vw"


view : Model -> Html Msg
view model =
    div
        [ onMouseDown (SetMouseDown True)
        , onMouseUp (SetMouseDown False)
        ]
        ((Array.toList (Array.indexedMap viewRow model.pixels))
            ++ List.map viewInput
                [ ( SetColor Red, "red" )
                , ( SetColor Green, "green" )
                , ( SetColor Blue, "blue" )
                ]
            ++ [ div
                    [ style
                        [ ( "width", "100%" )
                        , ( "height", "50px" )
                        , ( "background", toCssRgb model.color )
                        ]
                    ]
                    []
               ]
        )


toCssRgb : Color -> String
toCssRgb color =
    "rgb("
        ++ toString color.r
        ++ ","
        ++ toString color.g
        ++ ","
        ++ toString color.b
        ++ ")"


viewInput : ( String -> Msg, String ) -> Html Msg
viewInput ( message, colorName ) =
    div
        [ style
            [ ( "background", colorName )
            , ( "display", "inline-block" )
            , ( "width", "33%" )
            ]
        ]
        [ input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "255"
            , onInput message
            ]
            []
        ]


viewRow : Int -> Array Color -> Html Msg
viewRow x arr =
    div
        [ style
            [ ( "margin", "0" )
            , ( "height", pixelSize )
            ]
        ]
        (Array.toList (Array.indexedMap (viewPixel x) arr))


viewPixel : Int -> Int -> Color -> Html Msg
viewPixel x y color =
    let
        pt =
            { x = x, y = y }
    in
        div
            [ style
                [ ( "background", toCssRgb color )
                , ( "width", pixelSize )
                , ( "height", pixelSize )
                , ( "margin", "0" )
                , ( "display", "inline-block" )
                ]
            , onClick (Fill pt)
            , onMouseEnter (PencilDraw pt)
            ]
            []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fill point ->
            let
                maybeColorToReplace =
                    getPixel model.pixels point
            in
                case maybeColorToReplace of
                    Just colorToReplace ->
                        if colorToReplace == model.color then
                            model ! []
                        else
                            fill colorToReplace point model ! []

                    Nothing ->
                        model ! []

        PencilDraw point ->
            if model.mouseDown then
                placePixel model point ! []
            else
                model ! []

        SetMouseDown x ->
            { model | mouseDown = x } ! []

        SetColor color val ->
            let
                c =
                    model.color

                ( accessor, updater ) =
                    case color of
                        Red ->
                            ( .r, \x -> { c | r = x } )

                        Green ->
                            ( .g, \x -> { c | g = x } )

                        Blue ->
                            ( .b, \x -> { c | b = x } )
            in
                setColor
                    (Focus.create accessor <|
                        \f r -> accessor r |> f |> updater
                    )
                    model
                    val
                    ! []


fill : Color -> Point -> Model -> Model
fill colorToReplace pt model =
    if getPixel model.pixels pt /= Just colorToReplace then
        model
    else
        placePixel model pt
            |> fill colorToReplace { pt | x = pt.x - 1 }
            |> fill colorToReplace { pt | x = pt.x + 1 }
            |> fill colorToReplace { pt | y = pt.y - 1 }
            |> fill colorToReplace { pt | y = pt.y + 1 }


getPixel : Array (Array Color) -> Point -> Maybe Color
getPixel pixels pt =
    get pt.x pixels
        |> Maybe.withDefault Array.empty
        |> get pt.y


placePixel : Model -> Point -> Model
placePixel model pt =
    { model
        | pixels =
            let
                row =
                    Maybe.withDefault Array.empty (get pt.x model.pixels)
            in
                set pt.x (set pt.y model.color row) model.pixels
    }


setColor : Focus Color Int -> Model -> String -> Model
setColor whichColor model value =
    let
        colorAmount =
            case String.toInt value of
                Ok x ->
                    x

                Err _ ->
                    Focus.get whichColor model.color
    in
        { model | color = Focus.set whichColor colorAmount model.color }
