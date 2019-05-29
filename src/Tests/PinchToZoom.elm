module Main exposing (Model, Msg(..), colours, init, main, update, view)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (GameApp, GetKeyState, KeyState(..), Keys(..), gameApp)


type alias Model =
    { time : Float
    , startingFingers : ( ( Float, Float ), ( Float, Float ) )
    , startingRadius : Float
    , startingRotation : Float
    , startingPosition : ( Float, Float )
    , currentRadius : Float
    , currentPosition : ( Float, Float )
    , currentRotation : Float
    }


type Msg
    = Tick Float GetKeyState
    | TouchStart (List TouchEvent)
    | TouchMove (List TouchEvent)
    | TouchEnd (List TouchEvent)


main : GameApp Model Msg
main =
    gameApp Tick
        { model = init
        , update = update
        , view = view
        , title = "Pinch to Zoom Example"
        }


init : Model
init =
    { time = 0
    , startingFingers = ( ( 0, 0 ), ( 0, 0 ) )
    , startingRadius = 0
    , startingRotation = 0
    , startingPosition = ( 0, 0 )
    , currentRadius = 15
    , currentPosition = ( 0, 0 )
    , currentRotation = 0
    }


com : List ( Float, Float ) -> ( Float, Float )
com poss =
    Tuple.mapBoth (\x -> x / toFloat (List.length poss)) (\y -> y / toFloat (List.length poss)) <|
        List.foldl (\( a, b ) ( x, y ) -> ( a + x, b + y )) ( 0, 0 ) poss


subtractTuples : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
subtractTuples ( a, b ) ( x, y ) =
    ( a - x, b - y )


addTuples : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
addTuples ( a, b ) ( x, y ) =
    ( a + x, b + y )


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick t _ ->
            { model | time = t }

        TouchStart events ->
            { model
                | startingFingers =
                    case events of
                        [ ( _, pos0 ), ( _, pos1 ) ] ->
                            ( pos0, pos1 )

                        _ ->
                            model.startingFingers
                , startingRadius =
                    case events of
                        [ ( _, pos0 ), ( _, pos1 ) ] ->
                            model.currentRadius

                        _ ->
                            model.startingRadius
                , startingPosition =
                    case events of
                        [ ( _, pos0 ), ( _, pos1 ) ] ->
                            model.currentPosition

                        _ ->
                            model.startingPosition
                , startingRotation =
                    case events of
                        [ ( _, pos0 ), ( _, pos1 ) ] ->
                            model.currentRotation

                        _ ->
                            model.startingRotation
            }

        TouchMove events ->
            case events of
                [ ( _, pos0 ), ( _, pos1 ) ] ->
                    let
                        ( ( x0_1, y0_1 ), ( x0_2, y0_2 ) ) =
                            model.startingFingers

                        d0 =
                            sqrt ((x0_2 - x0_1) ^ 2 + (y0_2 - y0_1) ^ 2)

                        theta0 =
                            atan2 (y0_2 - y0_1) (x0_2 - x0_1)

                        ( ( x1_1, y1_1 ), ( x1_2, y1_2 ) ) =
                            ( pos0, pos1 )

                        d1 =
                            sqrt ((x1_2 - x1_1) ^ 2 + (y1_2 - y1_1) ^ 2)

                        theta1 =
                            atan2 (y1_2 - y1_1) (x1_2 - x1_1)

                        comOriginal =
                            com [ ( x0_1, y0_1 ), ( x0_2, y0_2 ) ]

                        comNew =
                            com [ pos0, pos1 ]

                        newPos =
                            addTuples model.startingPosition <| subtractTuples comNew comOriginal

                        newRot =
                            model.startingRotation + (theta1 - theta0)

                        scale =
                            1 + (d1 - d0) / d0
                    in
                    { model
                        | currentRadius = model.startingRadius * scale
                        , currentPosition = newPos
                        , currentRotation = newRot
                    }

                _ ->
                    model

        TouchEnd _ ->
            model


colours =
    [ red, orange, yellow, green, blue, purple, pink, black, white, brown, lightBlue, lightRed, lightGreen ]


view : Model -> Collage Msg
view model =
    let
        ( px, py ) =
            model.currentPosition
    in
    collage 192
        128
        [ rect 192 128
            |> filled gray
        , oval (model.currentRadius * 2) model.currentRadius
            |> filled red
            |> notifyTouchStart TouchStart
            |> notifyTouchMove TouchMove
            |> notifyTouchEnd TouchMove
            |> rotate model.currentRotation
            |> move model.currentPosition
        , text ("Position: (" ++ String.fromInt (round px) ++ "," ++ String.fromInt (round py) ++ ")")
            |> size 5
            |> filled black
            |> move ( 56, -40 )
        , text ("Rotation: " ++ String.fromInt (round <| model.currentRotation * 180 / pi) ++ "°")
            |> size 5
            |> filled black
            |> move ( 56, -46 )
        , text ("Minor axis: " ++ String.fromInt (round <| model.currentRadius))
            |> size 5
            |> filled black
            |> move ( 56, -52 )

        --, text (Debug.toString model) |> centered |> size 1 |> filled black
        ]
