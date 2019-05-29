module Main exposing (Model, Msg(..), biggerButton, init, main, smallerButton, title, update, view)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (EllieAppWithTick, GetKeyState, ellieAppWithTick)


type alias Model =
    { radius : Float
    , time : Float
    }


type Msg
    = Tick Float GetKeyState
    | MakeBigger
    | MakeSmaller


main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = \model -> { body = view model, title = title model }
        , subscriptions = \_ -> Sub.none
        }


init : Model
init =
    { radius = 10
    , time = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ( _, ( _, y ), _ ) ->
            ( { model | time = t, radius = model.radius + y }, Cmd.none )

        MakeBigger ->
            ( { model | radius = model.radius + 3 }, Cmd.none )

        MakeSmaller ->
            ( { model | radius = model.radius - 3 }, Cmd.none )


title : Model -> String
title model =
    "Ellie App with Tick Example"


view : Model -> Collage Msg
view model =
    collage 192
        128
        [ circle model.radius |> filled (hsl model.time 0.5 0.5)
        , biggerButton
            |> move ( 40, 10 )
            |> notifyTap MakeBigger
        , smallerButton
            |> move ( 40, -10 )
            |> notifyTap MakeSmaller
        , text "Try using the up and down arrows!"
            |> centered
            |> filled gray
            |> move ( 0, -50 )
        ]


biggerButton =
    group
        [ circle 5 |> filled darkGrey
        , rect 8 1 |> filled white
        , rect 1 8 |> filled white
        ]


smallerButton =
    group
        [ circle 5 |> filled darkGrey
        , rect 8 1 |> filled white
        ]
