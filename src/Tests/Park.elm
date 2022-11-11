module Tests.Park exposing(..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (ellieAppWithTick, EllieAppWithTick, GetKeyState)

type alias Model =
    { radius : Float
    , time : Float
    }

type Msg =
      Tick Float GetKeyState

main : EllieAppWithTick () Model Msg
main = ellieAppWithTick Tick
    { init = \_ -> (init, Cmd.none)
    , update = update
    , view = \model -> { body = view model, title = title model }
    , subscriptions = \_ -> Sub.none
    }

init : Model
init =
    {
        radius = 10
    ,   time = 0
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick t (_,(_,y),_) -> ( { model | time = t, radius = model.radius + y }, Cmd.none)

title : Model -> String
title model =
    "Ellie App with Tick Example"

view model =
    let
        saturation = 1
        lightness = 0.7
    in
    collage 192 128
    [ rect 192 128
              |> filled
                    (gradient
                      [stop (rgb 224 255 255) 0
                      ,stop (rgb 0 191 255) 128
                      ]
                      |> rotateGradient (degrees 90)

                      )
              |> move (0,32)
    , rect 192 128
              |> filled
                    (gradient
                        [stop lightGreen 0
                        ,stop darkGreen 128
                        ]
                        |> rotateGradient (degrees 90))
              |> move (0,-96)
    , circle 50
            |> filled
                (radialGradient
                    [ stop orange 0
                    , stop lightOrange 12.5
                    , stop yellow 25
                    , transparentStop yellow 50 0
                    ])
            |> scale 0.5
            |> move (50, 30)
    ]