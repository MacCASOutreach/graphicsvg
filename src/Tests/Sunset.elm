module Tests.Sunset exposing(..)

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
    [ rect 192 188
              |> filled
                    (radialGradient 128
                      [stop orange 0
                      ,stop red 32
                      ,stop pink 64
                      ,stop purple 96
                      ,stop black 128
                      ]
                      |> rotateGradient (degrees 90)
                     )
              |> move (0,-30)
    , circle 50
            |> filled
                (radialGradient 50
                    [ stop orange 0
                    , stop lightOrange 12.5
                    , stop yellow 25
                    , transparentStop yellow 50 0
                    ])
            |> scale 1
            |> move (0, -30)
    , rect 192 128
                    |> filled
                          (gradient 128
                              [stop (rgb 30 144 255) 96
                              ,transparentStop lightPurple 128 1
                              ]
                              |> rotateGradient (degrees 90))
                    |> move (0,-96)
    ]