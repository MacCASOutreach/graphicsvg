module Tests.GradientTest exposing(..)

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
    collage 192 168
    [ rect 192 128
        |> filled blue
        |> move (-192/2,0)
    ,  circle 50
        |> filled
            (radialGradient 50
                [ stop (hsla (degrees <| model.time*100) saturation lightness 0.5) 0
                , stop (hsl (degrees <| model.time*100 + 60) saturation lightness) 10
                , stop (hsl (degrees <| model.time*100 + 120) saturation lightness) 20
                , stop (hsl (degrees <| model.time*100 + 180) saturation lightness) 30
                , stop (hsl (degrees <| model.time*100 + 240) saturation lightness) 40
                , stop (hsl (degrees <| model.time*100 + 300) saturation lightness) 50
                ])
    ]