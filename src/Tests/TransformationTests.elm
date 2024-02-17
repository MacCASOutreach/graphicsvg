module Tests.TransformationTests exposing(..)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (appWithTick, AppWithTick, GetKeyState)
import GraphicSVG.Secret exposing(Shape(..))
import Url exposing (Url)
import Browser exposing (UrlRequest)
import Array
import Dict

-- This example must be viewed by elm reactor or another web server, not directly after using elm-make

type alias Model = 
    { test : Int
    , time : Float
    , successful : Dict.Dict Int Bool
    }

type Msg = 
      Tick Float GetKeyState
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | CorrectTest
    | IncorrectTest

main : AppWithTick () Model Msg
main = appWithTick Tick
    { init = \_ url key -> (init, Cmd.none)
    , update = update
    , view = \model -> { body = view model, title = title model }
    , subscriptions = \_ -> Sub.none
    , onUrlRequest = OnUrlRequest
    , onUrlChange = OnUrlChange
    }

clock = group [(group <| List.map (\n -> text (String.fromInt n) |> fixedwidth 
                |> size 6
                |> centered
                |> filled black 
                |> rotate (degrees (-360*toFloat n/12)) 
                |> move (0,25) 
                |> rotate (degrees (360*toFloat n/12))) (List.range 1 12)) |> move (0,-2)
                  , circle 30 |> outlined (solid 1) black    ]

init : Model
init =
        {
            time = 0
        ,   test = 0
        ,   successful = Dict.empty
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        Tick t _ -> ( { model | time = t }, Cmd.none )
        OnUrlRequest _ -> (model, Cmd.none) 
        OnUrlChange url -> (model, Cmd.none)
        CorrectTest -> ( { model | successful = Dict.insert model.test True model.successful
                                 , test = clamp 0 (Array.length tests) (model.test+1) 
                                 } , Cmd.none)
        IncorrectTest -> ( { model | successful = Dict.insert model.test False model.successful
                                 , test = clamp 0 (Array.length tests) (model.test+1) 
                                 } , Cmd.none)

title : Model -> String
title model =
    "Transformation Tests"

tests = Array.fromList
    [
        ("rect 40 20 |> filled brown |> scaleX 2",rect 40 20 |> filled brown |> scaleX 2),
        ("rect 40 20 |> filled brown |> scaleY 2",rect 40 20 |> filled brown |> scaleY 2),
        ("rect 40 20 |> filled brown |> scale 2",rect 40 20 |> filled brown |> scale 2),
        ("rect 40 20 |> filled brown |> rotate (degrees 30)",rect 40 20 |> filled brown |> rotate (degrees 30)),
        ("rect 40 20 |> filled brown |> move (30,-30)",rect 40 20 |> filled brown |> move (30,-30)),
        ("rect 40 20 |> filled brown |> skewX (degrees 30)",rect 40 20 |> filled brown |> skewX (degrees 30)),
        ("rect 40 20 |> filled brown |> skewY (degrees 30)",rect 40 20 |> filled brown |> skewY (degrees 30)),
        ("rect 40 20 |> filled brown |> scaleX 2 |> scaleX 2",rect 40 20 |> filled brown |> scaleX 2 |> scaleX 2),
        ("rect 40 20 |> filled brown |> scaleX 2 |> scaleY 2",rect 40 20 |> filled brown |> scaleX 2 |> scaleY 2),
        ("rect 40 20 |> filled brown |> scaleX 2 |> scale 2",rect 40 20 |> filled brown |> scaleX 2 |> scale 2),
        ("rect 40 20 |> filled brown |> scaleX 2 |> rotate (degrees 30)",rect 40 20 |> filled brown |> scaleX 2 |> rotate (degrees 30)),
        ("rect 40 20 |> filled brown |> scaleX 2 |> move (30,-30)",rect 40 20 |> filled brown |> scaleX 2 |> move (30,-30)),
        ("rect 40 20 |> filled brown |> scaleX 2 |> skewX (degrees 30)",rect 40 20 |> filled brown |> scaleX 2 |> skewX (degrees 30)),
        ("rect 40 20 |> filled brown |> scaleX 2 |> skewY (degrees 30)",rect 40 20 |> filled brown |> scaleX 2 |> skewY (degrees 30)),
        ("rect 40 20 |> filled brown |> scaleY 2 |> scaleX 2",rect 40 20 |> filled brown |> scaleY 2 |> scaleX 2),
        ("rect 40 20 |> filled brown |> scaleY 2 |> scaleY 2",rect 40 20 |> filled brown |> scaleY 2 |> scaleY 2),
        ("rect 40 20 |> filled brown |> scaleY 2 |> scale 2",rect 40 20 |> filled brown |> scaleY 2 |> scale 2),
        ("rect 40 20 |> filled brown |> scaleY 2 |> rotate (degrees 30)",rect 40 20 |> filled brown |> scaleY 2 |> rotate (degrees 30)),
        ("rect 40 20 |> filled brown |> scaleY 2 |> move (30,-30)",rect 40 20 |> filled brown |> scaleY 2 |> move (30,-30)),
        ("rect 40 20 |> filled brown |> scaleY 2 |> skewX (degrees 30)",rect 40 20 |> filled brown |> scaleY 2 |> skewX (degrees 30)),
        ("rect 40 20 |> filled brown |> scaleY 2 |> skewY (degrees 30)",rect 40 20 |> filled brown |> scaleY 2 |> skewY (degrees 30)),
        ("rect 40 20 |> filled brown |> scale 2 |> scaleX 2",rect 40 20 |> filled brown |> scale 2 |> scaleX 2),
        ("rect 40 20 |> filled brown |> scale 2 |> scaleY 2",rect 40 20 |> filled brown |> scale 2 |> scaleY 2),
        ("rect 40 20 |> filled brown |> scale 2 |> scale 2",rect 40 20 |> filled brown |> scale 2 |> scale 2),
        ("rect 40 20 |> filled brown |> scale 2 |> rotate (degrees 30)",rect 40 20 |> filled brown |> scale 2 |> rotate (degrees 30)),
        ("rect 40 20 |> filled brown |> scale 2 |> move (30,-30)",rect 40 20 |> filled brown |> scale 2 |> move (30,-30)),
        ("rect 40 20 |> filled brown |> scale 2 |> skewX (degrees 30)",rect 40 20 |> filled brown |> scale 2 |> skewX (degrees 30)),
        ("rect 40 20 |> filled brown |> scale 2 |> skewY (degrees 30)",rect 40 20 |> filled brown |> scale 2 |> skewY (degrees 30)),
        ("rect 40 20 |> filled brown |> rotate (degrees 30) |> scaleX 2",rect 40 20 |> filled brown |> rotate (degrees 30) |> scaleX 2),
        ("rect 40 20 |> filled brown |> rotate (degrees 30) |> scaleY 2",rect 40 20 |> filled brown |> rotate (degrees 30) |> scaleY 2),
        ("rect 40 20 |> filled brown |> rotate (degrees 30) |> scale 2",rect 40 20 |> filled brown |> rotate (degrees 30) |> scale 2),
        ("rect 40 20 |> filled brown |> rotate (degrees 30) |> rotate (degrees 30)",rect 40 20 |> filled brown |> rotate (degrees 30) |> rotate (degrees 30)),
        ("rect 40 20 |> filled brown |> rotate (degrees 30) |> move (30,-30)",rect 40 20 |> filled brown |> rotate (degrees 30) |> move (30,-30)),
        ("rect 40 20 |> filled brown |> rotate (degrees 30) |> skewX (degrees 30)",rect 40 20 |> filled brown |> rotate (degrees 30) |> skewX (degrees 30)),
        ("rect 40 20 |> filled brown |> rotate (degrees 30) |> skewY (degrees 30)",rect 40 20 |> filled brown |> rotate (degrees 30) |> skewY (degrees 30)),
        ("rect 40 20 |> filled brown |> move (30,-30) |> scaleX 2",rect 40 20 |> filled brown |> move (30,-30) |> scaleX 2),
        ("rect 40 20 |> filled brown |> move (30,-30) |> scaleY 2",rect 40 20 |> filled brown |> move (30,-30) |> scaleY 2),
        ("rect 40 20 |> filled brown |> move (30,-30) |> scale 2",rect 40 20 |> filled brown |> move (30,-30) |> scale 2),
        ("rect 40 20 |> filled brown |> move (30,-30) |> rotate (degrees 30)",rect 40 20 |> filled brown |> move (30,-30) |> rotate (degrees 30)),
        ("rect 40 20 |> filled brown |> move (30,-30) |> move (30,-30)",rect 40 20 |> filled brown |> move (30,-30) |> move (30,-30)),
        ("rect 40 20 |> filled brown |> move (30,-30) |> skewX (degrees 30)",rect 40 20 |> filled brown |> move (30,-30) |> skewX (degrees 30)),
        ("rect 40 20 |> filled brown |> move (30,-30) |> skewY (degrees 30)",rect 40 20 |> filled brown |> move (30,-30) |> skewY (degrees 30)),
        ("rect 40 20 |> filled brown |> skewX (degrees 30) |> scaleX 2",rect 40 20 |> filled brown |> skewX (degrees 30) |> scaleX 2),
        ("rect 40 20 |> filled brown |> skewX (degrees 30) |> scaleY 2",rect 40 20 |> filled brown |> skewX (degrees 30) |> scaleY 2),
        ("rect 40 20 |> filled brown |> skewX (degrees 30) |> scale 2",rect 40 20 |> filled brown |> skewX (degrees 30) |> scale 2),
        ("rect 40 20 |> filled brown |> skewX (degrees 30) |> rotate (degrees 30)",rect 40 20 |> filled brown |> skewX (degrees 30) |> rotate (degrees 30)),
        ("rect 40 20 |> filled brown |> skewX (degrees 30) |> move (30,-30)",rect 40 20 |> filled brown |> skewX (degrees 30) |> move (30,-30)),
        ("rect 40 20 |> filled brown |> skewX (degrees 30) |> skewX (degrees 30)",rect 40 20 |> filled brown |> skewX (degrees 30) |> skewX (degrees 30)),
        ("rect 40 20 |> filled brown |> skewX (degrees 30) |> skewY (degrees 30)",rect 40 20 |> filled brown |> skewX (degrees 30) |> skewY (degrees 30)),
        ("rect 40 20 |> filled brown |> skewY (degrees 30) |> scaleX 2",rect 40 20 |> filled brown |> skewY (degrees 30) |> scaleX 2),
        ("rect 40 20 |> filled brown |> skewY (degrees 30) |> scaleY 2",rect 40 20 |> filled brown |> skewY (degrees 30) |> scaleY 2),
        ("rect 40 20 |> filled brown |> skewY (degrees 30) |> scale 2",rect 40 20 |> filled brown |> skewY (degrees 30) |> scale 2),
        ("rect 40 20 |> filled brown |> skewY (degrees 30) |> rotate (degrees 30)",rect 40 20 |> filled brown |> skewY (degrees 30) |> rotate (degrees 30)),
        ("rect 40 20 |> filled brown |> skewY (degrees 30) |> move (30,-30)",rect 40 20 |> filled brown |> skewY (degrees 30) |> move (30,-30)),
        ("rect 40 20 |> filled brown |> skewY (degrees 30) |> skewX (degrees 30)",rect 40 20 |> filled brown |> skewY (degrees 30) |> skewX (degrees 30)),
        ("rect 40 20 |> filled brown |> skewY (degrees 30) |> skewY (degrees 30)",rect 40 20 |> filled brown |> skewY (degrees 30) |> skewY (degrees 30))
    ]

view : Model -> Collage Msg
view model = 
    let
        (name,currentTest) = 
            case Array.get model.test tests of
                Just t -> t
                Nothing -> ("",group [])
            
    in
            
    collage 192 128 
    (if model.test < Array.length tests then [
      graphPaper 10 
    , rect 192 40 |> filled white |> move (0,70)
    , text name |> size 4 |> centered |> fixedwidth |> filled black |> move(0,55)
    , currentTest
    , roundedRect 20 10 5 |> filled green |> notifyTap CorrectTest |> move(10,-30)
    , roundedRect 20 10 5 |> filled red |> notifyTap IncorrectTest |> move(-10,-30)
    ] else [renderSuccesses model.successful]
        )

renderSuccesses : Dict.Dict Int Bool -> Shape Msg
renderSuccesses successDict =
    group
        (List.map (\(n,s) -> 
            let
                test = case Array.get n tests of 
                        Just (name, sh) -> group [text name 
                                            |> size 2 
                                            |> fixedwidth 
                                            |> filled (if s then green else red) 
                                            |> move (-60,toFloat n * -3 + 60)
                                            ]
                        Nothing -> group [] 
            in
                test
            ) <| Dict.toList successDict)


xIcon = group 
            [
                rect 4 2 |> filled red |> rotate (degrees 45)
            ,   rect 2 4 |> filled red |> rotate (degrees 45)
            ]

checkIcon = group 
            [
                circle 2 |> filled green
            ]

{-testScaleX = square 20 |> filled (hsl 0.5 0.5 0.5) |> scaleX 2
testScaleXScaleX = square 20 |> filled (hsl 0.5 0.5 0.5) |> scaleX 2 |> scaleX 1.5
testScaleXScaleY = square 20 |> filled (hsl 0.5 0.5 0.5) |> scaleX 2 |> scaleY 1.5
-}
