module Main exposing (Model, Msg(..), init, keys, main, update, view)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (GameApp, GetKeyState, KeyState(..), Keys(..), gameApp)


type alias Model =
    { time : Float
    , change : Bool
    , arrows : ( Float, Float )
    , wasd : ( Float, Float )
    }


type Msg
    = Tick Float GetKeyState


main : GameApp Model Msg
main =
    gameApp Tick
        { model = init
        , update = update
        , view = view
        }


init : Model
init =
    { time = 0
    , change = False
    , arrows = ( 0, 0 )
    , wasd = ( 0, 0 )
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick t ( keyf, ( x0, y0 ), ( x1, y1 ) ) ->
            { model
                | time = t
                , change = keyf (Key "c") == Down
                , arrows = ( x0, y0 )
                , wasd = ( x1, y1 )
            }


view : Model -> Collage Msg
view model =
    collage 192
        128
        [ text "Hold the \"c\" key down to change the colour:"
            |> size 7
            |> centered
            |> outlined (solid 0.25) black
            |> move ( 0, 55 )
        , circle (25 * sin (3 * model.time) + 25)
            |> filled
                (if model.change then
                    green

                 else
                    orange
                )
        , square 20
            |> outlined (dashed 2) hotPink
            |> rotate (degrees (10 * model.time))
        , keys model.arrows True |> move ( 40, -50 )
        , keys model.wasd False |> move ( -40, -50 )
        ]


keys : ( Float, Float ) -> Bool -> Shape Msg
keys ( x, y ) arrows =
    group
        [ roundedRect 12 12 2
            |> filled
                (if y < 0 then
                    rgb 200 200 200

                 else
                    rgb 100 100 100
                )
        , if arrows then
            triangle 4 |> filled white |> rotate (degrees 30) |> move ( 0, 0.5 )

          else
            text "S" |> centered |> filled white |> move ( 0, -4 )
        , roundedRect 12 12 2
            |> filled
                (if x < 0 then
                    rgb 200 200 200

                 else
                    rgb 100 100 100
                )
            |> move ( -13, 0 )
        , if arrows then
            triangle 4 |> filled white |> rotate (degrees -60) |> move ( -12.5, 0 )

          else
            text "A" |> centered |> filled white |> move ( -13, -4 )
        , roundedRect 12 12 2
            |> filled
                (if x > 0 then
                    rgb 200 200 200

                 else
                    rgb 100 100 100
                )
            |> move ( 13, 0 )
        , if arrows then
            triangle 4 |> filled white |> move ( 12.5, 0 )

          else
            text "D" |> centered |> filled white |> move ( 13, -4 )
        , roundedRect 12 12 2
            |> filled
                (if y > 0 then
                    rgb 200 200 200

                 else
                    rgb 100 100 100
                )
            |> move ( 0, 13 )
        , if arrows then
            triangle 4 |> filled white |> rotate (degrees -30) |> move ( 0, 12.5 )

          else
            text "W" |> centered |> filled white |> move ( 0, 9 )
        ]
