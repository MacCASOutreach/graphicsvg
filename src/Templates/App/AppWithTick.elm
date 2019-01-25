import GraphicSVG exposing (..)
import GraphicSVG.App exposing (appWithTick, AppWithTick, GetKeyState)
import Url exposing (Url)
import Browser exposing (UrlRequest)
import Browser.Events
import Debug exposing (toString)

-- This example must be viewed by elm reactor or another web server, not directly after using elm-make

type alias Model = 
    { radius : Float
    , time : Float
    , x : Int
    , y : Int
    }

type Msg = 
      Tick Float GetKeyState
    | MakeBigger
    | MakeSmaller
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | OnResize Int Int

main : AppWithTick () Model Msg
main = appWithTick Tick
    { init = \_ url key -> (init, Cmd.none)
    , update = update
    , view = \model -> { body = view model, title = title model }
    , subscriptions = \_ -> Browser.Events.onResize OnResize
    , onUrlRequest = OnUrlRequest
    , onUrlChange = OnUrlChange
    }

init : Model
init =
        {
            radius = 10
        ,   time = 0
        ,   x = 0
        ,   y = 0
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        Tick t (_,(_,y),_) -> ( { model | time = t, radius = model.radius + y }, Cmd.none )
        MakeBigger ->  ( { model | radius = model.radius + 3 }, Cmd.none )
        MakeSmaller -> ( { model | radius = model.radius - 3 }, Cmd.none )
        OnUrlRequest _ -> (model, Cmd.none) 
        OnUrlChange url -> (model, Cmd.none)
        OnResize x y -> ( { model | x = x, y = y }, Cmd.none)

title : Model -> String
title model =
    "App with Tick Example"

view : Model -> Collage Msg
view model = collage 192 128 
    [ circle model.radius |> filled (hsl model.time 0.5 0.5)
    , biggerButton 
        |> move (40,10)
        |> notifyTap MakeBigger
    , smallerButton
        |> move (40,-10)
        |> notifyTap MakeSmaller
    , text "Try using the up and down arrows!"
        |> centered
        |> filled gray
        |> move (0,-50)
    , text (toString model.x ++ "," ++ toString model.y) |> filled black
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