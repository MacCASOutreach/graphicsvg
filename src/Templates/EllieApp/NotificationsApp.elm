import GraphicSVG exposing (..)
import GraphicSVG.App exposing (notificationsApp, NotificationsApp, GetKeyState)

type alias Model = 
    { radius : Float }

type Msg = 
      MakeBigger
    | MakeSmaller

main : NotificationsApp Model Msg
main = notificationsApp 
    { model = init
    , update = update
    , view = view 
    }

init : Model
init = 
    {
        radius = 10
    }

update : Msg -> Model -> Model
update msg model =
    case msg of 
        MakeBigger -> { model | radius = model.radius + 3 }
        MakeSmaller -> { model | radius = model.radius - 3 }

view : Model -> Collage Msg
view model = collage 192 128 
    [ circle model.radius |> filled lightBlue --model.radius |> filled lightBlue
    , biggerButton 
        |> move (40,10)
        |> notifyTap MakeBigger
    , smallerButton
        |> move (40,-10)
        |> notifyTap MakeSmaller
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