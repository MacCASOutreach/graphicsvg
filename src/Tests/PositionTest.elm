import GraphicSVG exposing (..)
import GraphicSVG.App exposing (notificationsApp, NotificationsApp, GetKeyState)

type alias Model = 
    { pos : (Float, Float) }

type Msg = 
      GoTo (Float,Float)

main : NotificationsApp Model Msg
main = notificationsApp 
    { model = init
    , update = update
    , view = view 
    }

init : Model
init = 
    {
        pos = (0,0)
    }

update : Msg -> Model -> Model
update msg model =
    case msg of 
        GoTo pos -> { model | pos = pos }

view : Model -> Collage Msg
view model = collage 192 128 
    [ rect 192 128 |> filled green
                    |> notifyMouseMoveAt GoTo
    , circle 1 |> filled red |> move model.pos
                |> notifyMouseMoveAt GoTo
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