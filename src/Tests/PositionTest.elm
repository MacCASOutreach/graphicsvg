import GraphicSVG exposing (..)
import GraphicSVG.App exposing (notificationsApp, NotificationsApp, GetKeyState)

type alias Model =
    (Float, Float)

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
    ( 0, 0 )

update : Msg -> Model -> Model
update msg model =
    case msg of 
        GoTo pos -> pos

view : Model -> Collage Msg
view model = collage 192 128 
    [ rect 192 128 |> filled green
        |> notifyMouseMoveAt GoTo
    , circle 1 |> filled red |> move model
        |> notifyMouseMoveAt GoTo
    ]