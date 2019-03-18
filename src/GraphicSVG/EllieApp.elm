module GraphicSVG.EllieApp exposing
    ( graphicsApp, GraphicsApp
    , notificationsApp, NotificationsApp
    , gameApp, GameApp
    , ellieAppWithTick, EllieAppWithTick
    , InputHandler, GetKeyState, Keys(..), KeyState(..)
    )

{-| The methods in this library are analogous to those in GraphicSVG.App; however,
they are built upon `GraphicSVG.ellieApp` for compatibility with that platform.


# Apps

@docs graphicsApp, GraphicsApp, notificationsApp, NotificationsApp, gameApp, GameApp


# Time and Keyboard interactions

@docs InputHandler, GetKeyState, Keys, KeyState


# Barebones ellieApp with keyboard and time

@docs EllieAppWithTick, ellieAppWithTick

-}

import Browser exposing (UrlRequest(..))
import Browser.Events exposing (onKeyDown, onKeyUp, onAnimationFrame)
import Browser.Navigation as Nav
import Dict
import GraphicSVG exposing (..)
import Json.Decode as D
import Task
import Time exposing (millisToPosix, posixToMillis)
import Url


{-| The `InputHandler` type alias descripts a message that contains a Float representing the time in seconds from
the time the program started and the `GetKeyState` type alias used for returning key actions.
This type is used for by the `gameApp` fully interactive application.
-}
type alias InputHandler userMsg =
    Float
    -> GetKeyState
    -> userMsg


{-| `GetKeyState` returns a triple where the first argument is of type `Keys -> KeyState`
so you can ask if a certain key is pressed. The other two are tuples of arrow keys and
WASD keys, respectively. They're in the form (x,y) which represents the key presses
of each player. For example, (0,-1) represents the left arrow (or "A") key, and (1,1)
would mean the up (or "W") and right (or "D") key are being pressed at the same time.
-}
type alias GetKeyState =
    ( Keys -> KeyState, ( Float, Float ), ( Float, Float ) )


type alias KeyCode =
    Int


type alias KeyDict =
    Dict.Dict KeyCode ( KeyState, Bool )


{-| The possible states when you ask for a key's state:

  - `JustDown` is the frame after the key went down (will show up exactly once per press)
  - `Down` is a press that is continuing for more than one frame
  - `JustUp` is the frame after the key went up / stopped being pressed (will show up exactly once per press)
  - `Up` means the key is not currently being pressed nor was it recently released

-}
type KeyState
    = JustDown
    | Down
    | JustUp
    | Up


type KeyAction
    = WentUp
    | WentDown


subs : List (Sub (HiddenMsg userMsg))
subs =
    [ onKeyUp (D.map KeyUp (D.field "keyCode" D.int))
    , onKeyDown (D.map KeyDown (D.field "keyCode" D.int))
    , onAnimationFrame TickTime
    ]


insertKeyDict : KeyDict -> KeyCode -> KeyAction -> KeyDict
insertKeyDict dict key action =
    let
        currState =
            Dict.get key dict
    in
    case currState of
        Just ( JustDown, False ) ->
            Dict.insert key
                (case action of
                    WentDown ->
                        ( JustDown, False )

                    WentUp ->
                        ( JustDown, True )
                )
                dict

        Just ( Down, False ) ->
            Dict.insert key
                (case action of
                    WentDown ->
                        ( Down, False )

                    WentUp ->
                        ( JustUp, False )
                )
                dict

        Just ( Up, False ) ->
            Dict.insert key
                (case action of
                    WentDown ->
                        ( JustDown, False )

                    WentUp ->
                        ( JustUp, False )
                )
                dict

        Just ( JustUp, False ) ->
            Dict.insert key
                (case action of
                    WentDown ->
                        ( JustUp, True )

                    WentUp ->
                        ( JustUp, False )
                )
                dict

        Just ( state, True ) ->
            Dict.insert key
                (case action of
                    WentDown ->
                        ( state, True )

                    WentUp ->
                        ( state, True )
                )
                dict

        Nothing ->
            Dict.insert key
                (case action of
                    WentDown ->
                        ( JustDown, False )

                    WentUp ->
                        ( JustUp, False )
                )
                dict


maintainKeyDict : KeyDict -> KeyDict
maintainKeyDict dict =
    Dict.filter filterHelper (Dict.map maintainHelper dict)


filterHelper : a -> ( KeyState, b ) -> Bool
filterHelper key action =
    case action of
        ( Up, _ ) ->
            False

        _ ->
            True


maintainHelper : a -> ( KeyState, Bool ) -> ( KeyState, Bool )
maintainHelper key action =
    case action of
        ( JustUp, False ) ->
            ( Up, False )

        ( JustUp, True ) ->
            ( JustDown, False )

        ( Up, False ) ->
            ( Up, False )

        ( Up, True ) ->
            ( Up, False )

        ( JustDown, False ) ->
            ( Down, False )

        ( JustDown, True ) ->
            ( JustUp, False )

        ( Down, False ) ->
            ( Down, False )

        ( Down, True ) ->
            ( Down, False )


{-| Includes all the regular keys. Ask for letters and numbers using `Key String`, e.g. `Key "a"` or `Key "3"`.
-}
type Keys
    = Key String
    | Backspace
    | Tab
    | Enter
    | Shift
    | Ctrl
    | Alt
    | Caps
    | LeftArrow
    | UpArrow
    | RightArrow
    | DownArrow
    | Delete
    | Space


keyCheckerFunction : Dict.Dict Int ( KeyState, a ) -> Keys -> KeyState
keyCheckerFunction dict key =
    let
        state =
            Dict.get kc dict

        kc =
            case key of
                Key str ->
                    Char.toCode <|
                        Char.toUpper <|
                            case String.uncons str of
                                Just ( a, _ ) ->
                                    a

                                Nothing ->
                                    'z'

                Backspace ->
                    8

                Tab ->
                    9

                Enter ->
                    13

                Shift ->
                    16

                Ctrl ->
                    17

                Alt ->
                    18

                Caps ->
                    20

                Space ->
                    32

                LeftArrow ->
                    37

                UpArrow ->
                    38

                RightArrow ->
                    39

                DownArrow ->
                    40

                Delete ->
                    46
    in
    case state of
        Just ( JustDown, _ ) ->
            JustDown

        Just ( Down, _ ) ->
            Down

        Just ( JustUp, _ ) ->
            JustUp

        Just ( Up, _ ) ->
            Up

        Nothing ->
            Up


arrowChecker :
    (Keys -> KeyState)
    -> Keys
    -> Keys
    -> Keys
    -> Keys
    -> ( number, number )
arrowChecker checker up down left right =
    ( case ( checker left, checker right ) of
        ( Down, Up ) ->
            -1

        ( Down, JustUp ) ->
            -1

        ( JustDown, Up ) ->
            -1

        ( JustDown, JustUp ) ->
            -1

        ( Up, Down ) ->
            1

        ( JustUp, Down ) ->
            1

        ( Up, JustDown ) ->
            1

        ( JustUp, JustDown ) ->
            1

        _ ->
            0
    , case ( checker down, checker up ) of
        ( Down, Up ) ->
            -1

        ( Down, JustUp ) ->
            -1

        ( JustDown, Up ) ->
            -1

        ( JustDown, JustUp ) ->
            -1

        ( Up, Down ) ->
            1

        ( JustUp, Down ) ->
            1

        ( Up, JustDown ) ->
            1

        ( JustUp, JustDown ) ->
            1

        _ ->
            0
    )


subtractTimeSeconds : Time.Posix -> Time.Posix -> Float
subtractTimeSeconds t1 t0 =
    ((Basics.toFloat <| posixToMillis t1) - Basics.toFloat (posixToMillis t0)) / 1000


type HiddenMsg userMsg
    = UserMsg userMsg
    | InitTime Time.Posix
    | TickTime Time.Posix
    | KeyDown Int
    | KeyUp Int


{-| This type alias is only used as a target for a user `main` type signature
to make the type signature more clear and concise when `main` calls
`graphicsApp`:
main : GraphicsApp userMsg
main =
graphicsApp { view = view }
Note that `userMsg` can be anything as no messages are used in this type of program.
-}
type alias GraphicsApp =
    EllieApp () () ()


{-| The simplest way to render graphics to the screen. These graphics will be
static (they don't move) and cannot be interacted with. This is great for beginners
or for when only need static graphics are needed. Note that your `view` function is bare,
with no parameters:
view =
collage 500 500
[ circle 10 |> filled red
][ circle 10 |> filled red
]
`graphicsApp` takes a parameter like `{ view = view }`
so the main program that would get the whole thing started for the above
`view` would be:
main =
graphicsApp { view = view }
-}
graphicsApp :
    { view : Collage ()
    }
    -> GraphicsApp
graphicsApp userApp =
    GraphicSVG.ellieApp
        { init = \_ -> ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , view = \_ -> { title = "GraphicSVG Graphics App", body = userApp.view }
        , subscriptions = \_ -> Sub.none
        }


{-| This type alias is only used as a target for a user `main` type signature
to make the type signature more clear and concise when `main` calls
`notificationsApp`:
main : NotificationsApp Model MyMsg
main =
notificationsApp { model = init, update = update, view = view }
where `Model` is the type alias of the user persistent model, and
`MyMsg` is the name of the user defined message type;
if other names are used, they can be substituted for these names.
-}
type alias NotificationsApp userModel userMsg =
    EllieApp () userModel (HiddenMsg userMsg)


{-| Like `graphicsApp`, but you can add interactivity to your graphics by using the
`notify*` functions. This allows you to learn Elm's architecture in a fun way with
graphics. Note that your `view` function needs a `model` parameter now, which in this
example is the colour of the shape:
view model =
    collage 500 500
        [ circle 10 |> filled model |> notifyTap Change
        ]
`notificationsApp` takes a parameter like:
{
    model = model
,   view = view
,   update = update
}
so the functions that would be required to make the above `view` function work
are as follows:
type Msg = Change
update msg model =
    case msg of
        Change ->
            green
main = notificationsApp
{   
    model = red -- causes circle to start red
,   update = update -- function which changes the model
,   view = view
}
which will cause the drawn red circle to change to green the first time
it is mouse clicked or tapped.
-}
notificationsApp :
    { model : userModel
    , view : userModel -> Collage userMsg
    , update : userMsg -> userModel -> userModel
    }
    -> NotificationsApp userModel userMsg
notificationsApp userApp =
    let
        userInit =
            userApp.model

        userUpdate =
            userApp.update

        userView =
            userApp.view
    in
    GraphicSVG.ellieApp
        { init = \_ -> ( userInit, Cmd.none )
        , update = hiddenNotifUpdate userUpdate
        , view = \userModel -> { title = "GraphicSVG Interactive App", body = mapCollage UserMsg <| userView userModel }
        , subscriptions = \_ -> Sub.none
        }


hiddenNotifUpdate :
    (userMsg -> userModel -> userModel)
    -> HiddenMsg userMsg
    -> userModel
    -> ( userModel, Cmd (HiddenMsg userMsg) )
hiddenNotifUpdate userUpdate msg userModel =
    case msg of
        UserMsg userMsg ->
            ( userUpdate userMsg userModel, Cmd.none )

        _ ->
            ( userModel, Cmd.none )


type alias HiddenModel userMsg =
    { tick : InputHandler userMsg
    , keys : KeyDict
    , initT : Time.Posix
    }


{-| This type alias is only used as a target for a user `main` type signature to make
the type signature more clear and concise when `main` calls `gameApp`:
```
main : GameApp Model Msg
main =
    gameApp Tick
        { model = init
        , update = update
        , view = view
        }
```
where `Tick` is the message handler called once per browser window update,
`Model` is the type alias of the user persistent model, and
`Msg` is the name of the user message type; if other names are used,
they can be substituted for these names.
-}
type alias GameApp userModel userMsg =
    EllieApp () ( userModel, HiddenModel userMsg ) (HiddenMsg userMsg)


{-| Automatically maps time and keyboard presses to your program. This should
be all you need for making complex interactive games and animations.
`gameApp` takes two parameters: one is your own type of `InputHandler` message
which will be automatically called each time the browser window is refreshed
(30 times per second)
of the form `Float -> GetKeyState -> UserMsg` and the other is
```
{
  model = model
, view = view
, update = update
}
````
The following program causes animation of the drawn line,
causing it to spin around; also, a press of the "r" key
causes the direction of the spin to reverse:
```
type Msg
    = Tick Float GetKeyState

type alias Model = { angle : Float, speed : Float }

init = { angle = 0, speed = 1 }

update msg model =
    case msg of
        Tick _ ( keys, _, _ ) ->
            case keys (Key "r") of
                JustDown ->
                    { model
                    | angle = model.angle - model.speed
                    , speed = -model.speed
                    }
            _ -> { model | angle = model.angle + model.speed }

view model =
    collage 500 500
        [ line ( 0, 0 ) ( 250, 0 )
            |> outlined (solid 1) green
            |> rotate (degrees model.angle)
        ]

main =
    gameApp Tick
        { model = init
        , update = update
        , view = view
        }
```
-}
gameApp :
    InputHandler userMsg
    ->
        { model : userModel
        , view : userModel -> Collage userMsg
        , update : userMsg -> userModel -> userModel
        , title : String
        }
    -> GameApp userModel userMsg
gameApp tickMsg userApp =
    let
        userInit =
            userApp.model

        userUpdate =
            userApp.update

        userView =
            userApp.view
    in
    GraphicSVG.ellieApp
        { init = \_ -> ( ( userInit, initHiddenModel tickMsg ), Task.perform InitTime Time.now )
        , update = hiddenGameUpdate userUpdate
        , view = \( userModel, _ ) -> { title = userApp.title, body = mapCollage UserMsg <| userView userModel }
        , subscriptions = \_ -> Sub.batch subs
        }


initHiddenModel : InputHandler userMsg -> HiddenModel userMsg
initHiddenModel tick =
    { tick = tick
    , keys = Dict.empty
    , initT = millisToPosix 0
    }


hiddenGameUpdate :
    (userMsg -> userModel -> userModel)
    -> HiddenMsg userMsg
    -> ( userModel, HiddenModel userMsg )
    -> ( ( userModel, HiddenModel userMsg ), Cmd (HiddenMsg userMsg) )
hiddenGameUpdate userUpdate msg ( userModel, hiddenModel ) =
    let
        updateTick =
            hiddenModel.tick
    in
    case msg of
        UserMsg userMsg ->
            ( ( userUpdate userMsg userModel, hiddenModel ), Cmd.none )

        InitTime t ->
            ( ( userModel, { hiddenModel | initT = t } ), Cmd.none )

        TickTime t ->
            let
                timeInSeconds =
                    subtractTimeSeconds t hiddenModel.initT

                keyChecker =
                    keyCheckerFunction hiddenModel.keys

                arrowKeys =
                    arrowChecker keyChecker UpArrow DownArrow LeftArrow RightArrow

                wasd =
                    arrowChecker keyChecker (Key "w") (Key "s") (Key "a") (Key "d")

                newModel =
                    userUpdate (hiddenModel.tick timeInSeconds ( keyChecker, arrowKeys, wasd )) userModel
            in
            ( ( newModel, { hiddenModel | keys = maintainKeyDict hiddenModel.keys } ), Cmd.none )

        KeyDown keyCode ->
            ( ( userModel, { hiddenModel | keys = insertKeyDict hiddenModel.keys keyCode WentDown } ), Cmd.none )

        KeyUp keyCode ->
            ( ( userModel, { hiddenModel | keys = insertKeyDict hiddenModel.keys keyCode WentUp } ), Cmd.none )

{-| This type alias is only used as a target for a user `main` type signature to make
the type signature more clear and concise when `main` calls `gameApp`:
```
main : EllieAppWithTick Model Msg
main =
    ellieAppWithTick Tick
        { model = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
```
where `Tick` is the message handler called once per browser window update,
`Model` is the type alias of the user persistent model, and
`Msg` is the name of the user message type; if other names are used,
they can be substituted for these names.
-}
type alias EllieAppWithTick flags userModel userMsg =
    EllieApp flags ( userModel, HiddenModel userMsg ) (HiddenMsg userMsg)

{-|
A GraphicSVG.ellieApp with automatic time and keyboard presses passed into the update function.
`ellieAppWithTick` takes two parameters: one is your own type of `InputHandler` message
which will be automatically called each time the browser window is refreshed
(30 times per second)
of the form `Float -> GetKeyState -> UserMsg` and the other is
```
{
    init = model
,   view = view
,   update = update
,   subscriptions = subscriptions
}
where init is the model and initial commands, view is a collage and a title,
update is the usual update function with commands, and subscriptions are things
which you'd like to be notified about on a regular basis (e.g. changes in time, 
incoming WebSocket messages)
-}
ellieAppWithTick :
    InputHandler userMsg
    ->
        { init : flags -> ( userModel, Cmd userMsg )
        , update : userMsg -> userModel -> ( userModel, Cmd userMsg )
        , view : userModel -> { title : String, body : Collage userMsg }
        , subscriptions : userModel -> Sub userMsg
        }
    -> EllieAppWithTick flags userModel userMsg
ellieAppWithTick tickMsg userApp =
    let
        userInit =
            userApp.init

        userUpdate =
            userApp.update

        userView =
            userApp.view

        userSubs =
            userApp.subscriptions
    in
    GraphicSVG.ellieApp
        { init =
            \flags ->
                let
                    userInitModel =
                        Tuple.first <| userInit flags

                    userInitCmds =
                        Tuple.second <| userInit flags
                in
                ( ( userInitModel, initHiddenModel tickMsg ), Cmd.batch [ Task.perform InitTime Time.now, Cmd.map UserMsg userInitCmds ] )
        , update = hiddenTickUpdate userUpdate
        , view =
            \( userModel, _ ) ->
                let
                    userViewE =
                        userView userModel
                in
                { title = userViewE.title, body = mapCollage UserMsg userViewE.body }
        , subscriptions = \( userModel, _ ) -> Sub.batch <| (Sub.map UserMsg <| userSubs userModel) :: subs
        }


hiddenTickUpdate :
    (userMsg -> userModel -> ( userModel, Cmd userMsg ))
    -> HiddenMsg userMsg
    -> ( userModel, HiddenModel userMsg )
    -> ( ( userModel, HiddenModel userMsg ), Cmd (HiddenMsg userMsg) )
hiddenTickUpdate userUpdate msg ( userModel, hiddenModel ) =
    let
        updateTick =
            hiddenModel.tick
    in
    case msg of
        UserMsg userMsg ->
            let
                ( newUserModel, newUserCmds ) =
                    userUpdate userMsg userModel
            in
            ( ( newUserModel, hiddenModel ), Cmd.map UserMsg newUserCmds )

        InitTime t ->
            ( ( userModel, { hiddenModel | initT = t } ), Cmd.none )

        TickTime t ->
            let
                timeInSeconds =
                    subtractTimeSeconds t hiddenModel.initT

                keyChecker =
                    keyCheckerFunction hiddenModel.keys

                arrowKeys =
                    arrowChecker keyChecker UpArrow DownArrow LeftArrow RightArrow

                wasd =
                    arrowChecker keyChecker (Key "w") (Key "s") (Key "a") (Key "d")

                ( newUserModel, newUserCmds ) =
                    userUpdate (hiddenModel.tick timeInSeconds ( keyChecker, arrowKeys, wasd )) userModel
            in
            ( ( newUserModel, { hiddenModel | keys = maintainKeyDict hiddenModel.keys } ), Cmd.map UserMsg newUserCmds )

        KeyDown keyCode ->
            ( ( userModel, { hiddenModel | keys = insertKeyDict hiddenModel.keys keyCode WentDown } ), Cmd.none )

        KeyUp keyCode ->
            ( ( userModel, { hiddenModel | keys = insertKeyDict hiddenModel.keys keyCode WentUp } ), Cmd.none )
