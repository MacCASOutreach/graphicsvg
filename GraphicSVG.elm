module GraphicSVG
    exposing
        ( Stencil
        , Shape
        , GraphicSVG
        , collage
        , GraphicsProgram
        , graphicsApp
        , NotificationsProgram
        , notificationsApp
        , GetKeyState
        , Keys(..)
        , KeyState(..)
        , GameProgram
        , gameApp
        , line
        , polygon
        , openPolygon
        , ngon
        , triangle
        , square
        , rect
        , rectangle
        , roundedRect
        , circle
        , oval
        , wedge
        , graphPaper
        , Pull(..)
        , curve
        , curveHelper
        , solid
        , dotted
        , dashed
        , longdash
        , dotdash
        , increasing
        , custom
        , text
        , size
        , bold
        , italic
        , underline
        , strikethrough
        , centered
        , sansserif
        , serif
        , fixedwidth
        , customFont
        , move
        , rotate
        , scale
        , scaleX
        , scaleY
        , mirrorX
        , mirrorY
        , filled
        , outlined
        , addOutline
        , rgb
        , rgba
        , hsl
        , hsla
        , notifyTap
        , notifyTapAt
        , notifyEnter
        , notifyEnterAt
        , notifyLeave
        , notifyLeaveAt
        , notifyMouseMoveAt
        , notifyMouseDown
        , notifyMouseDownAt
        , notifyMouseUp
        , notifyMouseUpAt
        , notifyTouchStart
        , notifyTouchStartAt
        , notifyTouchEnd
        , notifyTouchEndAt
        , notifyTouchMoveAt
        , makeTransparent
        , addHyperlink
        , group
        , black
        , blank
        , blue
        , brown
        , charcoal
        , darkBlue
        , darkBrown
        , darkCharcoal
        , darkGray
        , darkGreen
        , darkGrey
        , darkOrange
        , darkPurple
        , darkRed
        , darkYellow
        , gray
        , green
        , grey
        , hotPink
        , lightBlue
        , lightBrown
        , lightCharcoal
        , lightGray
        , lightGreen
        , lightGrey
        , lightOrange
        , lightPurple
        , lightRed
        , lightYellow
        , orange
        , pink
        , purple
        , red
        , white
        , yellow
        )

{-| A library for creating SVG graphics in a way that is compatible with Elm's
old Graphics library. Also includes built-in functions for creating games and
other applications including keyboard presses and mouse movements.


# Basic Types

@docs Stencil, Shape, GraphicSVG


# Rendering To Screen

@docs collage


# Graphics App

@docs graphicsApp, GraphicsProgram


# Notifications App

@docs notificationsApp, NotificationsProgram


# Game App

@docs GetKeyState, Keys, KeyState, gameApp, GameProgram


# Stencils

@docs line, polygon, openPolygon, ngon, triangle, square, rect, rectangle, roundedRect, circle, oval, wedge, graphPaper


# Creating Shapes by Filling and Outlining Stencils

@docs filled, outlined, addOutline, rgb, rgba, hsl, hsla


# Grouping Shapes

@docs group


# Curves

@docs curve, Pull, curveHelper


# Line Styles

@docs solid, dotted, dashed, longdash, dotdash, increasing, custom


# Text

@docs text, size, bold, italic, underline, strikethrough, centered, sansserif, serif, fixedwidth, customFont


# Transformations

@docs move, rotate, scale, scaleX, scaleY, mirrorX, mirrorY


# Notifications

@docs notifyTap, notifyTapAt, notifyEnter, notifyEnterAt, notifyLeave, notifyLeaveAt, notifyMouseMoveAt, notifyMouseDown, notifyMouseDownAt, notifyMouseUp, notifyMouseUpAt, notifyTouchStart, notifyTouchStartAt, notifyTouchEnd, notifyTouchEndAt, notifyTouchMoveAt


# Miscallaneous

@docs makeTransparent, addHyperlink


# Let there be colours!

@docs black,blank,blue,brown,charcoal,darkBlue,darkBrown,darkCharcoal,darkGray,darkGreen,darkGrey,darkOrange,darkPurple,darkRed,darkYellow,gray,green,grey,hotPink,lightBlue,lightBrown,lightCharcoal,lightGray,lightGreen,lightGrey,lightOrange,lightPurple,lightRed,lightYellow,orange,pink,purple,red,white,yellow

-}

{- Library created by Chris Schankula and Dr. Christopher Anand
   for the McMaster University Computing and Software Outreach Program
   and CompSci 1JC3, with input and testing from the rest of the Outreach
   team.
   Last updated: Tuesday, May 2nd, 2017
-}

import Html
import Html.Attributes
import Html.Events
import Svg
import Svg.Attributes exposing (..)
import String exposing (..)
import Svg exposing (Attribute)
import Array
import Mouse exposing (Position)
import Json.Decode as Json exposing (..)
import Time exposing (..)
import Window
import Task
import Keyboard exposing (..)
import Dict
import Char


{-| A primitive template representing the eventual Shape you wish to draw. This must be turned into
a Shape before being drawn to the screen with collage.
-}
type Stencil
    = Circle Float
    | Rect Float Float
    | RoundRect Float Float Float
    | Oval Float Float
    | BezierPath ( Float, Float ) (List ( ( Float, Float ), ( Float, Float ) ))
    | Polygon (List ( Float, Float ))
    | Path (List ( Float, Float ))
    | Text Face String


{-| A filled, outlined, or filled and outlined object that can be drawn to the screen using collage.
-}
type Shape notification
    = Inked Color (Maybe ( LineType, Color )) Stencil
    | ForeignObject (Html.Html notification)
    | ClipPath String (Shape notification) (Shape notification)
    | Move ( Float, Float ) (Shape notification)
    | Rotate Float (Shape notification)
    | ScaleXY Float Float (Shape notification)
    | Group (List (Shape notification))
    | Link String (Shape notification)
    | Tap notification (Shape notification)
    | TapAt (( Float, Float ) -> notification) (Shape notification)
    | EnterShape notification (Shape notification)
    | EnterAt (( Float, Float ) -> notification) (Shape notification)
    | Exit notification (Shape notification)
    | ExitAt (( Float, Float ) -> notification) (Shape notification)
    | MouseDown notification (Shape notification)
    | MouseDownAt (( Float, Float ) -> notification) (Shape notification)
    | MouseUp notification (Shape notification)
    | MouseUpAt (( Float, Float ) -> notification) (Shape notification)
    | MoveOverAt (( Float, Float ) -> notification) (Shape notification)
    | TouchStart notification (Shape notification)
    | TouchEnd notification (Shape notification)
    | TouchStartAt (( Float, Float ) -> notification) (Shape notification)
    | TouchEndAt (( Float, Float ) -> notification) (Shape notification)
    | TouchMoveAt (( Float, Float ) -> notification) (Shape notification)


{-| The GraphicSVG type alias represents the drawable surface of the window.

This type is only used to define a type signature for a user defined "view" as follows:

    view : GraphicSVG.GraphcSVG msgs

for use with "graphicsApp" where "msgs" can be anything as messages
are not used, and as follows:

    view : Model -> GraphicSVG.GraphcSVG Msg

for use with "notificationsApp" and "gameApp".

These assume that Model is the name of the user model type alias and
"Msg" is the name of the user msg type; just substitute the names
actually used.

-}
type alias GraphicSVG notifications =
    Collage (Msg notifications)


type Color
    = RGBA Float Float Float Float


type LineType
    = Solid Float
    | Broken (List ( Float, Float )) Float



-- length of lines and gaps in pixels


type Face
    = Face
        Float
        -- size
        Bool
        -- bold
        Bool
        -- italic
        Bool
        -- underline
        Bool
        -- strikethrough
        Bool
        -- selectable
        Font
        Bool



-- centered


type Font
    = Serif
    | Sansserif
    | FixedWidth
    | Custom String


{-| To make it easier to read the code defining a curve,
and to make sure we always use the right number of curve points
and pull points (which is one more curve point than pull points),
we define a special Pull type, whose first point is the point
we pull towards, and second point is the end point for this
curve segmentsment.
-}
type Pull
    = Pull ( Float, Float ) ( Float, Float )


{-| The possible states when you ask for a key's state.
JustDown is the frame after the key went down (will show up exactly once per press)
Down is a press that is continuing for more than one frame
JustUp is the frame after the key went up / stopped being pressed (will show up exactly once per press)
Up means the key is not currently being pressed nor was it recently released.
-}
type KeyState
    = JustDown
    | Down
    | JustUp
    | Up


type KeyAction
    = WentUp
    | WentDown


{-| The simplest way to render graphics to the screen. These graphics will be
static (they don't move) and you can't interact with them. Great for beginners
or for when you just need basic graphics. Note that your view function is bare,
with no parameters:

    view =
        collage 500
            500
            [ circle 10 |> filled red
            ]

graphicsApp takes a parameter like this:
{
view = view
}
so the main program that would get the whole thing started for the above
`view' would be:

    main =
        graphicsApp { view = view }

-}
graphicsApp : JustGraphics a -> GraphicsProgram a
graphicsApp input =
    Html.program
        { init = ( ( 0, initGModel ), initialSizeCmd [] input.view )
        , update = blankUpdate
        , view = blankView input.view
        , subscriptions = \_ -> Window.resizes sizeToMsg
        }


{-| The JustGraphics type alias is a simple record that contains the pointer to
the users view constant, which `view' does not take any arguments and returns
a GraphicsProgram type.
-}
type alias JustGraphics a =
    { view : Collage (Msg a) }


{-| This type alias is only used as a target for a user "main" type signature
to make the type signature more clear and concise when "main" calls
"graphicsApp":

    main : GraphicsProgram msgs
    main =
        graphicsApp { view = view }

Note that msgs can be anything as no messages are used in this type of program.

-}
type alias GraphicsProgram a =
    Program Never ( Int, GModel (Msg a) ) (Msg a)


{-| Like graphicsApp, but you can add interactivity to your graphics by using the
"notify" functions. This allows you to learn Elm's architecture in a fun way with
graphics. Note that your view function needs a model parameter now:

    view model =
        collage 500
            500
            [ circle 10 |> filled model |> notifyTap Change
            ]

notificationApp takes a parameter like:
{
model = model
, view = view
, update = update
}
so the functions that would be required to make the above `view' function work
are as follows:

    type Msg = Change

    type update msg model =
        case msg of
            Change -> green

    main = notificationsApp { model = red, update = update, view = view }

which will cause the red circle on-screen to change to green the first time
it is mouse clicked or tapped.

-}
notificationsApp : GraphicsApp model msgs -> NotificationsProgram model msgs
notificationsApp input =
    Html.program
        { init = ( ( input.model, initGModel ), initialSizeCmd [] (input.view input.model) )
        , update = hiddenUpdate input.update
        , view = hiddenView input.view
        , subscriptions = \_ -> Window.resizes sizeToMsg
        }


{-| This type alias is only used as a target for a user "main" type signature
to make the type signature more clear and concise when "main" calls
"notificationssApp":

    main : NotificationProgram Model Msg
    main =
        notificationsApp { model = init, update = update, view = view }

where "Model" is the type alias of the user persistent model, and
"Msg" is the name of the user defined message type;
if other names are used, they can just be substituted for these names.

-}
type alias NotificationsProgram model msgs =
    Program Never ( model, GModel (Msg msgs) ) (Msg msgs)


{-| Automatically maps time and keyboard presses to your program. This should
be all you need for making complex interactive games and animations.
gameApp takes two parameters: one is your own type of `InputHandler' message
which will be automatically called each time the browser window is refreshed
(usually either 50 or 60 times a second, depending on power frequency)
of the form (Float -> GetKeyState -> CustomMsg) and the other is
{
model = model
, view = view
, update = update
}

The following program causes animation of the drawn line,
causing it to spin around; also, a press of the "r" key
causes the direction of the spin to reverse:

    type Msg
        = Tick Float GetKeyState

    type alias Model =
        { angle : Float, speed : Float }

    init =
        { angle = 0, speed = 1 }

    update msg model =
        case msg of
            Tick _ ( keys, _, _ ) ->
                case keys (Key "r") of
                    JustDown ->
                        { model
                            | angle = model.angle - model.speed
                            , speed = -model.speed
                        }

                    _ ->
                        { model | angle = model.angle + model.speed }

    view model =
        collage 500
            500
            [ line ( 0, 0 ) ( 250, 0 )
                |> outlined (solid 1) green
                |> rotate (degrees model)
            ]

    main =
        gameApp Tick { model = init, update = update, view = view }

-}
gameApp : InputHandler msgs -> GraphicsApp model msgs -> GameProgram model msgs
gameApp tickMsg input =
    Html.program
        { init = ( ( input.model, { initGModel | updateTick = tickMsg } ), initialSizeCmd [] (input.view input.model) )
        , update = hiddenGameUpdate input.update
        , view = hiddenView input.view
        , subscriptions = subs []
        }


{-| The InputHandler type alias descripts a message that contains a Float representing the time in seconds from
the time the program started and the `GetKeyState` type alias used for returning key actions.
-}
type alias InputHandler msgs =
    Float
    -> GetKeyState
    -> msgs


{-| The GraphicsApp type alias is a record that contains the pointers to
the user's initial condition of the persistant state model of any type, the
user's update function which takes arguments of the message to be acted on
and the current state of the model and returns the new state of the model, and
the view function, which takes one argument of the current state of the model
and returns a Collage type.
-}
type alias GraphicsApp model msgs =
    { model : model, update : msgs -> model -> model, view : model -> Collage (Msg msgs) }


{-| This type alias is only used as a target for a user "main" type signature to make
the type signature more clear and concise when "main" calls "gameApp":

    main : GamesProgram Model Msg
    main =
        gameApp Tick { model = init, update = update, view = view }

where "Tick" is the message handler called once per browser window update
(Tick must be one of the user-defined Msgs with type Float -> GetKeyState -> Tick,
written as Tick Float GetKeyState),
"Model" is the type alias of the user persistent model, and
"Msg" is the name of the user message type; if other names are used,
they can just be substituted for these names.

-}
type alias GameProgram model msgs =
    Program Never ( model, GModel (InputHandler msgs) ) (Msg msgs)


subs : List (Sub (Msg notes)) -> a -> Sub (Msg notes)
subs extraSubs model =
    Sub.batch
        ([ Time.every (1000 / 30 * millisecond) (createTimeMessage)

         -- AnimationFrame.times (createTimeMessage timeMsg)
         , Window.resizes sizeToMsg
         ]
            ++ keySubs
            ++ extraSubs
        )


keySubs : List (Sub (Msg notes))
keySubs =
    [ Keyboard.ups (KeyUp), Keyboard.downs (KeyDown) ]


createTimeMessage : Time -> Msg notes
createTimeMessage t =
    let
        time =
            inSeconds t
    in
        TickTime time


blankUpdate :
    Msg notes
    -> ( a, { b | ch : Float, cw : Float, sh : Float, sw : Float } )
    -> ( ( a, { b | ch : Float, cw : Float, sh : Float, sw : Float } ), Cmd msg )
blankUpdate msg ( model, gModel ) =
    case msg of
        Graphics message ->
            ( ( model, gModel ), Cmd.none )

        WindowResize ( width, height ) ->
            ( ( model, { gModel | sw = Basics.toFloat width, sh = Basics.toFloat height } ), Cmd.none )

        ReturnPosition message ( x, y ) ->
            ( ( model, gModel ), Cmd.none )

        CollageSize ( width, height ) ->
            ( ( model, { gModel | cw = Basics.toFloat width, ch = Basics.toFloat height } ), Cmd.none )

        _ ->
            ( ( model, gModel ), Cmd.none )


hiddenUpdate :
    (a -> b -> b)
    -> Msg a
    -> ( b, { c | ch : Float, cw : Float, sh : Float, sw : Float } )
    -> ( ( b, { c | ch : Float, cw : Float, sh : Float, sw : Float } ), Cmd msg )
hiddenUpdate update msg ( model, gModel ) =
    case msg of
        Graphics message ->
            ( ( update message model, gModel ), Cmd.none )

        WindowResize ( width, height ) ->
            ( ( model, { gModel | sw = Basics.toFloat width, sh = Basics.toFloat height } ), Cmd.none )

        ReturnPosition message ( x, y ) ->
            ( ( update (message (convertCoords ( x, y ) gModel)) model, gModel ), Cmd.none )

        CollageSize ( width, height ) ->
            ( ( model, { gModel | cw = Basics.toFloat width, ch = Basics.toFloat height } ), Cmd.none )

        _ ->
            ( ( model, gModel ), Cmd.none )


hiddenGameUpdate :
    (a -> b -> b)
    -> Msg a
    ->
        ( b
        , { c
            | ch : Float
            , cw : Float
            , initT : Float
            , keys : KeyDict
            , sh : Float
            , sw : Float
            , updateTick :
                Float
                -> ( Keys -> KeyState, ( number, number1 ), ( number2, number3 ) )
                -> a
          }
        )
    ->
        ( ( b
          , { c
                | ch : Float
                , cw : Float
                , initT : Float
                , keys : KeyDict
                , sh : Float
                , sw : Float
                , updateTick :
                    Float
                    -> ( Keys -> KeyState, ( number, number1 ), ( number2, number3 ) )
                    -> a
            }
          )
        , Cmd msg
        )
hiddenGameUpdate update msg ( model, gModel ) =
    let
        updateTick =
            gModel.updateTick
    in
        case msg of
            Graphics message ->
                ( ( update message model, gModel ), Cmd.none )

            WindowResize ( width, height ) ->
                ( ( model, { gModel | sw = Basics.toFloat width, sh = Basics.toFloat height } ), Cmd.none )

            ReturnPosition message ( x, y ) ->
                ( ( update (message (convertCoords ( x, y ) gModel)) model, gModel ), Cmd.none )

            CollageSize ( width, height ) ->
                ( ( model, { gModel | cw = Basics.toFloat width, ch = Basics.toFloat height } ), Cmd.none )

            InitTime t ->
                ( ( model, { gModel | initT = t } ), Cmd.none )

            TickTime t ->
                ( ( update (gModel.updateTick (t - gModel.initT) ( (keyCheckerFunction gModel.keys), arrowKeys (keyCheckerFunction gModel.keys), wasdKeys (keyCheckerFunction gModel.keys) )) model, { gModel | keys = maintainKeyDict gModel.keys } ), Cmd.none )

            KeyDown n ->
                ( ( model, { gModel | keys = insertKeyDict gModel.keys n WentDown } ), Cmd.none )

            KeyUp n ->
                ( ( model, { gModel | keys = insertKeyDict gModel.keys n WentUp } ), Cmd.none )

            NoOp ->
                ( ( model, gModel ), Cmd.none )


blankView : Collage notification -> ( a, b ) -> Html.Html notification
blankView view ( model, gModel ) =
    case view of
        Collage ( w, h ) shapes ->
            createCollage w h shapes


hiddenView : (a -> Collage notification) -> ( a, b ) -> Html.Html notification
hiddenView view ( model, gModel ) =
    case (view model) of
        Collage ( w, h ) shapes ->
            createCollage w h shapes


convertCoords :
    ( Float, Float )
    -> { a | ch : Float, cw : Float, sh : number, sw : Float }
    -> ( Float, Float )
convertCoords ( x, y ) gModel =
    let
        sw =
            gModel.sw

        sh =
            gModel.sh

        cw =
            gModel.cw

        ch =
            gModel.ch

        aspect =
            if not (sh == 0) then
                sw / sh
            else
                4 / 3

        scaledInX =
            aspect < 4 / 3

        scaledInY =
            aspect > 4 / 3

        scale =
            if scaledInX then
                sw / cw
            else if scaledInY then
                sh * 0.99 / ch
            else
                1

        newW =
            cw * scale

        newH =
            ch * scale

        leadX =
            if scaledInY then
                (sw - newW) / 2
            else
                0

        leadY =
            if scaledInX then
                (sh - newH) / 2
            else
                0

        offsetY =
            if scaledInX then
                -3
            else
                0
    in
        ( (x - leadX - newW / 2) / scale, (y + leadY + offsetY + newH / 2) / scale )



--initialSizeCmd : Cmd Msg


initialSizeCmd :
    List (Cmd (Msg notes))
    -> Collage notification
    -> Cmd (Msg notes)
initialSizeCmd otherCmds userView =
    Cmd.batch
        ([ Task.perform sizeToMsg Window.size
         , Task.perform getCollageSize (Task.succeed userView)
         , Task.perform getInitTime Time.now
         ]
            ++ otherCmds
        )


getInitTime : Time -> Msg notes
getInitTime t =
    InitTime (inSeconds t)


sizeToMsg : Window.Size -> Msg a
sizeToMsg size =
    WindowResize ( size.width, size.height )


getCollageSize : Collage notification -> Msg notes
getCollageSize userView =
    case userView of
        Collage ( w, h ) _ ->
            CollageSize ( round w, round h )


{-| The Msg type encapsulates all GraphicSVG internal messages.

This type is only used to define type signature for user defined
"view" and "main" as follows:

    view : GraphicSVG.Collage (GraphicSVG.Msg msg)

for use with "graphicsApp" and "notificationsApp", and as follows:

    view : Model -> GraphicSVG.Collage (GraphicSVG.Msg msg)

for use with "gameApp".

It is also used to define the type signature for
a user supplied "main" as follows:

    main : Program Never (GraphicsModel Model Msg) (GraphicSVG.Msg msg)

for use with "graphicsApp" and "notificationsApp", and as follows:

    main : Program Never (GamesModel Model Msg) (GraphicSVG.Msg msg)

for use when "main" calls "gameApp"

These assume that "Model" is the type alias of the user persistent model, and
"Msg" is the name of the user msg type.

-}
type Msg notes
    = Graphics notes
    | WindowResize ( Int, Int )
    | ReturnPosition (( Float, Float ) -> notes) ( Float, Float )
    | CollageSize ( Int, Int )
    | InitTime Time
    | TickTime Time
    | KeyDown Int
    | KeyUp Int
    | NoOp


aHiddenUpdate : (a -> b -> c) -> a -> b -> ( c, Cmd msg )
aHiddenUpdate update msg model =
    ( update msg model, Cmd.none )


aHiddenView : (a -> b) -> a -> b
aHiddenView view model =
    view model


{-| The GModel type alias encapsulates the GraphicSVG internal model
which is not exposed to user code.
-}
type alias GModel a =
    { cw : Float
    , ch : Float
    , sw : Float
    , sh : Float
    , initT : Float
    , updateTick : a
    , keys : KeyDict
    }


initGModel :
    { ch : number
    , cw : number1
    , initT : number2
    , keys : Dict.Dict k v
    , sh : number3
    , sw : number4
    , updateTick : Msg notes
    }
initGModel =
    { cw = 0
    , ch = 0
    , sw = 0
    , sh = 0
    , initT = 0
    , updateTick = NoOp
    , keys = Dict.empty
    }


type alias KeyDict =
    Dict.Dict KeyCode ( KeyState, Bool )


{-| GetKeyState returns a triple where the first argument is of type (Keys -> KeyState)
so you can ask if a certain key is presses. The other two are tuples of arrow keys and
WASD keys, respectively. They're in the form (x,y) which represents the key presses
of each player. For example, (0,-1) represents the left or "A" key, and (1,1) would mean
the up (or "W") and right (or "D") keys are being pressed at the same time.
-}
type alias GetKeyState =
    ( Keys -> KeyState, ( Float, Float ), ( Float, Float ) )


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


{-| Includes all the regular keys. Ask for letters and numbers using "Key String."
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
                    Char.toCode
                        (Char.toUpper
                            (case (String.uncons str) of
                                Just ( a, bc ) ->
                                    a

                                Nothing ->
                                    'z'
                            )
                        )

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


arrowKeys : (Keys -> KeyState) -> ( number, number1 )
arrowKeys checker =
    ( case ( (checker LeftArrow), (checker RightArrow) ) of
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
    , case ( (checker DownArrow), (checker UpArrow) ) of
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


wasdKeys : (Keys -> KeyState) -> ( number, number1 )
wasdKeys checker =
    ( case ( (checker (Key "a")), (checker (Key "d")) ) of
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
    , case ( (checker (Key "s")), (checker (Key "w")) ) of
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


{-| Create a line from a point to a point. Use "outlined" to convert to a viewable
Shape.
-}
line : ( Float, Float ) -> ( Float, Float ) -> Stencil
line p1 p2 =
    Path [ p1, p2 ]


{-| Create a closed shape given a list of points. Can use "outlined" or "filled" to
convert to a Shape.
-}
polygon : List ( Float, Float ) -> Stencil
polygon ptList =
    Polygon ptList


{-| Create an open shape given a list of points. Unlike with polygon, the first and
last points will not join up automatically. Can use "outlined" or "filled" to
convert to a Shape.
-}
openPolygon : List ( Float, Float ) -> Stencil
openPolygon ptList =
    Path ptList


{-| Create a regular polygon with a given number of sides and radius.
Examples:

    ngon 3 50 - triangle
    ngon 5 50 - pentagon
    ngon 8 50 - octogon

-}
ngon : Int -> Float -> Stencil
ngon n r =
    Polygon <| List.map (ptOnCircle r (Basics.toFloat n) << Basics.toFloat) (List.range 0 n)


{-| Synonym for "ngon 3". Creates a triangle from a circle of given radius.
-}
triangle : Float -> Stencil
triangle r =
    ngon 3 <| vF r


{-| Creates a square with a given side length. (Synonym for rect s s)
-}
square : Float -> Stencil
square r =
    Rect (vF r)  (vF r)


{-| Creates a rectangle with a given width and height.
-}
rect : Float -> Float -> Stencil
rect w h =
    Rect (vF w) (vF h)


{-| Synonym for rect.
-}
rectangle : Float -> Float -> Stencil
rectangle w h =
    Rect (vF w) (vF h) --NaN and infinity cases covered by the rect function


{-| Creates a rectangle with a given width, height, and circular rounded
corners with the given radius.
-}
roundedRect : Float -> Float -> Float -> Stencil
roundedRect w h r =
    RoundRect (vF w) (vF h) (vF r)


{-| Creates a circle with a given radius.
-}
circle : Float -> Stencil
circle r =
    Circle (vF r)


{-| Creates an oval with a given width and height.
-}
oval : Float -> Float -> Stencil
oval w h =
    Oval (vF w) (vF h)


{-| Creates a graph paper with squares of a given size.
-}
graphPaper : Float -> Shape notification
graphPaper s =
    let
        sxi =
            round <| vF (1500 / s)

        syi =
            round <| vF (800 / s)

        xlisti =
            (List.range -sxi sxi)

        ylisti =
            (List.range -syi syi)
    in
        group
            (List.map (createGraphX 1600 (vF s) << Basics.toFloat) xlisti
                ++ List.map (createGraphY 3000 (vF s) << Basics.toFloat) ylisti
            )


createGraphX : Float -> Float -> Float -> Shape notification
createGraphX h s x =
    filled (rgb 135 206 250) (rect 1 (vF h)) |> move ( (vF x) * (vF s), 0 )


createGraphY : Float -> Float -> Float -> Shape notification
createGraphY w s y =
    filled (rgb 135 206 250) (rect (vF w) 1) |> move ( 0, (vF y) * (vF s))


{-| Creates a wedge with a given radius, and a given fraction of a circle.
wedge 50 0.5 - semi-circle
wedge 50 0.25 - quarter-circle
wedge 50 0.75 - three-quarter circle
-}
wedge : Float -> Float -> Stencil
wedge r frac =
    let
        n =
            (vF frac) * 360 / 10 + 5

        ni =
            round n
    in
        Polygon <|
            if (vF frac) > 0 then
                [ ( 0, 0 ), wedgeHelper r (-frac * 180) ]
                    ++ (List.map ((wedgeHelper r) << ((*) ( vF(frac / n) * 180)) << Basics.toFloat) (List.range -ni ni))
                    ++ [ wedgeHelper r (frac * 180), ( 0, 0 ) ]
            else
                []


wedgeHelper : Float -> Float -> ( Float, Float )
wedgeHelper r cn =
    let
        angle =
            vF cn
    in
        ( (vF r) * cos (degrees angle), (vF r) * sin (degrees angle) )


ptOnCircle : Float -> Float -> Float -> ( Float, Float )
ptOnCircle r n cn =
    let
        angle =
            360 * vF (cn / n)
    in
        ( vF r * cos (degrees angle), vF r * sin (degrees angle) )


{-| Creates a curve starting at a point, pulled towards a point, ending at a third point.
curve (0,0) [Pull (0,10) (0,20)] - a curve starting at (0,0), pulled towards (0,10), ending at (0,20)
Think about curves as what you get when you take a bunch of
bendy sticks with their ends glued down to a board, and then pulling each stick
towards another point.
You always need an initial point and at least one Pull, but you can add as many Pulls as you want to
add additional curve segments, but each curve segment can only bend one way, since it is pulled in one direction.
-}
curve : ( Float, Float ) -> List Pull -> Stencil
curve ( a, b ) list =
    BezierPath ( vF a, vF b ) (List.map curveListHelper list)


curveListHelper : Pull -> ( ( Float, Float ), ( Float, Float ) )
curveListHelper (Pull ( a, b ) ( c, d )) =
    ( ( vF a, vF b ), ( vF c, vF d ) )


{-| Add a hyperlink to any `Shape`.
circle 10
|> filled red
|> addHyperLink "<http://outreach.mcmaster.ca">
-}
addHyperlink : String -> Shape notification -> Shape notification
addHyperlink link shape =
    Link link shape


{-| Creates a text stencil. You can change this stencil using the text helper
functions. Note that "filled" or "outlined" must go at the *end* of the infixes
(ie note that all these functions are Stencil -> Stencil).
text "Hello World"
|> fixedwidth
|> bold
|> size 14
|> filled black
-}
text : String -> Stencil
text str =
    Text (Face 12 False False False False False Serif False) str


{-| Apply to a curve or group of curves in order to annotate their start points,
end points and "pull" points. Helpful while perfecting curves.
curve (0,0) [Pull (0,10) (0,20)]
|> curveHelper
-}
curveHelper : Shape notification -> Shape notification
curveHelper shape =
    case shape of
        Inked clr outline (BezierPath ( a, b ) list) ->
            group [ shape, generateCurveHelper ( a, b ) list ]

        Move s shape ->
            Move s (curveHelper shape)

        Rotate r shape ->
            Rotate r (curveHelper shape)

        ScaleXY sx sy shape ->
            ScaleXY sx sy (curveHelper shape)

        Group list ->
            Group (List.map curveHelper list)

        a ->
            a


generateCurveHelper :
    ( number, number1 )
    -> List ( ( number, number1 ), ( number, number1 ) )
    -> Shape notification
generateCurveHelper ( a, b ) list =
    let
        l1Array =
            Array.fromList ([ ( a, b ) ] ++ List.concat (List.map createTopLevelList list))
    in
        group [ generateCHLines l1Array, generateCHCircles l1Array ]


generateCHLines : Array.Array ( number, number1 ) -> Shape notification
generateCHLines ar =
    let
        len =
            Array.length ar
    in
        group (List.map (generateCHLine ar) (List.range 0 (len - 2)))


generateCHLine : Array.Array ( number, number1 ) -> Int -> Shape notification
generateCHLine ar int =
    let
        p1 =
            case (Array.get int ar) of
                Just p ->
                    p

                Nothing ->
                    ( 0, 0 )

        p2 =
            case (Array.get (int + 1) ar) of
                Just p ->
                    p

                Nothing ->
                    ( 0, 0 )
    in
        outlined (dashed 0.5) black (line (p1) (p2))


generateCHCircles : Array.Array ( number, number1 ) -> Shape notification
generateCHCircles ar =
    let
        len =
            Array.length ar
    in
        group (List.map (generateCHCircle ar) (List.range 0 (len - 1)))


generateCHCircle : Array.Array ( number, number1 ) -> Int -> Shape notification
generateCHCircle ar int =
    let
        p1 =
            case (Array.get int ar) of
                Just p ->
                    p

                Nothing ->
                    ( 0, 0 )

        ptStr =
            pairToString p1
    in
        group [ filled red (circle 2), text ("(" ++ ptStr ++ ")") |> filled black |> move ( 5, 5 ) ] |> move p1


createTopLevelList : ( ( a, b ), ( a, b ) ) -> List ( a, b )
createTopLevelList ( ( a, b ), ( c, d ) ) =
    [ ( a, b ), ( c, d ) ]


type alias Transform =
    ( ( ( Float, Float )
        -- normal transformation of whole group
      , ( Float, Float )
      , ( Float, Float )
      )
    , ( ( Float, Float ), Float, ( Float, Float ) )
      -- scale/rotate/shift inside groups
    )


coalesce :
    ( ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ), ( ( Float, Float ), Float, ( Float, Float ) ) )
    -> ( ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ), ( ( number, number1 ), number2, ( number3, number4 ) ) )
coalesce ( ( ( a, b ), ( c, d ), ( tx, ty ) ), ( ( sx, sy ), rot, ( shx, shy ) ) ) =
    let
        sa =
            sx * a

        sb =
            sy * b

        sc =
            sx * c

        sd =
            sy * d

        rx =
            cos rot

        ry =
            sin rot
    in
        ( ( ( rx * sa - ry * sb, ry * sa + rx * sb )
          , ( rx * sc - ry * sd, ry * sc + rx * sd )
          , ( tx + a * shx + c * shy, ty + b * shx + d * shy )
          )
        , ( ( 1, 1 ), 0, ( 0, 0 ) )
        )


id : ( ( ( number, number1 ), ( number2, number3 ), ( number4, number5 ) ), ( ( number6, number7 ), number8, ( number9, number10 ) ) )
id =
    ( ( ( 1, 0 )
      , ( 0, 1 )
      , ( 0, 0 )
      )
    , ( ( 1, 1 ), 0, ( 0, 0 ) )
    )


moveT : Transform -> ( Float, Float ) -> Transform
moveT ( trans, ( s, r, ( tx, ty ) ) ) ( u, v ) =
    ( trans, ( s, r, ( tx + u, ty + v ) ) )


rotT : ( a, ( b, number, c ) ) -> number -> ( a, ( b, number, c ) )
rotT ( trans, ( s, r, t ) ) rad =
    ( trans, ( s, r + rad, t ) )


scaleT :
    ( a, ( ( number, number1 ), b, ( c, d ) ) )
    -> ( number, number1 )
    -> ( a, ( ( number, number1 ), b, ( c, d ) ) )
scaleT ( trans, ( ( ssx, ssy ), r, ( shx, shy ) ) ) ( sx, sy ) =
    ( trans, ( ( ssx * sx, ssy * sy ), r, ( shx, shy ) ) )


{-| The Collage type represents the drawable surface of the window which contains
a (x, y) pair of horizontal and vertical dimensions (arbitrary units,
not necessarily in pixels) to which the drawing surface will be scaled,
and the `List' of Shapes to be drawn on the drawing surface.
-}
type Collage notification
    = Collage ( Float, Float ) (List (Shape notification))


{-| Creates a blank canvas on which you can draw shapes. Takes a width, height and a
list of Shape. Use this in your "view" functions in the three types of Apps above.
view = collage 500 500
[
circle 10 |> filled red
][
circle 10 |> filled red
]
-}
collage : Float -> Float -> List (Shape notification) -> Collage notification
collage w h shapes =
    Collage ( vF w, vF h ) shapes


createCollage : Float -> Float -> List (Shape a) -> Html.Html a
createCollage w h shapes =
    Svg.svg
        [ width "100%", height "99%", style "position:absolute", viewBox ((toString (-w / 2)) ++ " " ++ (toString (-h / 2)) ++ " " ++ (toString w) ++ " " ++ (toString h)) ]
        ([ cPath w h ] ++ [ Svg.g [ clipPath "url(#cPath)" ] (List.map (createSVG id) shapes) ])


myStyle : Html.Attribute msg
myStyle =
    Html.Attributes.style
        [ ( "width", "100%" )
        , ( "height", "20px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "1.5em" )
        , ( "text-align", "center" )
        , ( "background-color", "#2e3842" )
        , ( "color", "#ffffff" )
        , ( "border", "1px solid #ffffff" )
        , ( "-webkit-touch-callout", "none" )
        , ( "-webkit-user-select", "none" )
        , ( "-khtml-user-select", "none" )
        , ( "-moz-user-select", "none" )
        , ( "-ms-user-select", "none" )
        , ( "user-select", "none" )
        , ( "cursor", "default" )
        , ( "max-width", "600px" )
        ]


cPath : Float -> Float -> Svg.Svg msg
cPath w h =
    Svg.defs [] [ Svg.clipPath [ Svg.Attributes.id "cPath" ] [ Svg.rect [ width (toString w), height (toString h), x (toString (-w / 2)), y (toString (-h / 2)) ] [] ] ]


f : number
f =
    500


puppetShow :
    Float
    -> Float
    -> List ( Float, Shape notification )
    -> Collage notification
puppetShow w h listShapes =
    collage (vF w) (vF h) (List.map extractShape (List.sortWith flippedComparison listShapes))


extractShape : ( Float, Shape notification ) -> Shape notification
extractShape ( z, shape ) =
    let
        s =
            f / (f + z)
    in
        group [ shape ] |> scale s


flippedComparison : ( comparable, a ) -> ( comparable, b ) -> Order
flippedComparison ( a, x ) ( b, y ) =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT



--Notification functions


{-| Receive a notification when a Shape is tapped or clicked.
-}
notifyTap : msgs -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyTap msg shape =
    Tap (Graphics msg) shape


{-| Receive a notification with a tuple of position when the screen is tapped / clicked.
-}
notifyTapAt : (( Float, Float ) -> msgs) -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyTapAt msg shape =
    TapAt (ReturnPosition msg) shape


{-| Receive a notification when the mouse enters a Shape.
-}
notifyEnter : msgs -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyEnter msg shape =
    EnterShape (Graphics msg) shape


{-| Receive a notification with a tuple of position when mouse enters a Shape.
-}
notifyEnterAt : (( Float, Float ) -> msgs) -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyEnterAt msg shape =
    EnterAt (ReturnPosition msg) shape


{-| Receive a notification when the mouse leaves a Shape.
-}
notifyLeave : msgs -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyLeave msg shape =
    Exit (Graphics msg) shape


{-| Receive a notification with a tuple of position when the mouse leaves a Shape.
-}
notifyLeaveAt : (( Float, Float ) -> msgs) -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyLeaveAt msg shape =
    ExitAt (ReturnPosition msg) shape


{-| Receive a notification with a tuple of position when the mouse is moved accross a Shape.
-}
notifyMouseMoveAt : (( Float, Float ) -> msgs) -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyMouseMoveAt msg shape =
    MoveOverAt (ReturnPosition msg) shape


{-| Receive a notification when the mouse button is down over a Shape.
-}
notifyMouseDown : msgs -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyMouseDown msg shape =
    MouseDown (Graphics msg) shape


{-| Receive a notification with a tuple of position when the mouse button is down over a Shape.
-}
notifyMouseDownAt : (( Float, Float ) -> msgs) -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyMouseDownAt msg shape =
    MouseDownAt (ReturnPosition msg) shape


{-| Receive a notification when the mouse button goes up over a Shape.
-}
notifyMouseUp : msgs -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyMouseUp msg shape =
    MouseUp (Graphics msg) shape


{-| Receive a notification with a tuple of position when the mouse goes up over a Shape.
-}
notifyMouseUpAt : (( Float, Float ) -> msgs) -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyMouseUpAt msg shape =
    MouseUpAt (ReturnPosition msg) shape


{-| Receive a notification when the user begins touching a Shape.
-}
notifyTouchStart : msgs -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyTouchStart msg shape =
    TouchStart (Graphics msg) shape


{-| Receive a notification with a tuple of position when the user begins touching a Shape.
-}
notifyTouchStartAt : (( Float, Float ) -> msgs) -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyTouchStartAt msg shape =
    TouchStartAt (ReturnPosition msg) shape


{-| Receive a notification when the user stops touching a Shape.
-}
notifyTouchEnd : msgs -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyTouchEnd msg shape =
    TouchEnd (Graphics msg) shape


{-| Receive a notification with a tuple of position when the user stops touching a Shape.
-}
notifyTouchEndAt : (( Float, Float ) -> msgs) -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyTouchEndAt msg shape =
    TouchEndAt (ReturnPosition msg) shape


{-| Receive a notification with a tuple of position when the moves their finger over a Shape.
-}
notifyTouchMoveAt : (( Float, Float ) -> msgs) -> Shape (Msg msgs) -> Shape (Msg msgs)
notifyTouchMoveAt msg shape =
    TouchMoveAt (ReturnPosition msg) shape


xyToPair : { a | x : Int, y : Int } -> ( Float, Float )
xyToPair xy =
    ( Basics.toFloat (xy.x), Basics.toFloat (-xy.y) )


touchToPair : TouchPos -> ( Float, Float )
touchToPair tp =
    case tp of
        TouchPos x y ->
            ( x, -y )


onTapAt : (( Float, Float ) -> c) -> Html.Attribute c
onTapAt msg =
    Html.Events.on "click"
        (Json.map (msg << xyToPair) Mouse.position)


onEnterAt : (( Float, Float ) -> c) -> Html.Attribute c
onEnterAt msg =
    Html.Events.on "mouseover"
        (Json.map (msg << xyToPair) Mouse.position)


onLeaveAt : (( Float, Float ) -> c) -> Html.Attribute c
onLeaveAt msg =
    Html.Events.on "mouseleave"
        (Json.map (msg << xyToPair) Mouse.position)


onMoveAt : (( Float, Float ) -> c) -> Html.Attribute c
onMoveAt msg =
    Html.Events.on "mousemove"
        (Json.map (msg << xyToPair) Mouse.position)


onMouseDownAt : (( Float, Float ) -> c) -> Html.Attribute c
onMouseDownAt msg =
    Html.Events.on "mousedown"
        (Json.map (msg << xyToPair) Mouse.position)


onMouseUpAt : (( Float, Float ) -> c) -> Html.Attribute c
onMouseUpAt msg =
    Html.Events.on "mouseup"
        (Json.map (msg << xyToPair) Mouse.position)


onTouchStart : a -> Html.Attribute a
onTouchStart msg =
    Html.Events.on "touchstart" (Json.succeed msg)


onTouchStartAt : (( Float, Float ) -> c) -> Html.Attribute c
onTouchStartAt msg =
    Html.Events.on "touchstart"
        (Json.map (msg << touchToPair) touchDecoder)


onTouchEndAt : (( Float, Float ) -> c) -> Html.Attribute c
onTouchEndAt msg =
    Html.Events.on "touchend"
        (Json.map (msg << touchToPair) touchDecoder)


onTouchEnd : a -> Html.Attribute a
onTouchEnd msg =
    Html.Events.on "touchend" (Json.succeed msg)


onTouchMove : (( Float, Float ) -> c) -> Html.Attribute c
onTouchMove msg =
    let
        dOp =
            Html.Events.defaultOptions
    in
        Html.Events.onWithOptions "touchmove"
            { dOp | preventDefault = True }
            (Json.map (msg << touchToPair) touchDecoder)


type TouchPos
    = TouchPos Float Float


touchDecoder : Decoder TouchPos
touchDecoder =
    Json.oneOf
        [ Json.at [ "touches", "0" ] (Json.map2 TouchPos (Json.field "pageX" Json.float) (Json.field "pageY" Json.float))
        , Json.map2 TouchPos (Json.field "pageX" Json.float) (Json.field "pageY" Json.float)
        ]


createSVG : Transform -> Shape a -> Svg.Svg a
createSVG trans shape =
    case shape of
        Inked fillClr lt stencil ->
            let
                ( ( ( a, b ), ( c, d ), ( tx, ty ) ), _ ) =
                    coalesce trans

                attrs =
                    transAttrs ++ clrAttrs ++ strokeAttrs

                transAttrs =
                    [ Svg.Attributes.transform <| "matrix(" ++ (String.concat <| List.intersperse "," <| List.map toString [ a, -b, c, -d, tx, -ty ]) ++ ")" ]

                clrAttrs =
                    [ fill (mkRGB fillClr), fillOpacity (mkAlpha fillClr) ]

                strokeAttrs =
                    case lt of
                        Nothing ->
                            []

                        Just ( Solid w, strokeClr ) ->
                            [ strokeWidth (toString w)
                            , stroke (mkRGB strokeClr)
                            , strokeOpacity (mkAlpha strokeClr)
                            ]

                        Just ( Broken dashes w, strokeClr ) ->
                            [ strokeWidth (toString w)
                            , stroke (mkRGB strokeClr)
                            , strokeOpacity (mkAlpha strokeClr)
                            ]
                                ++ [ strokeDasharray <| String.concat (List.intersperse "," <| List.map pairToString dashes) ]
            in
                (case stencil of
                    Circle r ->
                        Svg.circle
                            ([ cx "0"
                             , cy "0"
                             , Svg.Attributes.r (toString r)
                             ]
                                ++ attrs
                            )
                            []

                    Rect w h ->
                        Svg.rect
                            ([ x (toString (-w / 2))
                             , y (toString (-h / 2))
                             , width (toString w)
                             , height (toString h)
                             ]
                                ++ attrs
                            )
                            []

                    RoundRect w h r ->
                        Svg.rect
                            ([ x (toString (-w / 2))
                             , y (toString (-h / 2))
                             , rx (toString r)
                             , ry (toString r)
                             , width (toString w)
                             , height (toString h)
                             ]
                                ++ attrs
                            )
                            []

                    Oval w h ->
                        Svg.ellipse
                            ([ cx "0"
                             , cy "0"
                             , rx (toString (0.5 * w))
                             , ry (toString (0.5 * h))
                             ]
                                ++ attrs
                            )
                            []

                    -- BezierPath (List )
                    Polygon vertices ->
                        Svg.polygon
                            ([ points <| String.concat <| List.intersperse " " <| List.map pairToString vertices ]
                                ++ attrs
                            )
                            []

                    Path vertices ->
                        Svg.polyline
                            ([ points <| String.concat <| List.intersperse " " <| List.map pairToString vertices ]
                                ++ attrs
                            )
                            []

                    BezierPath start pts ->
                        Svg.path
                            ([ Svg.Attributes.d <| (createBezierString start pts) ]
                                ++ attrs
                            )
                            []

                    Text (Face si bo i u s sel f cen) str ->
                        let
                            bol =
                                if bo then
                                    "font-weight: bold;"
                                else
                                    ""

                            it =
                                if i then
                                    "font-style: italic;"
                                else
                                    ""

                            un =
                                if u then
                                    "text-decoration: underline;"
                                else
                                    ""

                            stri =
                                if s then
                                    "text-decoration: strikethrough;"
                                else
                                    ""

                            select =
                                if not sel then
                                    "-webkit-touch-callout: none;\x0D\n                                                                                      -webkit-user-select: none;\x0D\n                                                                                      -khtml-user-select: none;\x0D\n                                                                                      -moz-user-select: none;\x0D\n                                                                                      -ms-user-select: none;\x0D\n                                                                                      user-select: none;cursor: default;"
                                else
                                    ""

                            anchor =
                                if cen then
                                    "middle"
                                else
                                    "left"

                            font =
                                case f of
                                    Sansserif ->
                                        "sansserif;"

                                    Serif ->
                                        "serif;"

                                    FixedWidth ->
                                        "monospace;"

                                    Custom fStr ->
                                        fStr ++ ";"

                            sty =
                                bol
                                    ++ it
                                    ++ un
                                    ++ stri
                                    ++ "font-family: "
                                    ++ font
                                    ++ select
                        in
                            Svg.text_ ([ x "0", y "0", Svg.Attributes.style sty, Svg.Attributes.fontSize (toString (si)), Svg.Attributes.textAnchor anchor, Html.Attributes.contenteditable True ] ++ attrs ++ [ Svg.Attributes.transform <| "matrix(" ++ (String.concat <| List.intersperse "," <| List.map toString [ a, -b, -c, d, tx, -ty ]) ++ ")" ]) [ Svg.text str ]
                )

        Move v shape ->
            createSVG (moveT trans v) shape

        Rotate deg shape ->
            createSVG (rotT trans deg) shape

        ScaleXY sx sy shape ->
            createSVG (scaleT trans ( sx, sy )) shape

        ForeignObject htm ->
            let
                ( ( ( a, b ), ( c, d ), ( tx, ty ) ), _ ) =
                    coalesce trans
            in
                Svg.foreignObject [ Svg.Attributes.transform <| "matrix(" ++ (String.concat <| List.intersperse "," <| List.map toString [ a, b, c, d, tx, -ty ]) ++ ")" ] [ htm ]

        ClipPath name cshape shape ->
            Svg.g [ Svg.Attributes.clipPath ("url(#" ++ name ++ ")") ] [ Svg.defs [] [ Svg.clipPath [ Svg.Attributes.id name ] [ createSVG (coalesce trans) cshape ] ], createSVG (coalesce trans) shape ]

        Link href shape ->
            Svg.a [ xlinkHref href, target "_blank" ] [ createSVG (coalesce trans) shape ]

        Tap msg shape ->
            Svg.g [ Html.Events.onClick msg ] [ createSVG (coalesce trans) shape ]

        TapAt msg shape ->
            Svg.g [ onTapAt msg ] [ createSVG (coalesce trans) shape ]

        EnterShape msg shape ->
            Svg.g [ Html.Events.onMouseEnter msg ] [ createSVG (coalesce trans) shape ]

        EnterAt msg shape ->
            Svg.g [ onEnterAt msg ] [ createSVG (coalesce trans) shape ]

        Exit msg shape ->
            Svg.g [ Html.Events.onMouseLeave msg ] [ createSVG (coalesce trans) shape ]

        ExitAt msg shape ->
            Svg.g [ onLeaveAt msg ] [ createSVG (coalesce trans) shape ]

        MouseDown msg shape ->
            Svg.g [ Html.Events.onMouseDown msg ] [ createSVG (coalesce trans) shape ]

        MouseDownAt msg shape ->
            Svg.g [ onMouseDownAt msg ] [ createSVG (coalesce trans) shape ]

        MouseUp msg shape ->
            Svg.g [ Html.Events.onMouseUp msg ] [ createSVG (coalesce trans) shape ]

        MouseUpAt msg shape ->
            Svg.g [ onMouseUpAt msg ] [ createSVG (coalesce trans) shape ]

        MoveOverAt msg shape ->
            Svg.g [ onMoveAt msg ] [ createSVG (coalesce trans) shape ]

        TouchStart msg shape ->
            Svg.g [ onTouchStart msg ] [ createSVG (coalesce trans) shape ]

        TouchEnd msg shape ->
            Svg.g [ onTouchEnd msg ] [ createSVG (coalesce trans) shape ]

        TouchStartAt msg shape ->
            Svg.g [ onTouchStartAt msg ] [ createSVG (coalesce trans) shape ]

        TouchEndAt msg shape ->
            Svg.g [ onTouchStartAt msg ] [ createSVG (coalesce trans) shape ]

        TouchMoveAt msg shape ->
            Svg.g [ onTouchMove msg ] [ createSVG (coalesce trans) shape ]

        Group shapes ->
            Svg.g [] <| List.map (createSVG <| coalesce trans) shapes



--Filling / outlining functions


{-| Fill a Stencil with a Color, creating a Shape.
circle 10
|> filled red
-}
filled : Color -> Stencil -> Shape notification
filled color shape =
    Inked color Nothing shape


{-| Outline a Stencil with a LineType and Color, creating a Shape.
circle 10
|> outlined (solid 5) red
-}
outlined : LineType -> Color -> Stencil -> Shape notification
outlined style outlineClr shape =
    let
        lineStyle =
            ( style, outlineClr )
    in
        Inked (rgba 0 0 0 0) (Just lineStyle) shape


{-| Add an outline to an already-filled Shape.
circle 10
|> filled red
|> addOutline (solid 5) white
-}
addOutline : LineType -> Color -> Shape notification -> Shape notification
addOutline style outlineClr shape =
    let
        lineStyle =
            ( style, outlineClr )
    in
        case shape of
            Inked clr outline shape ->
                Inked clr (Just lineStyle) shape

            Move s shape ->
                Move s (addOutline style outlineClr shape)

            Rotate r shape ->
                Rotate r (addOutline style outlineClr shape)

            ScaleXY sx sy shape ->
                ScaleXY sx sy (addOutline style outlineClr shape)

            Group list ->
                Group list

            a ->
                a


{-| Make a Shape transparent by the fraction given. Note that it multiplies on top of other transparencies:
circle 10
|> filled red
|> makeTransparent 0.5
--results in a transparency of 0.5 (half vislible)
circle 10
|> filled red
|> makeTransparent 0.5
|> makeTransparent 0.5
--results in a transparency of 0.25 (a quarter visible)
-}
makeTransparent : Float -> Shape notification -> Shape notification
makeTransparent alpha shape =
    case shape of
        Inked (RGBA r g b a) (Just ( lineType, RGBA sr sg sb sa )) shape ->
            Inked (RGBA r g b (a * alpha)) (Just ( lineType, (RGBA sr sg sb (sa * alpha)) )) shape

        Inked (RGBA r g b a) Nothing shape ->
            Inked (RGBA r g b (a * alpha)) Nothing shape

        Move s shape ->
            Move s (makeTransparent alpha shape)

        Rotate r shape ->
            Rotate r (makeTransparent alpha shape)

        ScaleXY sx sy shape ->
            ScaleXY sx sy (makeTransparent alpha shape)

        Group list ->
            Group (List.map (makeTransparent alpha) list)

        a ->
            a



--Line styles


{-| Define a solid line type with the given width.
-}
solid : Float -> LineType
solid th =
    Solid (vF th)


{-| Define a dotted line type with the given width.
-}
dotted : Float -> LineType
dotted th =
    Broken [ ( vF th, vF th ) ] (vF th)


{-| Define a dashed line type with the given width. Dashes are short line segments, versus dots which are theoretically points, but may be drawn with very sort line segments.
-}
dashed : Float -> LineType
dashed th =
    Broken [ ( vF th * 5, vF th * 2.5 ) ] (vF th)


{-| Define a dashed line type with the given width, where the dashes are longer than normal.
-}
longdash : Float -> LineType
longdash th =
    Broken [ ( vF th * 12, vF th * 6 ) ] (vF th)


{-| Define a line type with the given width, including alternating dots and dashes.
-}
dotdash : Float -> LineType
dotdash th =
    Broken [ ( vF th, vF th ), ( vF th * 5, vF th ) ] (vF th)


{-| A custom line defined by a list of (on,off):
custom [(10,5)] 5 -- a line that with dashes 10 long and spaces 5 long
custom [(10,5),(20,5)] -- on for 10, off 5, on 20, off 5
-}
custom : List ( Float, Float ) -> Float -> LineType
custom list th =
    Broken list (vF th)


{-| A line of increasing spaces from start to end with a given thickness, with
each step multiplied (scaled) by the thickness.
Example:

    increasing 1 10 5 -- increases in 10 steps from 5 to 50 with a thickness of 5.

-}
increasing : Int -> Int -> Float -> LineType
increasing s e th =
    Broken (List.map (makePair << (*) th << Basics.toFloat) (List.range s e)) (vF th)


makePair : a -> ( a, a )
makePair n =
    ( n, n )



--Text functions


{-| Apply to a Stencil to render any text in the Stencil in the size given (in points), which depends on the size and type of screen used, but try 12 to start.
-}
size : Float -> Stencil -> Stencil
size size stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face (vF size) bo i u s sel f c) str

        a ->
            a


{-| Apply to a Stencil to make any text in the Stencil bold.
-}
bold : Stencil -> Stencil
bold stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si True i u s sel f c) str

        a ->
            a


{-| Apply to a Stencil to make any text in the Stencil italic (ie slanted).
-}
italic : Stencil -> Stencil
italic stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo True u s sel f c) str

        a ->
            a


{-| Apply to a Stencil to underline any text in the Stencil.
-}
underline : Stencil -> Stencil
underline stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i True s sel f c) str

        a ->
            a


{-| Apply to a Stencil to draw a line through any text in the Stencil.
-}
strikethrough : Stencil -> Stencil
strikethrough stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u True sel f c) str

        a ->
            a


{-| Apply to a Stencil to make any text in the Stencil selectable (so users can copy your great quote and paste it in their essay).
-}
selectable : Stencil -> Stencil
selectable stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s True f c) str

        a ->
            a


{-| Apply to a Stencil to centre any text in the Stencil.
-}
centered : Stencil -> Stencil
centered stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s sel f True) str

        a ->
            a


{-| Apply to a Stencil to render any text in the Stencil with a Sans Serif font (ie one without thinner and thicker bits).
-}
sansserif : Stencil -> Stencil
sansserif stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s sel Sansserif c) str

        a ->
            a


{-| Apply to a Stencil to render any text in the Stencil with a Serif font (ie one with thinner and thicker bits).
-}
serif : Stencil -> Stencil
serif stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s sel Serif c) str

        a ->
            a


{-| Apply to a Stencil to render any text in the Stencil with a font in which every character has the same width so that that letters line up from line to line which is important in programming languages like Elm.
-}
fixedwidth : Stencil -> Stencil
fixedwidth stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s sel FixedWidth c) str

        a ->
            a


{-| Use a font of your choosing by specifying its string name. Use this sparingly as support for each font
will vary accross browsers and devices.
-}
customFont : String -> Stencil -> Stencil
customFont fStr stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s sel (Custom fStr) c) str

        a ->
            a



--Transformation functions


{-| Rotate a Shape by the specified amount (in radians). Use the "degrees" function to convert
from degrees into radians.
-}
rotate : Float -> Shape notification -> Shape notification
rotate theta shape =
    Rotate (vF theta) shape


{-| Move a Shape by a number of units in x and y.
-}
move : ( Float, Float ) -> Shape notification -> Shape notification
move (x,y) shape =
    Move (vF x, vF y) shape


{-| Scale a Shape by a given factor.
-}
scale : Float -> Shape notification -> Shape notification
scale s shape =
    ScaleXY (vF s) (vF s) shape


{-| Scale a Shape in the x-axis by a given factor.
-}
scaleX : Float -> Shape notification -> Shape notification
scaleX s shape =
    ScaleXY (vF s) 1 shape


{-| Scale a Shape in the y-axis by a given factor.
-}
scaleY : Float -> Shape notification -> Shape notification
scaleY s shape =
    ScaleXY 1 (vF s) shape


{-| Flip a Shape along the x-axis.
-}
mirrorX : Shape notification -> Shape notification
mirrorX shape =
    ScaleXY -1 1 shape


{-| Flip a shape along the y-axis.
-}
mirrorY : Shape notification -> Shape notification
mirrorY shape =
    ScaleXY 1 -1 shape


{-| Combine n number of Shape types into one Shape that can be
transformed as one.
-}
group : List (Shape notification) -> Shape notification
group shapes =
    Group shapes


{-| Create a custom colour given its red, green and blue components.
-}
rgb : Float -> Float -> Float -> Color
rgb r g b =
    RGBA (vF r) (vF g) (vF b) 1


{-| Create a custom colour given its red, green, blue and alpha components.
Alpha is a Float from 0 to 1 representing the Shape's level of transparency.
-}
rgba : Float -> Float -> Float -> Float -> Color
rgba r g b a =
    RGBA (vF r) (vF g) (vF b) (vF a)


pairToString : ( a, b ) -> String
pairToString ( x, y ) =
    (toString x) ++ "," ++ (toString y)


createBezierString : ( a, b ) -> List ( ( c, d ), ( e, f ) ) -> String
createBezierString first list =
    "M " ++ (pairToString first) ++ String.concat (List.map bezierStringHelper list)


bezierStringHelper : ( ( a, b ), ( c, d ) ) -> String
bezierStringHelper ( ( a, b ), ( c, d ) ) =
    " Q " ++ pairToString ( a, b ) ++ " " ++ pairToString ( c, d )


mkAlpha : Color -> String
mkAlpha (RGBA _ _ _ a) =
    toString a


mkRGB : Color -> String
mkRGB (RGBA r g b _) =
    "#" ++ (toHex <| round r) ++ (toHex <| round g) ++ (toHex <| round b)


toHex : Int -> String
toHex dec =
    let
        first =
            dec // 16

        second =
            (dec % 16)
    in
        (toHexHelper first) ++ (toHexHelper second)


toHexHelper : Int -> String
toHexHelper dec =
    case dec of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "A"

        11 ->
            "B"

        12 ->
            "C"

        13 ->
            "D"

        14 ->
            "E"

        15 ->
            "F"

        _ ->
            ""


{-| Create a custom colour given its hue, saturation and light components.
-}
hsl : Float -> Float -> Float -> Color
hsl h s l =
    case (convert (vF h) (vF s) (vF l) ) of
        ( r, g, b ) ->
            RGBA r g b 1


{-| Create a custom colour given its hue, saturation, light and alpha components.
Alpha is a Float from 0-1 representing the Shape's level of transparency.
lp
-}
hsla : Float -> Float -> Float -> Float -> Color
hsla h s l a =
    case (convert (vF h) (vF s) (vF l)) of
        ( r, g, b ) ->
            RGBA r g b (vF a)



{-
   - Jack You
-}
{-
   - use for converting values (just use this)
-}


convert : Float -> Float -> Float -> ( Float, Float, Float )
convert hue sat lit =
    let
        hue_ =
            modFloat hue (6.28318530718)

        rgb_ =
            toRGB_ hue_ sat lit

        chroma =
            findChroma lit sat

        m =
            findM lit chroma
    in
        mapTriple (\x -> x * 255) (mapTriple (\x -> x + m) rgb_)


findChroma : Float -> Float -> Float
findChroma lit sat =
    (1 - abs (2 * lit - 1)) * sat


findHue_ : Float -> Float
findHue_ hue =
    hue / (degrees 60)


findX : Float -> Float -> Float
findX chroma hue =
    chroma * (1 - abs ((modFloat (findHue_ hue) 2) - 1))


findM : Float -> Float -> Float
findM lit chroma =
    lit - 0.5 * chroma


toRGB_ : Float -> Float -> Float -> ( Float, Float, Float )
toRGB_ hue sat lit =
    let
        chroma =
            findChroma lit sat

        hue_ =
            findHue_ hue

        x =
            findX chroma hue
    in
        if hue_ >= 0 && hue_ < 1 then
            ( chroma, x, 0 )
        else if hue_ >= 1 && hue_ < 2 then
            ( x, chroma, 0 )
        else if hue_ >= 2 && hue_ < 3 then
            ( 0, chroma, x )
        else if hue_ >= 3 && hue_ < 4 then
            ( 0, x, chroma )
        else if hue_ >= 4 && hue_ < 5 then
            ( x, 0, chroma )
        else if hue_ >= 5 && hue_ < 6 then
            ( chroma, 0, x )
        else
            ( 0, 0, 0 )



{- Helper Functions -}


modFloat : Float -> Float -> Float
modFloat x m =
    x - m * Basics.toFloat (floor (vF x / m))


mapTriple : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapTriple f ( a1, a2, a3 ) =
    ( f a1, f a2, f a3 )




vF : Float -> Float --this function validates floats. Turns NaN and Infinity into 0 for crash avoidance! - A
vF f = 
    if (isNaN f || isInfinite f) then
        0
    else
        f


--


{-| -}
pink : Color
pink =
    RGBA 255 105 180 1


{-| -}
hotPink : Color
hotPink =
    RGBA 255 0 66 1


{-| -}
lightRed : Color
lightRed =
    RGBA 239 41 41 1


{-| -}
red : Color
red =
    RGBA 204 0 0 1


{-| -}
darkRed : Color
darkRed =
    RGBA 164 0 0 1


{-| -}
lightOrange : Color
lightOrange =
    RGBA 252 175 62 1


{-| -}
orange : Color
orange =
    RGBA 245 121 0 1


{-| -}
darkOrange : Color
darkOrange =
    RGBA 206 92 0 1


{-| -}
lightYellow : Color
lightYellow =
    RGBA 255 233 79 1


{-| -}
yellow : Color
yellow =
    RGBA 237 212 0 1


{-| -}
darkYellow : Color
darkYellow =
    RGBA 196 160 0 1


{-| -}
lightGreen : Color
lightGreen =
    RGBA 138 226 52 1


{-| -}
green : Color
green =
    RGBA 115 210 22 1


{-| -}
darkGreen : Color
darkGreen =
    RGBA 78 154 6 1


{-| -}
lightBlue : Color
lightBlue =
    RGBA 114 159 207 1


{-| -}
blue : Color
blue =
    RGBA 52 101 164 1


{-| -}
darkBlue : Color
darkBlue =
    RGBA 32 74 135 1


{-| -}
lightPurple : Color
lightPurple =
    RGBA 173 127 168 1


{-| -}
purple : Color
purple =
    RGBA 117 80 123 1


{-| -}
darkPurple : Color
darkPurple =
    RGBA 92 53 102 1


{-| -}
lightBrown : Color
lightBrown =
    RGBA 233 185 110 1


{-| -}
brown : Color
brown =
    RGBA 193 125 17 1


{-| -}
darkBrown : Color
darkBrown =
    RGBA 143 89 2 1


{-| -}
black : Color
black =
    RGBA 0 0 0 1


{-| -}
white : Color
white =
    RGBA 255 255 255 1


{-| -}
lightGrey : Color
lightGrey =
    RGBA 238 238 236 1


{-| -}
grey : Color
grey =
    RGBA 211 215 207 1


{-| -}
darkGrey : Color
darkGrey =
    RGBA 186 189 182 1


{-| -}
lightGray : Color
lightGray =
    RGBA 238 238 236 1


{-| -}
gray : Color
gray =
    RGBA 211 215 207 1


{-| -}
darkGray : Color
darkGray =
    RGBA 186 189 182 1


{-| -}
lightCharcoal : Color
lightCharcoal =
    RGBA 136 138 133 1


{-| -}
charcoal : Color
charcoal =
    RGBA 85 87 83 1


{-| -}
darkCharcoal : Color
darkCharcoal =
    RGBA 46 52 54 1


{-| -}
blank : Color
blank =
    RGBA 0 0 0 0
