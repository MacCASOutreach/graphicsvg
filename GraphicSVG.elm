module GraphicSVG
    exposing
        ( Stencil
        , Shape
        , GraphicSVG
        , Color
        , LineType
        , Face
        , Font
        , collage
        , map
        , GraphicsProgram
        , graphicsApp
        , NotificationsProgram
        , notificationsApp
        , GetKeyState
        , Keys(..)
        , KeyState(..)
        , GameProgram
        , gameApp
        , CmdProgram
        , cmdApp
        , line
        , polygon
        , openPolygon
        , ngon
        , triangle
        , rightTriangle
        , isosceles
        , sideAngleSide
        , square
        , rect
        , rectangle
        , roundedRect
        , circle
        , oval
        , wedge
        , graphPaper
        , graphPaperCustom
        , Pull(..)
        , curve
        , curveHelper
        , solid
        , dotted
        , dashed
        , longdash
        , dotdash
        , custom
        , text
        , size
        , bold
        , italic
        , underline
        , strikethrough
        , selectable
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


# Cmd App

@docs cmdApp, CmdProgram


# Stencils

@docs line, polygon, openPolygon, ngon, triangle, rightTriangle, isosceles, sideAngleSide, square, rect, rectangle, roundedRect, circle, oval, wedge


# Creating Shapes from Stencils

@docs filled, outlined, addOutline, rgb, rgba, hsl, hsla


# Grouping Shapes

@docs group


# Curves

@docs curve, Pull, curveHelper


# Line Styles

@docs LineType, solid, dotted, dashed, longdash, dotdash, custom


# Text

@docs Face, Font, text, size, bold, italic, underline, strikethrough, centered, selectable, sansserif, serif, fixedwidth, customFont


# Transformations

@docs move, rotate, scale, scaleX, scaleY, mirrorX, mirrorY


# Notifications

@docs notifyTap, notifyTapAt, notifyEnter, notifyEnterAt, notifyLeave, notifyLeaveAt, notifyMouseMoveAt, notifyMouseDown, notifyMouseDownAt, notifyMouseUp, notifyMouseUpAt, notifyTouchStart, notifyTouchStartAt, notifyTouchEnd, notifyTouchEndAt, notifyTouchMoveAt


# Miscellaneous

@docs makeTransparent, addHyperlink


# Helpers

@docs graphPaper, graphPaperCustom, map


# Let there be colours!

@docs Color,black,blank,blue,brown,charcoal,darkBlue,darkBrown,darkCharcoal,darkGray,darkGreen,darkGrey,darkOrange,darkPurple,darkRed,darkYellow,gray,green,grey,hotPink,lightBlue,lightBrown,lightCharcoal,lightGray,lightGreen,lightGrey,lightOrange,lightPurple,lightRed,lightYellow,orange,pink,purple,red,white,yellow

-}

{- Library created by Chris Schankula and Dr. Christopher Anand
   for the McMaster University Computing and Software Outreach Program
   and CompSci 1JC3, with input and testing from the rest of the Outreach
   team.
   Last updated: June 16, 2017
-}

import Html
import Html.Attributes
import Html.Events
import Http as Http
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
import Tuple


{-| A primitive template representing the shape you wish to draw. This must be turned into
a `Shape` before being drawn to the screen with `collage` (see below).
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


{-| A filled, outlined, or filled and outlined object that can be drawn to the screen using `collage`.
-}
type Shape userMsg
    = Inked Color (Maybe ( LineType, Color )) Stencil
    | Move ( Float, Float ) (Shape userMsg)
    | Rotate Float (Shape userMsg)
    | ScaleXY Float Float (Shape userMsg)
    | Group (List (Shape userMsg))
    | Link String (Shape userMsg)
    | Tap userMsg (Shape userMsg)
    | TapAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | EnterShape userMsg (Shape userMsg)
    | EnterAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | Exit userMsg (Shape userMsg)
    | ExitAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | MouseDown userMsg (Shape userMsg)
    | MouseDownAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | MouseUp userMsg (Shape userMsg)
    | MouseUpAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | MoveOverAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | TouchStart userMsg (Shape userMsg)
    | TouchEnd userMsg (Shape userMsg)
    | TouchStartAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | TouchEndAt (( Float, Float ) -> userMsg) (Shape userMsg)
    | TouchMoveAt (( Float, Float ) -> userMsg) (Shape userMsg)


{-| To compose multiple pages or components which each have a Msg/view/update, we need to map messages.
 (Ask if you don't know what this means.)
-}
map : (a -> b) -> Shape (Msg a) -> Shape (Msg b)
map f sh =
    let
        ff mmsg =
            case mmsg of
                Graphics userMsg ->
                    Graphics (f userMsg)

                WindowResize a ->
                    WindowResize a

                ReturnPosition a b ->
                    ReturnPosition (f << a) b

                CollageSize a ->
                    CollageSize a

                InitTime a ->
                    InitTime a

                TickTime a ->
                    TickTime a

                KeyDown a ->
                    KeyDown a

                KeyUp a ->
                    KeyUp a
    in
        case sh of
            Inked fillClr lt stencil ->
                Inked fillClr lt stencil

            Move v shape ->
                Move v (map f shape)

            Rotate deg shape ->
                Rotate deg (map f shape)

            ScaleXY sx sy shape ->
                ScaleXY sx sy (map f shape)

            Link href shape ->
                Link href (map f shape)

            Tap msg shape ->
                Tap (ff msg) (map f shape)

            TapAt msg shape ->
                TapAt (ff << msg) (map f shape)

            EnterShape msg shape ->
                EnterShape (ff msg) (map f shape)

            EnterAt msg shape ->
                EnterAt (ff << msg) (map f shape)

            Exit msg shape ->
                Exit (ff msg) (map f shape)

            ExitAt msg shape ->
                ExitAt (ff << msg) (map f shape)

            MouseDown msg shape ->
                MouseDown (ff msg) (map f shape)

            MouseDownAt msg shape ->
                MouseDownAt (ff << msg) (map f shape)

            MouseUp msg shape ->
                MouseUp (ff msg) (map f shape)

            MouseUpAt msg shape ->
                MouseUpAt (ff << msg) (map f shape)

            MoveOverAt msg shape ->
                MoveOverAt (ff << msg) (map f shape)

            TouchStart msg shape ->
                TouchStart (ff msg) (map f shape)

            TouchEnd msg shape ->
                TouchEnd (ff msg) (map f shape)

            TouchStartAt msg shape ->
                TouchStartAt (ff << msg) (map f shape)

            TouchEndAt msg shape ->
                TouchEndAt (ff << msg) (map f shape)

            TouchMoveAt msg shape ->
                TouchMoveAt (ff << msg) (map f shape)

            Group shapes ->
                Group (List.map (map f) shapes)


{-| The `GraphicSVG` type alias represents the drawable surface of the window.

This type is only used to define a type signature for a user defined `view` as follows:

    view : GraphicSVG.GraphicSVG userMsg

for use with `graphicsApp` and as follows:

    view : Model -> GraphicSVG.GraphicSVG MyMsg

for use with `notificationsApp`, `gameApp` and `cmdApp`.

These assume that `Model` is the name of the user model type alias and
`MyMsg` is the name of the user message type. Simply substitute the names
actually used for these labels.

-}
type alias GraphicSVG userMsg =
    Collage (Msg userMsg)

{-| The `Color` type is used for filling or outlining a `Stencil`.
-}
type Color
    = RGBA Float Float Float Float

{-| The `LineType` type is used to define the appearance of an outline for a `Stencil`.
    `LineType` also defines the appearence of `line` and `curve`.
-}
type LineType
    = Solid Float
    | Broken (List ( Float, Float )) Float

{-| The `Face` type describes the appearance of a text `Stencil`.
-}
type Face
    = Face
        Float   -- size
        Bool    -- bold
        Bool    -- italic
        Bool    -- underline
        Bool    -- strikethrough
        Bool    -- selectable
        Font   
        Bool    -- centred

{-| The `Font` type describes the font of a text `Stencil`.
-}
type Font
    = Serif
    | Sansserif
    | FixedWidth
    | Custom String


{-| To make it easier to read the code defining a `curve`,
and to make sure we always use the right number of curve points
and pull points (which is one more curve point than pull points),
we define a special `Pull` type, whose first point is the point
we pull towards, and second point is the end point for this
curve segments.
-}
type Pull
    = Pull ( Float, Float ) ( Float, Float )


{-| The possible states when you ask for a key's state:

* `JustDown` is the frame after the key went down (will show up exactly once per press)
* `Down` is a press that is continuing for more than one frame
* `JustUp` is the frame after the key went up / stopped being pressed (will show up exactly once per press)
* `Up` means the key is not currently being pressed nor was it recently released

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
static (they don't move) and cannot be interacted with. This is great for beginners
or for when only need static graphics are needed. Note that your `view` function is bare,
with no parameters:

    view = collage 500 500 
        [ 
            circle 10 |> filled red
        ]

`graphicsApp` takes a parameter like `{ view = view }`
so the main program that would get the whole thing started for the above
`view` would be:

    main =
        graphicsApp { view = view }

-}
graphicsApp : JustGraphics a -> GraphicsProgram a
graphicsApp input =
    Html.program
        { init = ( ( 0, initHiddenModel () ), initialSizeCmd [] input.view )
        , update = blankUpdate
        , view = blankView input.view
        , subscriptions = \_ -> Window.resizes sizeToMsg
        }


{-| The `JustGraphics` type alias is a simple record that contains the pointer to
the users view constant, which `view` does not take any arguments and returns
a `GraphicsProgram` type.
-}
type alias JustGraphics a =
    { view : Collage (Msg a) }


{-| This type alias is only used as a target for a user `main` type signature
to make the type signature more clear and concise when `main` calls
`graphicsApp`:

    main : GraphicsProgram userMsg
    main =
        graphicsApp { view = view }

Note that `userMsg` can be anything as no messages are used in this type of program.

-}
type alias GraphicsProgram userMsg =
    Program Never ( Int, HiddenModel () ) (Msg userMsg)


{-| Like `graphicsApp`, but you can add interactivity to your graphics by using the
`notify*` functions. This allows you to learn Elm's architecture in a fun way with
graphics. Note that your `view` function needs a `model` parameter now, which in this
example is the colour of the shape:

    view model =
        collage 500 500
            [ 
                circle 10 |> filled model |> notifyTap Change
            ]

`notificationApp` takes a parameter like:

    {
      model = model
    , view = view
    , update = update
    }

so the functions that would be required to make the above `view` function work
are as follows:

    type Msg
        = Change

    update msg model =
        case msg of
            Change ->
                green

    main =
        notificationsApp
            { model = red -- causes circle to start red
            , update = update -- function which changes the model
            , view = view
            }

which will cause the drawn red circle to change to green the first time
it is mouse clicked or tapped.

-}
notificationsApp : GraphicsApp model userMsg -> NotificationsProgram model userMsg
notificationsApp input =
    Html.program
        { init = ( ( input.model, initHiddenModel () ), initialSizeCmd [] (input.view input.model) )
        , update = hiddenUpdate input.update
        , view = hiddenView input.view
        , subscriptions = \_ -> Window.resizes sizeToMsg
        }


{-| This type alias is only used as a target for a user `main` type signature
to make the type signature more clear and concise when `main` calls
`notificationsApp`:

    main : NotificationProgram Model MyMsg
    main =
        notificationsApp { model = init, update = update, view = view }

where `Model` is the type alias of the user persistent model, and
`MyMsg` is the name of the user defined message type;
if other names are used, they can be substituted for these names.

-}
type alias NotificationsProgram model userMsg =
    Program Never ( model, HiddenModel () ) (Msg userMsg)


{-| Automatically maps time and keyboard presses to your program. This should
be all you need for making complex interactive games and animations.
`gameApp` takes two parameters: one is your own type of `InputHandler` message
which will be automatically called each time the browser window is refreshed
(30 times per second)
of the form `Float -> GetKeyState -> UserMsg` and the other is

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
        collage 500 500
            [ line ( 0, 0 ) ( 250, 0 )
                |> outlined (solid 1) green
                |> rotate (degrees model)
            ]

    main =
        gameApp Tick { 
                       model = init
                     , update = update
                     , view = view 
                     }

-}
gameApp : InputHandler userMsg -> GraphicsApp model userMsg -> GameProgram model userMsg
gameApp tickMsg input =
    Html.program
        { init = ( ( input.model, initHiddenModel tickMsg ), initialSizeCmd [] (input.view input.model) )
        , update = hiddenGameUpdate input.update
        , view = hiddenView input.view
        , subscriptions = subs []
        }


{-| The `InputHandler` type alias descripts a message that contains a Float representing the time in seconds from
the time the program started and the `GetKeyState` type alias used for returning key actions.
-}
type alias InputHandler userMsg =
    Float
    -> GetKeyState
    -> userMsg


{-| The `GraphicsApp` type alias is a record that contains the
the initial state of the user-defined model of any type, the
user's `update` function which takes arguments of the message to be acted on
and the current state of the model and returns the new state of the model, and
the `view` function, which takes one argument of the current state of the model
and returns a `Collage` type.
-}
type alias GraphicsApp model userMsg =
    { model : model, update : userMsg -> model -> model, view : model -> Collage (Msg userMsg) }


{-| This type alias is only used as a target for a user `main` type signature to make
the type signature more clear and concise when `main` calls `gameApp`:

    main : GamesProgram Model Msg
    main =
        gameApp Tick { 
                       model = init
                     , update = update
                     , view = view 
                     }

where `Tick` is the message handler called once per browser window update,
`Model` is the type alias of the user persistent model, and
`Msg` is the name of the user message type; if other names are used,
they can be substituted for these names.

-}
type alias GameProgram model userMsg =
    Program Never ( model, HiddenModel (InputHandler userMsg) ) (Msg userMsg)


{-| Advanced Function Warning! cmdApp takes two parameters: one is your own type of the form `Float -> GetKeyState -> CustomMsg` and the other is
    
    {
      init = (model, cmd)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

This matches the Elm architecture and is analogous to `Html.program`.
-}
cmdApp : InputHandler userMsg -> CmdApp model userMsg -> CmdProgram model userMsg
cmdApp tickMsg input =
    Html.program
        { init =
            ( ( Tuple.first input.init, initHiddenModel tickMsg )
            , initialSizeCmd [ Cmd.map (\cmdMap -> Graphics cmdMap) (Tuple.second input.init) ]
                (input.view (Tuple.first input.init))
            )
        , update = hiddenCmdUpdate input.update
        , view = hiddenCmdView input.view
        , subscriptions = subs [ Sub.map (\sub -> Graphics sub) (input.subscriptions (Tuple.first input.init)) ]
        }


type alias CmdApp model userMsg =
    { init : ( model, Cmd userMsg )
    , update : userMsg -> model -> ( model, Cmd userMsg )
    , view : model -> Collage (Msg userMsg)
    , subscriptions : model -> Sub userMsg
    }


{-| This type alias is only used as a target for a user `main` type signature to make
the type signature more clear and concise when `main` calls `cmdApp`:

    main : CmdProgram Model Msg
    main =
        cmdApp Tick { model = init, update = update, view = view, subscriptions = subscriptions }

where `Tick` is a message handler called once per browser window update,
`Model` is the type alias of the user persistent model, and
`Msg` is the name of the user message type; if other names are used,
they can just be substituted for these names.

-}
type alias CmdProgram model userMsg =
    Program Never ( model, HiddenModel (InputHandler userMsg) ) (Msg userMsg)


subs : List (Sub (Msg userMsg)) -> a -> Sub (Msg userMsg)
subs extraSubs model =
    Sub.batch
        ([ Time.every (1000 / 30 * millisecond) (createTimeMessage)
         , Window.resizes sizeToMsg
         ]
            ++ keySubs
            ++ extraSubs
        )


keySubs : List (Sub (Msg userMsg))
keySubs =
    [ Keyboard.ups (KeyUp), Keyboard.downs (KeyDown) ]


createTimeMessage : Time -> Msg userMsg
createTimeMessage t =
    let
        time =
            inSeconds t
    in
        TickTime time


blankUpdate :
    Msg userMsg
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


hiddenCmdUpdate :
    (userMsg -> model -> ( model, Cmd userMsg ))
    -> Msg userMsg
    -> ( model, HiddenModel (InputHandler userMsg) )
    -> ( ( model, HiddenModel (InputHandler userMsg) ), Cmd (Msg userMsg) )
hiddenCmdUpdate update msg ( model, gModel ) =
    let
        mapUserCmd cmd =
            Cmd.map (\cmd -> Graphics cmd) cmd
    in
        case msg of
            Graphics message ->
                let
                    ( newModel, userCmds ) =
                        update message model
                in
                    ( ( newModel, gModel ), mapUserCmd userCmds )

            WindowResize ( width, height ) ->
                ( ( model, { gModel | sw = Basics.toFloat width, sh = Basics.toFloat height } ), Cmd.none )

            ReturnPosition message ( x, y ) ->
                let
                    ( newModel, userCmds ) =
                        update (message (convertCoords ( x, y ) gModel)) model
                in
                    ( ( newModel, gModel ), mapUserCmd userCmds )

            CollageSize ( width, height ) ->
                ( ( model, { gModel | cw = Basics.toFloat width, ch = Basics.toFloat height } ), Cmd.none )

            InitTime t ->
                ( ( model, { gModel | initT = t } ), Cmd.none )

            TickTime t ->
                let
                    ( newModel, userCmds ) =
                        update (gModel.updateTick (t - gModel.initT) ( (keyCheckerFunction gModel.keys), arrowKeys (keyCheckerFunction gModel.keys), wasdKeys (keyCheckerFunction gModel.keys) )) model
                in
                    ( ( newModel, { gModel | keys = maintainKeyDict gModel.keys } ), mapUserCmd userCmds )

            KeyDown n ->
                ( ( model, { gModel | keys = insertKeyDict gModel.keys n WentDown } ), Cmd.none )

            KeyUp n ->
                ( ( model, { gModel | keys = insertKeyDict gModel.keys n WentUp } ), Cmd.none )


blankView : Collage userMsg -> ( a, b ) -> Html.Html userMsg
blankView view ( model, gModel ) =
    case view of
        Collage ( w, h ) shapes ->
            createCollage w h shapes


hiddenView : (a -> Collage userMsg) -> ( a, b ) -> Html.Html userMsg
hiddenView view ( model, gModel ) =
    case (view model) of
        Collage ( w, h ) shapes ->
            createCollage w h shapes


hiddenCmdView view ( model, gModel ) =
    case (view model) of
        Collage ( w, h ) shapes ->
            createCollage w h shapes


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


initialSizeCmd :
    List (Cmd (Msg userMsg))
    -> Collage (Msg userMsg)
    -> Cmd (Msg userMsg)
initialSizeCmd otherCmds userView =
    Cmd.batch
        ([ Task.perform sizeToMsg Window.size
         , Task.perform getCollageSize (Task.succeed userView)
         , Task.perform getInitTime Time.now
         ]
            ++ otherCmds
        )


getInitTime : Time -> Msg userMsg
getInitTime t =
    InitTime (inSeconds t)


sizeToMsg : Window.Size -> Msg userMsg
sizeToMsg size =
    WindowResize ( size.width, size.height )


getCollageSize : Collage (Msg userMsg) -> Msg userMsg
getCollageSize userView =
    case userView of
        Collage ( w, h ) _ ->
            CollageSize ( round w, round h )


{-| The `Msg` type encapsulates all GraphicSVG internal messages.

This type is only used to define type signature fors user defined
`view` and `main` as follows:

    view : GraphicSVG.Collage (GraphicSVG.Msg Msg)

for use with `graphicsApp` and `notificationsApp`, and as follows:

    view : Model -> GraphicSVG.Collage (GraphicSVG.Msg Msg)

for use with `gameApp`.

It is also used to define the type signature for
a user supplied `main` as follows:

    main : Program Never (GraphicsModel Model Msg) (GraphicSVG.Msg Msg)

for use with `graphicsApp` and `notificationsApp`, and as follows:

    main : Program Never (GamesModel Model Msg) (GraphicSVG.Msg Msg)

for use when `main` calls `gameApp`

These assume that `Model` is the type alias of the user persistent model, and
`Msg` is the name of the user message type.

-}
type Msg userMsg
    = Graphics userMsg
    | WindowResize ( Int, Int )
    | ReturnPosition (( Float, Float ) -> userMsg) ( Float, Float )
    | CollageSize ( Int, Int )
    | InitTime Time
    | TickTime Time
    | KeyDown Int
    | KeyUp Int


aHiddenUpdate : (a -> b -> c) -> a -> b -> ( c, Cmd msg )
aHiddenUpdate update msg model =
    ( update msg model, Cmd.none )


aHiddenView : (a -> b) -> a -> b
aHiddenView view model =
    view model


{-| The `HiddenModel` type alias encapsulates the GraphicSVG internal model
which is not exposed to user code.
-}
type alias HiddenModel inputHandler =
    { cw : Float
    , ch : Float
    , sw : Float
    , sh : Float
    , initT : Float
    , updateTick : inputHandler
    , keys : KeyDict
    }


initHiddenModel userTick =
    { cw = 0
    , ch = 0
    , sw = 0
    , sh = 0
    , initT = 0
    , updateTick = userTick
    , keys = Dict.empty
    }


type alias KeyDict =
    Dict.Dict KeyCode ( KeyState, Bool )


{-| `GetKeyState` returns a triple where the first argument is of type `Keys -> KeyState`
so you can ask if a certain key is pressed. The other two are tuples of arrow keys and
WASD keys, respectively. They're in the form (x,y) which represents the key presses
of each player. For example, (0,-1) represents the left arrow (or "A") key, and (1,1)
would mean the up (or "W") and right (or "D") key are being pressed at the same time.
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


{-| Create a line from a point to a point. Use `outlined` to convert to a viewable
`Shape`.
-}
line : ( Float, Float ) -> ( Float, Float ) -> Stencil
line p1 p2 =
    Path [ p1, p2 ]


{-| Create a closed shape given a list of points. Can use `outlined` or `filled` to
convert to a `Shape`.
-}
polygon : List ( Float, Float ) -> Stencil
polygon ptList =
    Polygon ptList


{-| Create an open shape given a list of points. Unlike with polygon, the first and
last points will not join up automatically. Can use `outlined` or `filled` to
convert to a `Shape`.
-}
openPolygon : List ( Float, Float ) -> Stencil
openPolygon ptList =
    Path ptList


{-| Create a regular polygon with a given number of sides and radius. Examples:

    ngon 3 50 -- triangle
    ngon 5 50 -- pentagon
    ngon 8 50 -- octogon

-}
ngon : Int -> Float -> Stencil
ngon n r =
    Polygon <| List.map (ptOnCircle r (Basics.toFloat n) << Basics.toFloat) (List.range 0 n)


{-| Synonym for `ngon 3`. Creates a triangle with a given size.
-}
triangle : Float -> Stencil
triangle r =
    ngon 3 r


{-| Creates a right-angled triangle with a given base and height.
-}
rightTriangle : Float -> Float -> Stencil
rightTriangle base height = 
    polygon [(0,0),(base,0),(0,height)]


{-| Creates an isosceles triangle with a given base and height.
-}
isosceles : Float -> Float -> Stencil
isosceles base height =
    polygon [(0,0),(base,0),(base/2, height)]


{-| Creates a triangle given two side lengths and the angle between them.

For example, `sideAngleSide 30 (degrees 45) 50` creates a triangle with side lengths 
30 and 50 with an angle of 45 degrees between them.
-}
sideAngleSide : Float -> Float -> Float -> Stencil
sideAngleSide sideOne angle sideTwo =
    polygon [(0,0),(sideOne,0),sideTwoPoint angle sideTwo]


sideTwoPoint : Float -> Float -> (Float,Float)
sideTwoPoint angle sideTwo = (cos(angle) * sideTwo,sin(angle) * sideTwo )


{-| Creates a square with a given side length. (Synonym for `rect s s`)
-}
square : Float -> Stencil
square r =
    Rect r r


{-| Creates a rectangle with a given width and height.
-}
rect : Float -> Float -> Stencil
rect w h =
    Rect w h


{-| Synonym for `rect`.
-}
rectangle : Float -> Float -> Stencil
rectangle w h =
    Rect w h


{-| Creates a rectangle with a given width, height, and circular rounded
corners with the given radius.
-}
roundedRect : Float -> Float -> Float -> Stencil
roundedRect w h r =
    RoundRect w h r


{-| Creates a circle with a given radius.
-}
circle : Float -> Stencil
circle r =
    Circle r


{-| Creates an oval with a given width and height.
-}
oval : Float -> Float -> Stencil
oval w h =
    Oval w h


{-| Creates a graph paper with squares of a given size.
-}
graphPaper : Float -> Shape userMsg
graphPaper s =
    graphPaperCustom s 1 (rgb 135 206 250)


{-| Creates graph paper with squares of a given size, with a user-defined thickness and colour.
-}
graphPaperCustom : Float -> Float -> Color -> Shape userMsg
graphPaperCustom s th c =
    let
        sxi =
            round (1500 / s)

        syi =
            round (800 / s)

        xlisti =
            List.range -sxi sxi

        ylisti =
            List.range -syi syi
    in
        group
            (List.map (createGraphX 1600 s th c << Basics.toFloat) xlisti
                ++ List.map (createGraphY 3000 s th c << Basics.toFloat) ylisti
            )


createGraphX : Float -> Float -> Float -> Color -> Float -> Shape userMsg
createGraphX h s th c x =
    filled c (rect th h) |> move ( x * s, 0 )


createGraphY : Float -> Float -> Float -> Color -> Float -> Shape userMsg
createGraphY w s th c y =
    filled c (rect w th) |> move ( 0, y * s )


{-| Creates a wedge with a given radius, and a given fraction of a circle.
    
    wedge 50 0.5 -- semi-circle
    wedge 50 0.25 -- quarter-circle
    wedge 50 0.75 -- three-quarter circle
-}
wedge : Float -> Float -> Stencil
wedge r frac =
    let
        n =
            frac * 360 / 10 + 5

        ni =
            round n
    in
        Polygon <|
            if frac > 0 then
                [ ( 0, 0 ), wedgeHelper r (-frac * 180) ]
                    ++ (List.map ((wedgeHelper r) << ((*) (frac / n * 180)) << Basics.toFloat) (List.range -ni ni))
                    ++ [ wedgeHelper r (frac * 180), ( 0, 0 ) ]
            else
                []


wedgeHelper : Float -> Float -> ( Float, Float )
wedgeHelper r cn =
    let
        angle =
            cn
    in
        ( r * cos (degrees angle), r * sin (degrees angle) )


ptOnCircle : Float -> Float -> Float -> ( Float, Float )
ptOnCircle r n cn =
    let
        angle =
            360 * cn / n
    in
        ( r * cos (degrees angle), r * sin (degrees angle) )


{-| Creates a curve starting at a point, pulled towards a point, ending at a third point. For example,

    curve (0,0) [Pull (0,10) (0,20)]

gives a curve starting at (0,0), pulled towards (0,10) and ending at (0,20).

Think about curves as what you get when you take a bunch of
bendy sticks with their ends glued down to a board, and then pulling each stick
towards another point.
You always need an initial point and at least one `Pull`, but you can add as many `Pull`s as you want to
add additional curve segments, but each curve segment can only bend one way, since it is pulled in one direction.
-}
curve : ( Float, Float ) -> List Pull -> Stencil
curve ( a, b ) list =
    BezierPath ( a, b ) (List.map curveListHelper list)


curveListHelper : Pull -> ( ( Float, Float ), ( Float, Float ) )
curveListHelper (Pull ( a, b ) ( c, d )) =
    ( ( a, b ), ( c, d ) )


{-| Add a hyperlink to any `Shape`:

    circle 10
        |> filled red
        |> addHyperLink "http://outreach.mcmaster.ca"
-}
addHyperlink : String -> Shape userMsg -> Shape userMsg
addHyperlink link shape =
    Link link shape


{-| Creates a text `Stencil`. You can change this `Stencil` using the text helper
functions. Note that `|> filled ...` or `|> outlined ...` must go at the *end* of the text helper functions
(ie note that all these functions have type `Stencil -> Stencil`). For example,

    text "Hello World"
        |> fixedwidth
        |> bold
        |> size 14
        |> filled black
-}
text : String -> Stencil
text str =
    Text (Face 12 False False False False False Serif False) str


{-| Apply to a curve or group of curves in order to view their start points,
end points and `Pull` points. Helpful while perfecting curves.

    curve (0,0) [Pull (0,10) (0,20)]
        |> curveHelper
-}
curveHelper : Shape userMsg -> Shape userMsg
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
    -> Shape userMsg
generateCurveHelper ( a, b ) list =
    let
        l1Array =
            Array.fromList ([ ( a, b ) ] ++ List.concat (List.map createTopLevelList list))
    in
        group [ generateCHLines l1Array, generateCHCircles l1Array ]


generateCHLines : Array.Array ( number, number1 ) -> Shape userMsg
generateCHLines ar =
    let
        len =
            Array.length ar
    in
        group (List.map (generateCHLine ar) (List.range 0 (len - 2)))


generateCHLine : Array.Array ( number, number1 ) -> Int -> Shape userMsg
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


generateCHCircles : Array.Array ( number, number1 ) -> Shape userMsg
generateCHCircles ar =
    let
        len =
            Array.length ar
    in
        group (List.map (generateCHCircle ar) (List.range 0 (len - 1)))


generateCHCircle : Array.Array ( number, number1 ) -> Int -> Shape userMsg
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
type Collage userMsg
    = Collage ( Float, Float ) (List (Shape userMsg))


{-| Creates a blank canvas on which you can draw. Takes a width, height and a
list of `Shape`s. Use this in your `view` functions in the three types of Apps above:

    collage 500 500 
        [ 
            circle 10 |> filled red
        ]
-}
collage : Float -> Float -> List (Shape userMsg) -> Collage userMsg
collage w h shapes =
    Collage ( w, h ) shapes


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
    -> List ( Float, Shape userMsg )
    -> Collage userMsg
puppetShow w h listShapes =
    collage w h (List.map extractShape (List.sortWith flippedComparison listShapes))


extractShape : ( Float, Shape userMsg ) -> Shape userMsg
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


{-| Receive a message (`userMsg`) when a `Shape` is clicked or tapped.
-}
notifyTap : userMsg -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyTap msg shape =
    Tap (Graphics msg) shape


{-| Receive a message (`userMsg`) with the x and y position of the mouse or finger when the `Shape` is clicked or tapped.
-}
notifyTapAt : (( Float, Float ) -> userMsg) -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyTapAt msg shape =
    TapAt (ReturnPosition msg) shape


{-| Receive a message (`userMsg`) when the mouse enters a `Shape`.
-}
notifyEnter : userMsg -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyEnter msg shape =
    EnterShape (Graphics msg) shape


{-| Receive a message (`userMsg`) with the x and y position of the mouse when the mouse enters a `Shape`.
-}
notifyEnterAt : (( Float, Float ) -> userMsg) -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyEnterAt msg shape =
    EnterAt (ReturnPosition msg) shape


{-| Receive a message (`userMsg`) when the mouse leaves a `Shape`.
-}
notifyLeave : userMsg -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyLeave msg shape =
    Exit (Graphics msg) shape


{-| Receive a message (`userMsg`) with the x and y position of the mouse when the mouse leaves a `Shape`.
-}
notifyLeaveAt : (( Float, Float ) -> userMsg) -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyLeaveAt msg shape =
    ExitAt (ReturnPosition msg) shape


{-| Receive a message (`userMsg`) with the x and y position of the mouse when the mouse is moved across a `Shape`.
-}
notifyMouseMoveAt : (( Float, Float ) -> userMsg) -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyMouseMoveAt msg shape =
    MoveOverAt (ReturnPosition msg) shape


{-| Receive a message (`userMsg`) when the mouse button is pressed while the cursor is over a `Shape`.
-}
notifyMouseDown : userMsg -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyMouseDown msg shape =
    MouseDown (Graphics msg) shape


{-| Receive a message (`userMsg`) with the x and y position of the mouse when the mouse button is pressed while the cursor is over a `Shape`.
-}
notifyMouseDownAt : (( Float, Float ) -> userMsg) -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyMouseDownAt msg shape =
    MouseDownAt (ReturnPosition msg) shape


{-| Receive a message (`userMsg`) when the mouse button is released while the cursor is over a `Shape`.
-}
notifyMouseUp : userMsg -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyMouseUp msg shape =
    MouseUp (Graphics msg) shape


{-| Receive a message (`userMsg`) with the x and y position of the mouse when the mouse button is released while the cursor is over a `Shape`.
-}
notifyMouseUpAt : (( Float, Float ) -> userMsg) -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyMouseUpAt msg shape =
    MouseUpAt (ReturnPosition msg) shape


{-| Receive a message (`userMsg`) when the user begins touching a `Shape`.
-}
notifyTouchStart : userMsg -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyTouchStart msg shape =
    TouchStart (Graphics msg) shape


{-| Receive a message (`userMsg`) with the x and y position of the user's finger when the user begins touching a `Shape`.
-}
notifyTouchStartAt : (( Float, Float ) -> userMsg) -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyTouchStartAt msg shape =
    TouchStartAt (ReturnPosition msg) shape


{-| Receive a message (`userMsg`) when the user lifts their finger off a `Shape`.
-}
notifyTouchEnd : userMsg -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyTouchEnd msg shape =
    TouchEnd (Graphics msg) shape


{-| Receive a message (`userMsg`) with the x and y position of the user's finger when the user lifts their finger off a `Shape`.
-}
notifyTouchEndAt : (( Float, Float ) -> userMsg) -> Shape (Msg userMsg) -> Shape (Msg userMsg)
notifyTouchEndAt msg shape =
    TouchEndAt (ReturnPosition msg) shape


{-| Receive a message (`userMsg`) with the x and y position of the user's finger when the user moves their finger over a `Shape`.
-}
notifyTouchMoveAt : (( Float, Float ) -> userMsg) -> Shape (Msg userMsg) -> Shape (Msg userMsg)
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
                                    "text-decoration: line-through;"
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
                                        "sans-serif;"

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
                            Svg.text_ ([ x "0", y "0", Svg.Attributes.style sty, Svg.Attributes.fontSize (toString (si)), Svg.Attributes.textAnchor anchor, Html.Attributes.contenteditable True ] ++ attrs ++ [ Svg.Attributes.transform <| "matrix(" ++ (String.concat <| List.intersperse "," <| List.map toString [ a, -b, -c, d, tx, -ty ]) ++ ")" ] ++ [Svg.Attributes.xmlSpace "preserve"]) [ Svg.text str ]
                )

        Move v shape ->
            createSVG (moveT trans v) shape

        Rotate deg shape ->
            createSVG (rotT trans deg) shape

        ScaleXY sx sy shape ->
            createSVG (scaleT trans ( sx, sy )) shape
            
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


{-| Fill a `Stencil` with a `Color`, creating a `Shape`.

    circle 10
        |> filled red
-}
filled : Color -> Stencil -> Shape userMsg
filled color shape =
    Inked color Nothing shape


{-| Outline a Stencil with a `LineType` and `Color`, creating a `Shape`.

    circle 10
        |> outlined (solid 5) red
-}
outlined : LineType -> Color -> Stencil -> Shape userMsg
outlined style outlineClr shape =
    let
        lineStyle =
            ( style, outlineClr )
    in
        Inked (rgba 0 0 0 0) (Just lineStyle) shape


{-| Add an outline to an already-filled `Shape`.

    circle 10
        |> filled red
        |> addOutline (solid 5) white
-}
addOutline : LineType -> Color -> Shape userMsg -> Shape userMsg
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


{-| Make a `Shape` transparent by the fraction given. Multiplies on top of other transparencies:

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
makeTransparent : Float -> Shape userMsg -> Shape userMsg
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

        Link s shape ->
            Link s (makeTransparent alpha shape)
        
        Tap userMsg shape ->
            Tap userMsg (makeTransparent alpha shape)
      
        TapAt userMsg shape ->
            TapAt userMsg (makeTransparent alpha shape)

        EnterShape userMsg shape ->
            EnterShape userMsg (makeTransparent alpha shape)
       
        EnterAt userMsg shape ->
            EnterAt userMsg (makeTransparent alpha shape)
       
        Exit userMsg shape ->
            Exit userMsg (makeTransparent alpha shape)

        ExitAt userMsg shape ->
            ExitAt userMsg (makeTransparent alpha shape)
    
        MouseDown userMsg shape ->
            MouseDown userMsg (makeTransparent alpha shape)
       
        MouseDownAt userMsg shape ->
            MouseDownAt userMsg (makeTransparent alpha shape)
       
        MouseUp userMsg shape ->
            MouseUp userMsg (makeTransparent alpha shape)

        MouseUpAt userMsg shape ->
            MouseUpAt userMsg (makeTransparent alpha shape)

        MoveOverAt userMsg shape ->
            MoveOverAt userMsg (makeTransparent alpha shape)

        TouchStart userMsg shape ->
            TouchStart userMsg (makeTransparent alpha shape)

        TouchEnd userMsg shape ->
            TouchEnd userMsg (makeTransparent alpha shape)

        TouchStartAt userMsg shape ->
            TouchStartAt userMsg (makeTransparent alpha shape)

        TouchEndAt userMsg shape ->
            TouchEndAt userMsg (makeTransparent alpha shape)

        TouchMoveAt userMsg shape ->
            TouchMoveAt userMsg (makeTransparent alpha shape)



--Line styles


{-| Define a solid `LineType` with the given width.
-}
solid : Float -> LineType
solid th =
    Solid th


{-| Define a dotted `LineType` with the given width.
-}
dotted : Float -> LineType
dotted th =
    Broken [ ( th, th ) ] th


{-| Define a dashed `LineType` with the given width. Dashes are short line segments, versus dots which are theoretically points, but may be drawn with very short line segments.
-}
dashed : Float -> LineType
dashed th =
    Broken [ ( th * 5, th * 2.5 ) ] th


{-| Define a dashed `LineType` with the given width, where the dashes are longer than normal.
-}
longdash : Float -> LineType
longdash th =
    Broken [ ( th * 12, th * 6 ) ] th


{-| Define a `LineType` with the given width, including alternating dots and dashes.
-}
dotdash : Float -> LineType
dotdash th =
    Broken [ ( th, th ), ( th * 5, th ) ] th


{-| A custom line defined by a list of (on,off):

    custom [(10,5)] 5 -- a line with dashes 10 long and spaces 5 long
    custom [(10,5),(20,5)] -- on for 10, off 5, on 20, off 5

-}
custom : List ( Float, Float ) -> Float -> LineType
custom list th =
    Broken list th


makePair : a -> ( a, a )
makePair n =
    ( n, n )



--Text functions


{-| Apply to a `text` `Stencil` to change the font size of the text.

The size has a unit called "points", which depends on the size and type of screen used, but try 12 to start.
-}
size : Float -> Stencil -> Stencil
size size stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face size bo i u s sel f c) str

        a ->
            a


{-| Apply to a `text` `Stencil` to make the text bold.
-}
bold : Stencil -> Stencil
bold stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si True i u s sel f c) str

        a ->
            a


{-| Apply to a `text` `Stencil` to make the text italicized (slanted).
-}
italic : Stencil -> Stencil
italic stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo True u s sel f c) str

        a ->
            a


{-| Apply to a `text` `Stencil` to underline the text.
-}
underline : Stencil -> Stencil
underline stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i True s sel f c) str

        a ->
            a


{-| Apply to a `text` `Stencil` to put a line through the centre of the text.
-}
strikethrough : Stencil -> Stencil
strikethrough stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u True sel f c) str

        a ->
            a


{-| Apply to a `text` `Stencil` to make the text selectable (so users can copy your text and paste it elsewhere).
-}
selectable : Stencil -> Stencil
selectable stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s True f c) str

        a ->
            a


{-| Apply to a `text` `Stencil` to centre the text.
-}
centered : Stencil -> Stencil
centered stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s sel f True) str

        a ->
            a


{-| Apply to a `text` `Stencil` to render the text with a Sans Serif font (ie one without thinner and thicker bits).
-}
sansserif : Stencil -> Stencil
sansserif stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s sel Sansserif c) str

        a ->
            a


{-| Apply to a `text` `Stencil` to render the text with a Serif font (ie one with thinner and thicker bits).
-}
serif : Stencil -> Stencil
serif stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s sel Serif c) str

        a ->
            a


{-| Apply to a text `Stencil` to render the text `Stencil` with a font in which every character has the same width. 
This will mean that the letters line up from line to line which is important in programming languages like Elm.
-}
fixedwidth : Stencil -> Stencil
fixedwidth stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s sel FixedWidth c) str

        a ->
            a


{-| Apply to a `text` `Stencil` to render the text with a font of your choosing by specifying its name in a `String`. 

*Use this sparingly as support for each font will vary across browsers and devices.*
-}
customFont : String -> Stencil -> Stencil
customFont fStr stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s sel (Custom fStr) c) str

        a ->
            a



--Transformation functions


{-| Rotate a `Shape` by the specified amount (in radians). Use the `degrees` function to convert
from degrees into radians:

    [
        rect 30 60
            |> filled blue
            |> rotate(degrees 30)
    ]
-}
rotate : Float -> Shape userMsg -> Shape userMsg
rotate theta shape =
    Rotate theta shape


{-| Move a `Shape` by a number of units along the x-axis and y-axis.
-}
move : ( Float, Float ) -> Shape userMsg -> Shape userMsg
move disp shape =
    Move disp shape


{-| Scale a `Shape` by a given factor.
-}
scale : Float -> Shape userMsg -> Shape userMsg
scale s shape =
    ScaleXY s s shape


{-| Scale a `Shape` in the x-axis by a given factor.
-}
scaleX : Float -> Shape userMsg -> Shape userMsg
scaleX s shape =
    ScaleXY s 1 shape


{-| Scale a `Shape` in the y-axis by a given factor.
-}
scaleY : Float -> Shape userMsg -> Shape userMsg
scaleY s shape =
    ScaleXY 1 s shape


{-| Flip a `Shape` along the x-axis.
-}
mirrorX : Shape userMsg -> Shape userMsg
mirrorX shape =
    ScaleXY -1 1 shape


{-| Flip a `Shape` along the y-axis.
-}
mirrorY : Shape userMsg -> Shape userMsg
mirrorY shape =
    ScaleXY 1 -1 shape


{-| Combine any number of `Shape` types into one `Shape` that can be
transformed (moved, rotated, scaled, etc) as one `Shape`.
-}
group : List (Shape userMsg) -> Shape userMsg
group shapes =
    Group shapes


{-| Define a colour given its red, green and blue components.
-}
rgb : Float -> Float -> Float -> Color
rgb r g b =
    RGBA r g b 1


{-| Define a colour given its red, green, blue and alpha components.
Alpha is a decimal number (`Float`) from 0 to 1 representing the level of transparency.
-}
rgba : Float -> Float -> Float -> Float -> Color
rgba r g b a =
    RGBA r g b a


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


{-| Define a colour given its hue, saturation and light components.
-}
hsl : Float -> Float -> Float -> Color
hsl h s l =
    case (convert h s l) of
        ( r, g, b ) ->
            RGBA r g b 1


{-| Define a colour given its hue, saturation, light and alpha components.
Alpha is a decimal number (`Float`) from 0 to 1 representing the level of transparency.
-}
hsla : Float -> Float -> Float -> Float -> Color
hsla h s l a =
    case (convert h s l) of
        ( r, g, b ) ->
            RGBA r g b a



{-
   Contributed by Jack You.
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
    x - m * Basics.toFloat (floor (x / m))


mapTriple : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapTriple f ( a1, a2, a3 ) =
    ( f a1, f a2, f a3 )



-- Colours


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
