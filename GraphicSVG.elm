module GraphicSVG
    exposing
        ( Stencil
        , Shape
        , collage
        , graphicsApp
        , notificationsApp
        , GetKeyState
        , Keys(..)
        , KeyState(..)
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
        , Pull (..)
        , curve
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
        ,black
        ,blank
        ,blue
        ,brown
        ,charcoal
        ,darkBlue
        ,darkBrown
        ,darkCharcoal
        ,darkGray
        ,darkGreen
        ,darkGrey
        ,darkOrange
        ,darkPurple
        ,darkRed
        ,darkYellow
        ,gray
        ,green
        ,grey
        ,hotPink
        ,lightBlue
        ,lightBrown
        ,lightCharcoal
        ,lightGray
        ,lightGreen
        ,lightGrey
        ,lightOrange
        ,lightPurple
        ,lightRed
        ,lightYellow
        ,orange
        ,pink
        ,purple
        ,red
        ,white
        ,yellow
        )

{-| A library for creating SVG graphics in a way that is compatible with Elm's
old Graphics library. Also includes built-in functions for creating games and
other applications including keyboard presses and mouse movements.
# Basic Types
@docs Stencil, Shape
# Rendering To Screen
@docs collage
# Graphics App
@docs graphicsApp
# Notifications App
@docs notificationsApp
# Game App
@docs GetKeyState, Keys, KeyState, gameApp
# Stencils
@docs line, polygon, openPolygon, ngon, triangle, square, rect, rectangle, roundedRect, circle, oval, wedge, graphPaper
# Creating Shapes by Filling and Outlining Stencils
@docs filled, outlined, addOutline, rgb, rgba, hsl, hsla
# Grouping Shapes
@docs group
# Curves
@docs curve, Pull
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
   Last updated: September 5th, 2016
-}

import Html
import Html.Attributes
import Html.App as App
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


{-| A filled object that can be drawn to the screen using collage.
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

    view = collage 500 500
        [ circle 10 |> filled red
        ]

graphicsApp takes a parameter like this:
    {
        view = view
    }
-}
graphicsApp : JustGraphics a -> Program Never
graphicsApp input =
    App.program
        { init = ( ( 0, initGModel ), initialSizeCmd [] input.view )
        , update = blankUpdate
        , view = blankView input.view
        , subscriptions = \_ -> Window.resizes sizeToMsg
        }


type alias JustGraphics a =
    { view : Collage (Msg a) }


{-| Like graphicsApp, but you can add interactivity to your graphics by using the
"notify" functions. This allows you to learn Elm's architecture in a fun way with
graphics. Note that your view function needs a parameter now:

    view model = collage 500 500
        [ circle 10 |> filled red
        ]

notificationApp takes a parameter like:
    {
        model = model
    ,   view = view
    ,   update = update
    }
-}
notificationsApp : GraphicsApp model msgs -> Program Never
notificationsApp input =
    App.program
        { init = ( ( input.model, initGModel ), initialSizeCmd [] (input.view input.model) )
        , update = hiddenUpdate input.update
        , view = hiddenView input.view
        , subscriptions = \_ -> Window.resizes sizeToMsg
        }


{-| Automatically maps time and keyboard presses to your program. This should
be all you need for making complex interactive games and animations.

gameApp takes two parameters: one is your own type of the form (GetKeyState -> Float -> CustomMsg) and the other is
    {
        model = model
    ,   view = view
    ,   update = update
    }
-}
gameApp : InputHandler msgs -> GraphicsApp model msgs -> Program Never
gameApp tickMsg input =
    App.program
        { init = ( ( input.model, { initGModel | updateTick = tickMsg } ), initialSizeCmd [] (input.view input.model) )
        , update = hiddenGameUpdate input.update
        , view = hiddenView input.view
        , subscriptions = subs []
        }


type alias InputHandler msgs =
    Float
    -> ( Keys -> KeyState, ( Float, Float ), ( Float, Float ) )
    -> msgs


type alias GraphicsApp model msgs =
    { model : model, update : msgs -> model -> model, view : model -> Collage (Msg msgs) }


subs extraSubs model =
    Sub.batch
        ([ Time.every (1000 / 30 * millisecond) (createTimeMessage)
           -- AnimationFrame.times (createTimeMessage timeMsg)
         , Window.resizes sizeToMsg
         ]
            ++ keySubs
            ++ extraSubs
        )


keySubs =
    [ Keyboard.ups (KeyUp), Keyboard.downs (KeyDown) ]


createTimeMessage t =
    let
        time =
            inSeconds t
    in
        TickTime time


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

            NoOp ->
                ( ( model, gModel ), Cmd.none )


blankView view ( model, gModel ) =
    case view of
        Collage ( w, h ) shapes ->
            createCollage w h shapes


hiddenView view ( model, gModel ) =
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



--initialSizeCmd : Cmd Msg


initialSizeCmd otherCmds userView =
    Cmd.batch
        ([ Task.perform (\_ -> NoOp) sizeToMsg Window.size
         , Task.perform (\_ -> NoOp) getCollageSize (Task.succeed userView)
         , Task.perform (\_ -> NoOp) getInitTime Time.now
         ]
            ++ otherCmds
        )


getInitTime t =
    InitTime (inSeconds t)


sizeToMsg : Window.Size -> Msg a
sizeToMsg size =
    WindowResize ( size.width, size.height )


getCollageSize userView =
    case userView of
        Collage ( w, h ) _ ->
            CollageSize ( round w, round h )


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


aHiddenUpdate update msg model =
    ( update msg model, Cmd.none )


aHiddenView view model =
    view model


type alias GModel a =
    { cw : Float
    , ch : Float
    , sw : Float
    , sh : Float
    , initT : Float
    , updateTick : a
    , keys : KeyDict
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
of each player.
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


filterHelper key action =
    case action of
        ( Up, _ ) ->
            False

        _ ->
            True


maintainHelper key action =
    case action of
        ( JustUp, False ) ->
            ( Up, False )

        ( JustUp, True ) ->
            ( JustDown, False )

        ( Up, False ) ->
            ( Up, False )

        --This should never actually happen here though.
        ( Up, True ) ->
            ( Up, False )

        --Same with this.
        ( JustDown, False ) ->
            ( Down, False )

        ( JustDown, True ) ->
            ( JustUp, False )

        ( Down, False ) ->
            ( Down, False )

        ( Down, True ) ->
            ( Down, False )



--Again, this shouldn't happen.

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


{-| Create a regular polygon with a given number of sides and radius. Examples:

    ngon 3 50 - triangle
    ngon 5 50 - pentagon
    ngon 8 50 - octogon
-}
ngon : Float -> Float -> Stencil
ngon n r =
    Polygon <| List.map (ptOnCircle r n) [0..n]


{-| Synonym for "ngon 3". Creates a triangle with a given size.
-}
triangle : Float -> Stencil
triangle r =
    ngon 3 r


{-| Creates a square with a given side length. (Synonym for rect s s)
-}
square : Float -> Stencil
square r =
    Rect r r


{-| Creates a rectangle with a given width and height.
-}
rect : Float -> Float -> Stencil
rect w h =
    Rect w h


{-| Synonym for rect.
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
graphPaper : Float -> Shape notification
graphPaper s =
    group (List.map (createGraphX 1600 s) [-1500 / s..1500 / s] ++ List.map (createGraphY 3000 s) [-800 / s..800 / s])


createGraphX h s x =
    filled (rgb 135 206 250) (rect 1 h) |> move ( x * s, 0 )


createGraphY w s y =
    filled (rgb 135 206 250) (rect w 1) |> move ( 0, y * s )


{-| Creates a wedge with a given radius, and a given fraction of a circle.

    wedge 50 0.5 - semi-circle
    wedge 50 0.25 - quarter-circle
    wedge 50 0.75 - three-quarter circle
-}
wedge : Float -> Float -> Stencil
wedge r frac =
    let
        n =
            frac * 360 / 10 + 5
    in
        Polygon <|
            [ ( 0, 0 ) ]
                ++ (List.map ((wedgeHelper r) << ((*) (frac / n * 180))) [-n..n])
                ++ [ ( 0, 0 ) ]


wedgeHelper r cn =
    let
        angle =
            cn
    in
        ( r * cos (degrees angle), r * sin (degrees angle) )


ptOnCircle r n cn =
    let
        angle =
            360 * cn / n
    in
        ( r * cos (degrees angle), r * sin (degrees angle) )


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
    BezierPath ( a, b ) (List.map curveListHelper list)


curveListHelper (Pull ( a, b ) ( c, d )) =
    ( ( a, b ), ( c, d ) )


{-| Add a hyperlink to any `Shape`.

    circle 10
        |> filled red
        |> addHyperLink "www.redcircle.com"
-}
addHyperlink : String -> Shape notification -> Shape notification
addHyperlink link shape =
    Link link shape


{-| Creates a text stencil. You can change this stencil using the text helper
functions. Note that "filled" or "outlined" must go at the *end* of the infixes
(ie note that all these functions are Stencil -> Stencil).

    text "Hello World"
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


generateCurveHelper ( a, b ) list =
    let
        l1Array =
            Array.fromList ([ ( a, b ) ] ++ List.concat (List.map createTopLevelList list))
    in
        group [ generateCHLines l1Array, generateCHCircles l1Array ]


generateCHLines ar =
    let
        len =
            Array.length ar
    in
        group (List.map (generateCHLine ar) [0..(len - 2)])


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


generateCHCircles ar =
    let
        len =
            Array.length ar
    in
        group (List.map (generateCHCircle ar) [0..(len - 1)])


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


createTopLevelList ( ( a, b ), ( c, d ) ) =
    [ ( a, b ), ( c, d ) ]



--group: (List Shape) ->


type alias Transform =
    ( ( ( Float, Float )
        -- normal transformation of whole group
      , ( Float, Float )
      , ( Float, Float )
      )
    , ( ( Float, Float ), Float, ( Float, Float ) )
      -- scale/rotate/shift inside groups
    )


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


rotT ( trans, ( s, r, t ) ) rad =
    ( trans, ( s, r + rad, t ) )


scaleT ( trans, ( ( ssx, ssy ), r, ( shx, shy ) ) ) ( sx, sy ) =
    ( trans, ( ( ssx * sx, ssy * sy ), r, ( shx, shy ) ) )


type Collage notification
    = Collage ( Float, Float ) (List (Shape notification))


{-| Creates a blank canvas on which you can draw shapes. Takes a width, height and a
list of Shape. Use this in your "view" functions in the three types of Apps above.

    view = collage 500 500
        [ circle 10 |> filled red
        ]
-}
collage : Float -> Float -> List (Shape notification) -> Collage notification
collage w h shapes =
    Collage ( w, h ) shapes


createCollage w h shapes =
    Svg.svg
        [ width "100%", height "99%", style "position:absolute", viewBox ((toString (-w / 2)) ++ " " ++ (toString (-h / 2)) ++ " " ++ (toString w) ++ " " ++ (toString h)) ]
        ([ cPath w h ] ++ [ Svg.g [ clipPath "url(#cPath)" ] (List.map (createSVG id) shapes) ])


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


cPath w h =
    Svg.defs [] [ Svg.clipPath [ Svg.Attributes.id "cPath" ] [ Svg.rect [ width (toString w), height (toString h), x (toString (-w / 2)), y (toString (-h / 2)) ] [] ] ]


f =
    500



--focal length
--puppetShow : Float -> Float -> List (Float,Shape) -> Html.Html msg


puppetShow w h listShapes =
    collage w h (List.map extractShape (List.sortWith flippedComparison listShapes))



--extractShape: (Float,Shape notification) -> Shape notification


extractShape ( z, shape ) =
    let
        s =
            f / (f + z)
    in
        group [ shape ] |> scale s


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


xyToPair xy =
    ( Basics.toFloat (xy.x), Basics.toFloat (-xy.y) )


touchToPair tp =
    case tp of
        TouchPos x y ->
            ( x, -y )


onTapAt msg =
    Html.Events.on "click"
        (Json.map (msg << xyToPair) Mouse.position)


onEnterAt msg =
    Html.Events.on "mouseover"
        (Json.map (msg << xyToPair) Mouse.position)


onLeaveAt msg =
    Html.Events.on "mouseleave"
        (Json.map (msg << xyToPair) Mouse.position)


onMoveAt msg =
    Html.Events.on "mousemove"
        (Json.map (msg << xyToPair) Mouse.position)


onMouseDownAt msg =
    Html.Events.on "mousedown"
        (Json.map (msg << xyToPair) Mouse.position)


onMouseUpAt msg =
    Html.Events.on "mouseup"
        (Json.map (msg << xyToPair) Mouse.position)


onTouchStart msg =
    Html.Events.on "touchstart" (Json.succeed msg)


onTouchStartAt msg =
    Html.Events.on "touchstart"
        (Json.map (msg << touchToPair) touchDecoder)


onTouchEndAt msg =
    Html.Events.on "touchend"
        (Json.map (msg << touchToPair) touchDecoder)


onTouchEnd msg =
    Html.Events.on "touchend" (Json.succeed msg)


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


touchDecoder =
    Json.oneOf
        [ Json.at [ "touches", "0" ] (Json.object2 TouchPos ("pageX" := Json.float) ("pageY" := Json.float))
        , Json.object2 TouchPos ("pageX" := Json.float) ("pageY" := Json.float)
        ]



--createSVG : Transform -> Shape notification -> Svg.Svg notification


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
                            Svg.text' ([ x (toString 0), y (toString 0), Svg.Attributes.style sty, Svg.Attributes.fontSize (toString (si)), Svg.Attributes.textAnchor anchor, Html.Attributes.contenteditable True ] ++ attrs ++ [ Svg.Attributes.transform <| "matrix(" ++ (String.concat <| List.intersperse "," <| List.map toString [ a, b, c, d, tx, -ty ]) ++ ")" ]) [ Svg.text str ]
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


{-| Make a Shape transparent by the fraction given. Multiplies on top of other transparencies:

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
    Solid th


{-| Define a dotted line type with the given width.
-}
dotted : Float -> LineType
dotted th =
    Broken [ ( th, th ) ] th


{-| Define a dashed line type with the given width.  Dashes are short line segments, versus dots which are theoretically points, but may be drawn with very sort line segments.
-}
dashed : Float -> LineType
dashed th =
    Broken [ ( th * 5, th * 2.5 ) ] th


{-| Define a dashed line type with the given width, where the dashes are longer than normal.
-}
longdash : Float -> LineType
longdash th =
    Broken [ ( th * 12, th * 6 ) ] th


{-| Define a line type with the given width, including alternating dots and dashes.
-}
dotdash : Float -> LineType
dotdash th =
    Broken [ ( th, th ), ( th * 5, th ) ] th


{-| A custom line defined by a list of (on,off).

    custom [(10,5)] 5 -- a line that with dashes 10 long and spaces 5 long
    custom [(10,5),(20,5)] -- on for 10, off 5, on 20, off 5
-}
custom : List ( Float, Float ) -> Float -> LineType
custom list th =
    Broken list th


{-| A line of increasing spaces from start to end with a given thickness.

    increasing 1 10 5 -- increases from 1 to 10 with a thickness of 5.
-}
increasing : Float -> Float -> Float -> LineType
increasing s e th =
    Broken (List.map makePair [s..e]) th


makePair n =
    ( n, n )



--Text functions


{-| Apply to a Stencil to render any text in the Stencil in the size given (in points), which depends on the size and type of screen used, but try 12 to start.
-}
size : Float -> Stencil -> Stencil
size size stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face size bo i u s sel f c) str

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


{-| Rotate a Shape by the specified degrees (in radians). Use the "degrees" function to convert
from degrees into radians.
-}
rotate : Float -> Shape notification -> Shape notification
rotate theta shape =
    Rotate theta shape


{-| Move a Shape by a number of units in x and y.
-}
move : ( Float, Float ) -> Shape notification -> Shape notification
move disp shape =
    Move disp shape


{-| Scale a Shape by a given factor.
-}
scale : Float -> Shape notification -> Shape notification
scale s shape =
    ScaleXY s s shape


{-| Scale a Shape in the x-axis by a given factor.
-}
scaleX : Float -> Shape notification -> Shape notification
scaleX s shape =
    ScaleXY s 1 shape


{-| Scale a Shape in the y-axis by a given factor.
-}
scaleY : Float -> Shape notification -> Shape notification
scaleY s shape =
    ScaleXY 1 s shape


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
    RGBA r g b 1


{-| Create a custom colour given its red, green, blue and alpha components.
Alpha is a Float from 0-1 representing the Shape's level of transparency.
-}
rgba : Float -> Float -> Float -> Float -> Color
rgba r g b a =
    RGBA r g b a



{- degrees: Float -> Float
   degrees deg = deg*(pi/180)
-}


pairToString ( x, y ) =
    (toString x) ++ "," ++ (toString y)


createBezierString first list =
    "M " ++ (pairToString first) ++ String.concat (List.map bezierStringHelper list)


bezierStringHelper ( ( a, b ), ( c, d ) ) =
    " Q " ++ pairToString ( a, b ) ++ " " ++ pairToString ( c, d )


mkAlpha (RGBA _ _ _ a) =
    toString a


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
    case (convert h s l) of
        ( r, g, b ) ->
            RGBA r g b 1


{-| Create a custom colour given its hue, saturation, light and alpha components.
Alpha is a Float from 0-1 representing the Shape's level of transparency.
lp
-}
hsla : Float -> Float -> Float -> Float -> Color
hsla h s l a =
    case (convert h s l) of
        ( r, g, b ) ->
            RGBA r g b a



{-
   - Jack You
-}
{-
   - use for converting values (just use this)
-}


convert : Float -> Float -> Float -> ( Float, Float, Float )
convert hue sat lit =
    let
        hue' =
            modFloat hue (6.28318530718)

        rgb' =
            toRGB' hue' sat lit

        chroma =
            findChroma lit sat

        m =
            findM lit chroma
    in
        mapTriple (\x -> x * 255) (mapTriple (\x -> x + m) rgb')


findChroma : Float -> Float -> Float
findChroma lit sat =
    (1 - abs (2 * lit - 1)) * sat


findHue' : Float -> Float
findHue' hue =
    hue / (degrees 60)


findX : Float -> Float -> Float
findX chroma hue =
    chroma * (1 - abs ((modFloat (findHue' hue) 2) - 1))


findM : Float -> Float -> Float
findM lit chroma =
    lit - 0.5 * chroma


toRGB' : Float -> Float -> Float -> ( Float, Float, Float )
toRGB' hue sat lit =
    let
        chroma =
            findChroma lit sat

        hue' =
            findHue' hue

        x =
            findX chroma hue
    in
        if hue' >= 0 && hue' < 1 then
            ( chroma, x, 0 )
        else if hue' >= 1 && hue' < 2 then
            ( x, chroma, 0 )
        else if hue' >= 2 && hue' < 3 then
            ( 0, chroma, x )
        else if hue' >= 3 && hue' < 4 then
            ( 0, x, chroma )
        else if hue' >= 4 && hue' < 5 then
            ( x, 0, chroma )
        else if hue' >= 5 && hue' < 6 then
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
