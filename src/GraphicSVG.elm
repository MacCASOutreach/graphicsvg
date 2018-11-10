module GraphicSVG exposing
    ( Stencil, Shape, Collage(..), GraphicSVG
    , collage, mapCollage
    , App, app
    , EllieApp, ellieApp
    , line, polygon, openPolygon, ngon, triangle, rightTriangle, isosceles, sideAngleSide, square, rect, rectangle, roundedRect, circle, oval, wedge
    , filled, outlined, repaint, addOutline, rgb, rgba, hsl, hsla
    , group
    , html
    , curve, Pull(..), curveHelper
    , LineType, solid, dotted, dashed, longdash, dotdash, custom, text, size, bold, italic, underline, strikethrough, centered, alignLeft, alignRight, selectable, sansserif, serif, fixedwidth, customFont
    , move, rotate, scale, scaleX, scaleY, mirrorX, mirrorY
    , clip, union, subtract, outside, ghost
    , notifyTap, notifyTapAt, notifyEnter, notifyEnterAt, notifyLeave, notifyLeaveAt, notifyMouseMoveAt, notifyMouseDown, notifyMouseDownAt, notifyMouseUp, notifyMouseUpAt, notifyTouchStart, notifyTouchStartAt, notifyTouchEnd, notifyTouchEndAt, notifyTouchMoveAt
    , makeTransparent, addHyperlink, puppetShow
    , graphPaper, graphPaperCustom, map
    , Color, black, blank, blue, brown, charcoal, darkBlue, darkBrown, darkCharcoal, darkGray, darkGreen, darkGrey, darkOrange, darkPurple, darkRed, darkYellow, gray, green, grey, hotPink, lightBlue, lightBrown, lightCharcoal, lightGray, lightGreen, lightGrey, lightOrange, lightPurple, lightRed, lightYellow, orange, pink, purple, red, white, yellow
    )

{-| A library for creating SVG graphics in a way that is compatible with Elm's
old Graphics library. Also includes built-in functions for creating games and
other applications including keyboard presses and mouse movements.


# Basic Types

@docs Stencil, Shape, Collage, GraphicSVG


# Rendering To Screen

@docs collage, mapCollage


# App

@docs App, app


# EllieApp

@docs EllieApp, ellieApp


# Stencils

@docs line, polygon, openPolygon, ngon, triangle, rightTriangle, isosceles, sideAngleSide, square, rect, rectangle, roundedRect, circle, oval, wedge


# Creating Shapes from Stencils

@docs filled, outlined, repaint, addOutline, rgb, rgba, hsl, hsla


# Grouping Shapes

@docs group


# Rendering HTML

@docs html

# Curves

@docs curve, Pull, curveHelper


# Line Styles

@docs LineType, solid, dotted, dashed, longdash, dotdash, custom


# Text

@docs text, size, bold, italic, underline, strikethrough, centered, alignLeft, alignRight, selectable, sansserif, serif, fixedwidth, customFont


# Transformations

@docs move, rotate, scale, scaleX, scaleY, mirrorX, mirrorY


# Group Operations

@docs clip, union, subtract, outside, ghost


# Notifications

@docs notifyTap, notifyTapAt, notifyEnter, notifyEnterAt, notifyLeave, notifyLeaveAt, notifyMouseMoveAt, notifyMouseDown, notifyMouseDownAt, notifyMouseUp, notifyMouseUpAt, notifyTouchStart, notifyTouchStartAt, notifyTouchEnd, notifyTouchEndAt, notifyTouchMoveAt


# Miscellaneous

@docs makeTransparent, addHyperlink, puppetShow


# Helpers

@docs graphPaper, graphPaperCustom, map


# Let there be colours!

@docs Color, black, blank, blue, brown, charcoal, darkBlue, darkBrown, darkCharcoal, darkGray, darkGreen, darkGrey, darkOrange, darkPurple, darkRed, darkYellow, gray, green, grey, hotPink, lightBlue, lightBrown, lightCharcoal, lightGray, lightGreen, lightGrey, lightOrange, lightPurple, lightRed, lightYellow, orange, pink, purple, red, white, yellow

-}

{- Library created by Chris Schankula and Dr. Christopher Anand
   for the McMaster University Computing and Software Outreach Program
   and CompSci 1JC3, with input and testing from the rest of the Outreach
   team.
   Last updated: September 27, 2018
-}

import Array
import Browser exposing (UrlRequest)
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onKeyDown, onKeyPress, onKeyUp, onResize)
import Browser.Navigation exposing (Key)
import Char
import Dict
import Html
import Html.Attributes
import Html.Events
import Http as Http
import Json.Decode as D exposing (..)
import String exposing (..)
import Svg exposing (Attribute)
import Svg.Attributes exposing (..)
import Task
import Time exposing (..)
import Tuple
import Url exposing (Url)


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
    | ForeignObject Float Float (Html.Html userMsg)
    | Move ( Float, Float ) (Shape userMsg)
    | Rotate Float (Shape userMsg)
    | ScaleXY Float Float (Shape userMsg)
    | Group (List (Shape userMsg))
    | AlphaMask (Shape userMsg) (Shape userMsg)
    | Clip (Shape userMsg) (Shape userMsg)
    | Everything
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
    | GraphPaper Float Float Color

type FontAlign = AlignLeft | AlignCentred | AlignRight

{-| To compose multiple pages or components which each have a Msg/view/update, we need to map messages.
(Ask if you don't know what this means.)
-}
map : (a -> b) -> Shape a -> Shape b
map f sh =
    case sh of
        Inked fillClr lt stencil ->
            Inked fillClr lt stencil

        ForeignObject w h htm ->
            ForeignObject w h (Html.map f htm)

        Move v shape ->
            Move v (map f shape)

        Rotate deg shape ->
            Rotate deg (map f shape)

        ScaleXY sx sy shape ->
            ScaleXY sx sy (map f shape)

        Link href shape ->
            Link href (map f shape)

        AlphaMask sh1 sh2 ->
            AlphaMask (map f sh1) (map f sh2)

        Clip sh1 sh2 ->
            Clip (map f sh1) (map f sh2)

        Everything ->
            Everything

        Tap msg shape ->
            Tap (f msg) (map f shape)

        TapAt msg shape ->
            TapAt (f << msg) (map f shape)

        EnterShape msg shape ->
            EnterShape (f msg) (map f shape)

        EnterAt msg shape ->
            EnterAt (f << msg) (map f shape)

        Exit msg shape ->
            Exit (f msg) (map f shape)

        ExitAt msg shape ->
            ExitAt (f << msg) (map f shape)

        MouseDown msg shape ->
            MouseDown (f msg) (map f shape)

        MouseDownAt msg shape ->
            MouseDownAt (f << msg) (map f shape)

        MouseUp msg shape ->
            MouseUp (f msg) (map f shape)

        MouseUpAt msg shape ->
            MouseUpAt (f << msg) (map f shape)

        MoveOverAt msg shape ->
            MoveOverAt (f << msg) (map f shape)

        TouchStart msg shape ->
            TouchStart (f msg) (map f shape)

        TouchEnd msg shape ->
            TouchEnd (f msg) (map f shape)

        TouchStartAt msg shape ->
            TouchStartAt (f << msg) (map f shape)

        TouchEndAt msg shape ->
            TouchEndAt (f << msg) (map f shape)

        TouchMoveAt msg shape ->
            TouchMoveAt (f << msg) (map f shape)

        Group shapes ->
            Group (List.map (map f) shapes)

        GraphPaper s th c ->
            GraphPaper s th c


{-| To apply the function over all the shapes in the given `Collage` using the `map` function.
(Ask if you don't know what this means.)
-}
mapCollage : (a -> b) -> Collage a -> Collage b
mapCollage f (Collage ( w, h ) shapes) =
    Collage ( w, h ) (List.map (map f) shapes)


{-| The `GraphicSVG` type alias represents the drawable surface of the window.

This type is only used to define a type signature for a user defined `view` as follows:

    view : GraphicSVG.GraphicSVG userMsg

for use with `graphicsApp` and as follows:

    view : Model -> GraphicSVG.GraphicSVG MyMsg

for use with `notificationsApp`, `gameApp` and `App`.

These assume that `Model` is the name of the user model type alias and
`MyMsg` is the name of the user message type. Simply substitute the names
actually used for these labels.

THIS IS DEPRECIATED AND ONLY KEPT FOR COMPATIBILITY WITH THE PREVIOUS VERSION;
Use "Collage userMsg" instead of "GraphicSVG userMsg", as they are identical except in name.

-}
type alias GraphicSVG userMsg =
    Collage userMsg


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
THIS IS NOT EXPOSED BY ANY TYPE AND IS NOW NOT EXPOSED BY THE LIBARY.
-}
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
        -- font alignment
        FontAlign


{-| The `Font` type describes the font of a text `Stencil`.
THIS IS NOT EXPOSED BY ANY TYPE AND IS NOW NOT EXPOSED BY THE LIBARY.
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


{-| Advanced Function Warning! app takes one parameter of its own type of the form:

    {
      init = \flags url key -> (model, cmd)
    , view = \model -> { title = "Your Title goes here",
                       , body = view model
                       }
    , update = \userMsg userModel -> ( userModel, Cmd userMsg )
    , subscriptions = \userModel -> Sub userMsg
    , onUrlRequest = \urlRequest -> userMsg
    , onUrlChange = \url -> userMsg
    }

This matches the Elm architecture and is analogous to `Browser.application`.

-}
app :
    { init : flags -> Url -> Key -> ( userModel, Cmd userMsg )
    , update : userMsg -> userModel -> ( userModel, Cmd userMsg )
    , view : userModel -> { title : String, body : Collage userMsg }
    , subscriptions : userModel -> Sub userMsg
    , onUrlRequest : UrlRequest -> userMsg
    , onUrlChange : Url -> userMsg
    }
    -> App flags userModel userMsg
app input =
    Browser.application
        { init =
            \flags url key ->
                let
                    userInit =
                        Tuple.first <| input.init flags url key

                    userView =
                        (input.view userInit).body

                    (Collage ( initW, initH ) _) =
                        userView

                    userInitCmd =
                        Tuple.second <| input.init flags url key
                in
                ( ( userInit, { initHiddenModel | cw = initW, ch = initH } ), initialCmd <| Cmd.map Graphics userInitCmd )
        , update = hiddenAppUpdate input.view input.update
        , view = hiddenAppView input.view
        , subscriptions = subs <| input.subscriptions
        , onUrlRequest = Graphics << input.onUrlRequest
        , onUrlChange = Graphics << input.onUrlChange
        }


{-| This type alias is only used as a target for a user `main` type signature to make
the type signature more clear and concise when `main` calls `app`:

    main : App Flags Model Msg
    main =
        app Tick
            { init = init
            , update = update
            , view = view
            , subscriptions = subscriptions
            , onUrlRequest = onUrlRequest
            , onUrlChange = onUrlChange
            }

where `Tick` is a message handler called once per browser window update,
`Flags` is the type alias (or type) of the flags input from JavaScript
`Model` is the type alias of the user persistent model, and
`Msg` is the name of the user message type; if other names are used,
they can just be substituted for these names.

Note that "type alias" could also be an ordinary "type" in all case,
just that the syntax for producing a new type is not as clean as using a
type alias for a record.

-}
type alias App flags userModel userMsg =
    Program flags ( userModel, HiddenModel ) (Msg userMsg)


subs : (userModel -> Sub userMsg) -> ( userModel, gModel ) -> Sub (Msg userMsg)
subs userSubs ( userModel, _ ) =
    Sub.batch
        ([ onResize (\w h -> WindowResize ( w, h ))
         ]
            ++ [ Sub.map Graphics (userSubs userModel) ]
        )


hiddenAppUpdate :
    (userModel -> { title : String, body : Collage userMsg })
    -> (userMsg -> userModel -> ( userModel, Cmd userMsg ))
    -> Msg userMsg
    -> ( userModel, HiddenModel )
    -> ( ( userModel, HiddenModel ), Cmd (Msg userMsg) )
hiddenAppUpdate userView userUpdate msg ( userModel, gModel ) =
    let
        mapUserCmd cmd =
            Cmd.map Graphics cmd

        (Collage ( cw, ch ) _) =
            (userView userModel).body
    in
    case msg of
        Graphics message ->
            let
                ( newModel, userCmds ) =
                    userUpdate message userModel
            in
            ( ( newModel, { gModel | cw = cw, ch = ch } )
            , mapUserCmd userCmds
            )

        WindowResize ( width, height ) ->
            ( ( userModel
              , { gModel
                    | sw = Basics.toFloat width
                    , sh = Basics.toFloat height
                }
              )
            , Cmd.none
            )

        ReturnPosition message ( x, y ) ->
            let
                ( newModel, userCmds ) =
                    userUpdate
                        (message (convertCoords ( x, y ) gModel))
                        userModel
            in
            ( ( newModel, gModel ), mapUserCmd userCmds )


hiddenAppView :
    (userModel -> { title : String, body : Collage userMsg })
    -> ( userModel, HiddenModel )
    -> { title : String, body : List (Html.Html (Msg userMsg)) }
hiddenAppView userView ( userModel, _ ) =
    let
        userViewEval =
            userView userModel

        title =
            userViewEval.title

        (Collage ( w, h ) shapes) =
            userViewEval.body
    in
    { title = title, body = [ createCollage w h shapes ] }

{-| This type alias is only used as a target for a user `main` type signature to make
the type signature more clear and concise when `main` calls `ellieApp`:

    main : EllieApp Flags Model Msg
    main =
        ellieApp Tick
            { init = init
            , update = update
            , view = view
            , subscriptions = subscriptions
            }

where `Tick` is a message handler called once per browser window update,
`Flags` is the type alias (or type) of the flags input from JavaScript
`Model` is the type alias of the user persistent model, and
`Msg` is the name of the user message type; if other names are used,
they can just be substituted for these names.

Note that "type alias" could also be an ordinary "type" in all case,
just that the syntax for producing a new type is not as clean as using a
type alias for a record.

-}
type alias EllieApp flags userModel userMsg =
    Program flags ( userModel, HiddenModel ) (Msg userMsg)

{-| Advanced Function Warning! ellieApp takes one parameter of its own type of the form:

    {
      init = \flags url key -> (model, cmd)
    , view = \model -> { title = "Your Title goes here",
                       , body = view model
                       }
    , update = \userMsg userModel -> ( userModel, Cmd userMsg )
    , subscriptions = \userModel -> Sub userMsg
    }

This matches the Elm architecture and is analogous to `Browser.document`.
It is called ellieApp because this version is compatible with the online
Elm IDE Ellie.

-}
ellieApp :
    { init : flags -> ( userModel, Cmd userMsg )
    , update : userMsg -> userModel -> ( userModel, Cmd userMsg )
    , view : userModel -> { title : String, body : Collage userMsg }
    , subscriptions : userModel -> Sub userMsg
    }
    -> EllieApp flags userModel userMsg
ellieApp input =
    Browser.document
        { init =
            \flags ->
                let
                    userInit =
                        Tuple.first <| input.init flags

                    userView =
                        (input.view userInit).body

                    (Collage ( initW, initH ) _) =
                        userView

                    userInitCmd =
                        Tuple.second <| input.init flags
                in
                ( ( userInit, { initHiddenModel | cw = initW, ch = initH } ), initialCmd <| Cmd.map Graphics userInitCmd )
        , update = hiddenAppUpdate input.view input.update
        , view = hiddenAppView input.view
        , subscriptions = subs <| input.subscriptions
        }


convertCoords : ( Float, Float ) -> HiddenModel -> ( Float, Float )
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

        aspectout =
            if not (sh == 0) then
                sw / sh

            else
                4 / 3

        aspectin =
            if not (ch == 0) then
                cw / ch

            else
                4 / 3

        scaledInX =
            aspectout < aspectin

        scaledInY =
            aspectout > aspectin

        cscale =
            if scaledInX then
                sw / cw

            else if scaledInY then
                sh / ch

            else
                1
    in
    ( (x - sw / 2) / cscale
    , (y + sh / 2) / cscale
    )


initialCmd :
    Cmd (Msg userMsg)
    -> Cmd (Msg userMsg)
initialCmd userCmd =
    Cmd.batch
        [ Task.perform
            (\vp ->
                WindowResize
                    ( round vp.viewport.width, round vp.viewport.height )
            )
            getViewport
        , userCmd
        ]


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


{-| The `HiddenModel` type alias encapsulates the GraphicSVG internal model
which is not exposed to user code.
-}
type alias HiddenModel =
    { cw : Float
    , ch : Float
    , sw : Float
    , sh : Float
    }


initHiddenModel : HiddenModel
initHiddenModel =
    { cw = 0
    , ch = 0
    , sw = 0
    , sh = 0
    }


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
    polygon [ ( 0, 0 ), ( base, 0 ), ( 0, height ) ]


{-| Creates an isosceles triangle with a given base and height.
-}
isosceles : Float -> Float -> Stencil
isosceles base height =
    polygon [ ( -base / 2, 0 ), ( base / 2, 0 ), ( 0, height ) ]


{-| Creates a triangle given two side lengths and the angle between them.

For example, `sideAngleSide 30 (degrees 45) 50` creates a triangle with side lengths
30 and 50 with an angle of 45 degrees between them.

-}
sideAngleSide : Float -> Float -> Float -> Stencil
sideAngleSide sideOne angle sideTwo =
    polygon [ sideTwoPoint angle sideOne, ( 0, 0 ), ( sideTwo, 0 ) ]


sideTwoPoint : Float -> Float -> ( Float, Float )
sideTwoPoint angle sideOne =
    ( cos angle * sideOne, sin angle * sideOne )


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
    GraphPaper s 1 (RGBA 135 206 250 1)


{-| Creates graph paper with squares of a given size, with a user-defined thickness and colour.
-}
graphPaperCustom : Float -> Float -> Color -> Shape userMsg
graphPaperCustom s th c =
    if s >= 2 then
        GraphPaper s th c
    else
        group []

createGraph : (Float, Float) -> Float -> Float -> Color -> Shape userMsg
createGraph (w,h) s th c =
    let
        sxi =
            ceiling (w / (s * 2))

        syi =
            ceiling (h / (s * 2))

        xlisti =
            List.range -sxi sxi

        ylisti =
            List.range -syi syi
    in
    group
        (List.map (createGraphX h s th c << Basics.toFloat) xlisti
            ++ List.map (createGraphY w s th c << Basics.toFloat) ylisti
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
                ++ List.map
                    (wedgeHelper r
                        << (*) (frac / n * 180)
                        << Basics.toFloat
                    )
                    (List.range -ni ni)
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
        |> addHyperlink "http://outreach.mcmaster.ca"

-}
addHyperlink : String -> Shape userMsg -> Shape userMsg
addHyperlink link shape =
    Link link shape


{-| Creates a text `Stencil`. You can change this `Stencil` using the text helper
functions. Note that `|> filled ...` or `|> outlined ...` must go at the _end_ of the text helper functions
(ie note that all these functions have type `Stencil -> Stencil`). For example,

    text "Hello World"
        |> fixedwidth
        |> bold
        |> size 14
        |> filled black

-}
text : String -> Stencil
text str =
    Text (Face 12 False False False False False Serif AlignLeft) str


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

        Move s sh ->
            Move s (curveHelper sh)

        Rotate r sh ->
            Rotate r (curveHelper sh)

        ScaleXY sx sy sh ->
            ScaleXY sx sy (curveHelper sh)

        Group list ->
            Group (List.map curveHelper list)

        a ->
            a


generateCurveHelper :
    ( Float, Float )
    -> List ( ( Float, Float ), ( Float, Float ) )
    -> Shape userMsg
generateCurveHelper ( a, b ) list =
    let
        l1Array =
            Array.fromList
                ([ ( a, b ) ]
                    ++ List.concat (List.map createTopLevelList list)
                )
    in
    group [ generateCHLines l1Array, generateCHCircles l1Array ]


generateCHLines : Array.Array ( Float, Float ) -> Shape userMsg
generateCHLines ar =
    let
        len =
            Array.length ar
    in
    group (List.map (generateCHLine ar) (List.range 0 (len - 2)))


generateCHLine : Array.Array ( Float, Float ) -> Int -> Shape userMsg
generateCHLine ar int =
    let
        p1 =
            case Array.get int ar of
                Just p ->
                    p

                Nothing ->
                    ( 0, 0 )

        p2 =
            case Array.get (int + 1) ar of
                Just p ->
                    p

                Nothing ->
                    ( 0, 0 )
    in
    outlined (dashed 0.5) black (line p1 p2)


generateCHCircles : Array.Array ( Float, Float ) -> Shape userMsg
generateCHCircles ar =
    let
        len =
            Array.length ar
    in
    group (List.map (generateCHCircle ar) (List.range 0 (len - 1)))


generateCHCircle : Array.Array ( Float, Float ) -> Int -> Shape userMsg
generateCHCircle ar int =
    let
        p1 =
            case Array.get int ar of
                Just p ->
                    p

                Nothing ->
                    ( 0, 0 )

        ptStr =
            pairToString p1
    in
    group
        [ filled red (circle 2)
        , text ("(" ++ ptStr ++ ")")
            |> filled black
            |> move ( 5, 5 )
        ]
        |> move p1


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
    -> ( ( ( Float, Float ), ( Float, Float ), ( Float, Float ) ), ( ( number, number ), number, ( number, number ) ) )
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


ident : ( ( ( number, number ), ( number, number ), ( number, number ) ), ( ( number, number ), number, ( number, number ) ) )
ident =
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
and the \`List' of Shapes to be drawn on the drawing surface.
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


createCollage : Float -> Float -> List (Shape userMsg) -> Html.Html (Msg userMsg)
createCollage w h shapes =
    Svg.svg
        [ width "100%"
        , height "100%"
        , style "position:absolute;top:0px;left:0px;"
        , viewBox
            (String.fromFloat (-w / 2)
                ++ " "
                ++ String.fromFloat (-h / 2)
                ++ " "
                ++ String.fromFloat w
                ++ " "
                ++ String.fromFloat h
            )
        ]
        (cPath w h
            :: [ Svg.g
                    [ clipPath "url(#cPath)" ]
                    (List.indexedMap
                        (\n -> createSVG (String.fromInt n) w h ident)
                        shapes
                    )
               ]
        )


cPath : Float -> Float -> Svg.Svg (Msg userMsg)
cPath w h =
    Svg.defs []
        [ Svg.clipPath
            [ Svg.Attributes.id "cPath" ]
            [ Svg.rect
                [ width (String.fromFloat w)
                , height (String.fromFloat h)
                , x (String.fromFloat (-w / 2))
                , y (String.fromFloat (-h / 2))
                ]
                []
            ]
        ]


{-| Take a `Collage` width and height as well as a list of tuples of depth and `Shape`.
to produce a list of `Shape`'s suitable for display as a group in a `Collage` with
`Shape`'s with lesser depth larger and in front of `Shape`'s with greater depth.
-}
puppetShow :
    Float
    -> Float
    -> List ( Float, Shape userMsg )
    -> List (Shape userMsg)
puppetShow w h listShapes =
    List.map
        (extractShape (Basics.max w h))
        (List.sortWith flippedComparison listShapes)


extractShape : Float -> ( Float, Shape userMsg ) -> Shape userMsg
extractShape fl ( z, shape ) =
    let
        s =
            fl / (fl + z)
    in
    shape |> scale s


flippedComparison : ( comparable, a ) -> ( comparable, b ) -> Order
flippedComparison ( f, _ ) ( s, _ ) = compare s f


--Notification functions


{-| Receive a message (`userMsg`) when a `Shape` is clicked or tapped.
-}
notifyTap : userMsg -> Shape userMsg -> Shape userMsg
notifyTap msg shape =
    Tap msg shape


{-| Receive a message (`userMsg`) with the x and y position of the mouse or finger when the `Shape` is clicked or tapped.
-}
notifyTapAt : (( Float, Float ) -> userMsg) -> Shape userMsg -> Shape userMsg
notifyTapAt msg shape =
    TapAt msg shape


{-| Receive a message (`userMsg`) when the mouse enters a `Shape`.
-}
notifyEnter : userMsg -> Shape userMsg -> Shape userMsg
notifyEnter msg shape =
    EnterShape msg shape


{-| Receive a message (`userMsg`) with the x and y position of the mouse when the mouse enters a `Shape`.
-}
notifyEnterAt : (( Float, Float ) -> userMsg) -> Shape userMsg -> Shape userMsg
notifyEnterAt msg shape =
    EnterAt msg shape


{-| Receive a message (`userMsg`) when the mouse leaves a `Shape`.
-}
notifyLeave : userMsg -> Shape userMsg -> Shape userMsg
notifyLeave msg shape =
    Exit msg shape


{-| Receive a message (`userMsg`) with the x and y position of the mouse when the mouse leaves a `Shape`.
-}
notifyLeaveAt : (( Float, Float ) -> userMsg) -> Shape userMsg -> Shape userMsg
notifyLeaveAt msg shape =
    ExitAt msg shape


{-| Receive a message (`userMsg`) with the x and y position of the mouse when the mouse is moved across a `Shape`.
-}
notifyMouseMoveAt : (( Float, Float ) -> userMsg) -> Shape userMsg -> Shape userMsg
notifyMouseMoveAt msg shape =
    MoveOverAt msg shape


{-| Receive a message (`userMsg`) when the mouse button is pressed while the cursor is over a `Shape`.
-}
notifyMouseDown : userMsg -> Shape userMsg -> Shape userMsg
notifyMouseDown msg shape =
    MouseDown msg shape


{-| Receive a message (`userMsg`) with the x and y position of the mouse when the mouse button is pressed while the cursor is over a `Shape`.
-}
notifyMouseDownAt : (( Float, Float ) -> userMsg) -> Shape userMsg -> Shape userMsg
notifyMouseDownAt msg shape =
    MouseDownAt msg shape


{-| Receive a message (`userMsg`) when the mouse button is released while the cursor is over a `Shape`.
-}
notifyMouseUp : userMsg -> Shape userMsg -> Shape userMsg
notifyMouseUp msg shape =
    MouseUp msg shape


{-| Receive a message (`userMsg`) with the x and y position of the mouse when the mouse button is released while the cursor is over a `Shape`.
-}
notifyMouseUpAt : (( Float, Float ) -> userMsg) -> Shape userMsg -> Shape userMsg
notifyMouseUpAt msg shape =
    MouseUpAt msg shape


{-| Receive a message (`userMsg`) when the user begins touching a `Shape`.
-}
notifyTouchStart : userMsg -> Shape userMsg -> Shape userMsg
notifyTouchStart msg shape =
    TouchStart msg shape


{-| Receive a message (`userMsg`) with the x and y position of the user's finger when the user begins touching a `Shape`.
-}
notifyTouchStartAt : (( Float, Float ) -> userMsg) -> Shape userMsg -> Shape userMsg
notifyTouchStartAt msg shape =
    TouchStartAt msg shape


{-| Receive a message (`userMsg`) when the user lifts their finger off a `Shape`.
-}
notifyTouchEnd : userMsg -> Shape userMsg -> Shape userMsg
notifyTouchEnd msg shape =
    TouchEnd msg shape


{-| Receive a message (`userMsg`) with the x and y position of the user's finger when the user lifts their finger off a `Shape`.
-}
notifyTouchEndAt : (( Float, Float ) -> userMsg) -> Shape userMsg -> Shape userMsg
notifyTouchEndAt msg shape =
    TouchEndAt msg shape


{-| Receive a message (`userMsg`) with the x and y position of the user's finger when the user moves their finger over a `Shape`.
-}
notifyTouchMoveAt : (( Float, Float ) -> userMsg) -> Shape userMsg -> Shape userMsg
notifyTouchMoveAt msg shape =
    TouchMoveAt msg shape


touchToPair : TouchPos -> ( Float, Float )
touchToPair tp =
    case tp of
        TouchPos x y ->
            ( x, -y )


mousePosDecoder =
    D.map2 (\x y -> ( x, -y )) (D.field "pageX" D.float) (D.field "pageY" D.float)


onTapAt : (( Float, Float ) -> Msg userMsg) -> Html.Attribute (Msg userMsg)
onTapAt msg =
    Html.Events.on "click"
        (D.map msg mousePosDecoder)


onEnterAt : (( Float, Float ) -> Msg userMsg) -> Html.Attribute (Msg userMsg)
onEnterAt msg =
    Html.Events.on "mouseover"
        (D.map msg mousePosDecoder)


onLeaveAt : (( Float, Float ) -> Msg userMsg) -> Html.Attribute (Msg userMsg)
onLeaveAt msg =
    Html.Events.on "mouseleave"
        (D.map msg mousePosDecoder)


onMoveAt : (( Float, Float ) -> Msg userMsg) -> Html.Attribute (Msg userMsg)
onMoveAt msg =
    Html.Events.on "mousemove"
        (D.map msg mousePosDecoder)


onMouseDownAt : (( Float, Float ) -> Msg userMsg) -> Html.Attribute (Msg userMsg)
onMouseDownAt msg =
    Html.Events.on "mousedown"
        (D.map msg mousePosDecoder)


onMouseUpAt : (( Float, Float ) -> Msg userMsg) -> Html.Attribute (Msg userMsg)
onMouseUpAt msg =
    Html.Events.on "mouseup"
        (D.map msg mousePosDecoder)


onTouchStart : Msg userMsg -> Html.Attribute (Msg userMsg)
onTouchStart msg =
    Html.Events.on "touchstart" (D.succeed msg)


onTouchStartAt : (( Float, Float ) -> Msg userMsg) -> Html.Attribute (Msg userMsg)
onTouchStartAt msg =
    Html.Events.on "touchstart"
        (D.map (msg << touchToPair) touchDecoder)


onTouchEndAt : (( Float, Float ) -> Msg userMsg) -> Html.Attribute (Msg userMsg)
onTouchEndAt msg =
    Html.Events.on "touchend"
        (D.map (msg << touchToPair) touchDecoder)


onTouchEnd : Msg userMsg -> Html.Attribute (Msg userMsg)
onTouchEnd msg =
    Html.Events.on "touchend" (D.succeed msg)


onTouchMove : (( Float, Float ) -> Msg userMsg) -> Html.Attribute (Msg userMsg)
onTouchMove msg =
    Html.Events.preventDefaultOn "touchmove"
        (D.map (\a -> ( (msg << touchToPair) a, True )) touchDecoder)


type TouchPos
    = TouchPos Float Float


touchDecoder : Decoder TouchPos
touchDecoder =
    D.oneOf
        [ D.at
            [ "touches", "0" ]
            (D.map2
                TouchPos
                (D.field "pageX" D.float)
                (D.field "pageY" D.float)
            )
        , D.map2
            TouchPos
            (D.field "pageX" D.float)
            (D.field "pageY" D.float)
        ]


createSVG : String -> Float -> Float -> Transform -> Shape a -> Svg.Svg (Msg a)
createSVG id w h trans shape =
    case shape of
        Inked fillClr lt stencil ->
            let
                ( ( ( a, b ), ( c, d ), ( tx, ty ) ), _ ) =
                    coalesce trans

                attrs =
                    transAttrs ++ clrAttrs ++ strokeAttrs

                transAttrs =
                    [ Svg.Attributes.transform <|
                        "matrix("
                            ++ (String.concat <|
                                    List.intersperse "," <|
                                        List.map String.fromFloat [ a, -b, c, -d, tx, -ty ]
                               )
                            ++ ")"
                    ]

                clrAttrs =
                    [ fill (mkRGB fillClr), fillOpacity (mkAlpha fillClr) ]

                strokeAttrs =
                    case lt of
                        Nothing ->
                            []

                        Just ( Solid th, strokeClr ) ->
                            [ strokeWidth (String.fromFloat th)
                            , stroke (mkRGB strokeClr)
                            , strokeOpacity (mkAlpha strokeClr)
                            ]

                        Just ( Broken dashes th, strokeClr ) ->
                            [ strokeWidth (String.fromFloat th)
                            , stroke (mkRGB strokeClr)
                            , strokeOpacity (mkAlpha strokeClr)
                            ]
                                ++ [ strokeDasharray <|
                                        String.concat
                                            (List.intersperse "," <|
                                                List.map pairToString dashes
                                            )
                                   ]
            in
            case stencil of
                Circle r ->
                    Svg.circle
                        ([ cx "0"
                         , cy "0"
                         , Svg.Attributes.r (String.fromFloat r)
                         ]
                            ++ attrs
                        )
                        []

                Rect rw rh ->
                    Svg.rect
                        ([ x <| String.fromFloat <| -rw / 2
                         , y <| String.fromFloat <| -rh / 2
                         , width <| String.fromFloat rw
                         , height <| String.fromFloat rh
                         ]
                            ++ attrs
                        )
                        []

                RoundRect rw rh r ->
                    Svg.rect
                        ([ x <| String.fromFloat <| -rw / 2
                         , y <| String.fromFloat <| -rh / 2
                         , rx <| String.fromFloat r
                         , ry <| String.fromFloat r
                         , width <| String.fromFloat rw
                         , height <| String.fromFloat rh
                         ]
                            ++ attrs
                        )
                        []

                Oval ow oh ->
                    Svg.ellipse
                        ([ cx "0"
                         , cy "0"
                         , rx <| String.fromFloat <| 0.5 * ow
                         , ry <| String.fromFloat <| 0.5 * oh
                         ]
                            ++ attrs
                        )
                        []

                -- BezierPath (List )
                Polygon vertices ->
                    Svg.polygon
                        ([ points <|
                            String.concat <|
                                List.intersperse " " <|
                                    List.map pairToString vertices
                         ]
                            ++ attrs
                        )
                        []

                Path vertices ->
                    Svg.polyline
                        ([ points <|
                            String.concat <|
                                List.intersperse " " <|
                                    List.map pairToString vertices
                         ]
                            ++ attrs
                        )
                        []

                BezierPath start pts ->
                    Svg.path
                        ([ Svg.Attributes.d <| createBezierString start pts ]
                            ++ attrs
                        )
                        []

                Text (Face si bo i u s sel f align) str ->
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

                        txtDec =
                            if u && s then
                                "text-decoration: underline line-through;"

                            else if u then
                                "text-decoration: underline;"

                            else if s then
                                "text-decoration: line-through;"

                            else
                                ""

                        select =
                            if not sel then
                                "-webkit-touch-callout: none;\n-webkit-user-select: none;\n-khtml-user-select: none;\n-moz-user-select: none;\n-ms-user-select: none;\nuser-select: none;cursor: default;"

                            else
                                ""

                        anchor =
                            case align of
                                AlignCentred -> "middle"
                                AlignLeft -> "start"
                                AlignRight -> "end"

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
                                ++ txtDec
                                ++ "font-family: "
                                ++ font
                                ++ select
                    in
                    Svg.text_
                        ([ x "0"
                         , y "0"
                         , Svg.Attributes.style sty
                         , Svg.Attributes.fontSize (String.fromFloat si)
                         , Svg.Attributes.textAnchor anchor
                         , Html.Attributes.contenteditable True
                         ]
                            ++ attrs
                            ++ [ Svg.Attributes.transform <|
                                    "matrix("
                                        ++ (String.concat <|
                                                List.intersperse "," <|
                                                    List.map
                                                        String.fromFloat
                                                        [ a, -b, -c, d, tx, -ty ]
                                           )
                                        ++ ")"
                               ]
                            ++ [ Svg.Attributes.xmlSpace "preserve" ]
                        )
                        [ Svg.text str ]

        ForeignObject fw fh htm ->
            let
                ( ( ( a, b ), ( c, d ), ( tx, ty ) ), _ ) =
                    coalesce trans
            in
                Svg.foreignObject [ width <| String.fromFloat fw, height <| String.fromFloat fh, Svg.Attributes.transform <| "matrix(" ++ (String.concat <| List.intersperse "," <| List.map String.fromFloat [ a, -b, -c, d, tx, -ty ]) ++ ")" ] [ Html.map Graphics htm ]

        Move v sh ->
            createSVG id w h (moveT trans v) sh

        Everything ->
            createSVG id w h ident (rect w h |> filled white)

        Rotate deg sh ->
            createSVG id w h (rotT trans deg) sh

        ScaleXY sx sy sh ->
            createSVG id w h (scaleT trans ( sx, sy )) sh

        Link href sh ->
            Svg.a
                [ xlinkHref href, target "_blank" ]
                [ createSVG id w h (coalesce trans) sh ]

        AlphaMask region sh ->
            Svg.g []
                [ Svg.defs []
                    [ Svg.mask
                        [ Svg.Attributes.id ("m" ++ id) ]
                        [ createSVG (id ++ "m") w h (coalesce trans) region ]
                    ]
                , Svg.g
                    [ Svg.Attributes.mask ("url(#m" ++ id ++ ")") ]
                    [ createSVG (id ++ "mm") w h (coalesce trans) sh ]
                ]

        Clip region sh ->
            Svg.g []
                [ Svg.defs []
                    [ Svg.clipPath
                        [ Svg.Attributes.id ("c" ++ id) ]
                        [ createSVG (id ++ "m") w h (coalesce trans) region ]
                    ]
                , Svg.g
                    [ clipPath ("url(#c" ++ id ++ ")") ]
                    [ createSVG (id ++ "cc") w h (coalesce trans) sh ]
                ]

        Tap msg sh ->
            Svg.g
                [ Html.Events.onClick (Graphics msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        TapAt msg sh ->
            Svg.g
                [ onTapAt (ReturnPosition msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        EnterShape msg sh ->
            Svg.g
                [ Html.Events.onMouseEnter (Graphics msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        EnterAt msg sh ->
            Svg.g
                [ onEnterAt (ReturnPosition msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        Exit msg sh ->
            Svg.g
                [ Html.Events.onMouseLeave (Graphics msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        ExitAt msg sh ->
            Svg.g
                [ onLeaveAt (ReturnPosition msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        MouseDown msg sh ->
            Svg.g
                [ Html.Events.onMouseDown (Graphics msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        MouseDownAt msg sh ->
            Svg.g
                [ onMouseDownAt (ReturnPosition msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        MouseUp msg sh ->
            Svg.g
                [ Html.Events.onMouseUp (Graphics msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        MouseUpAt msg sh ->
            Svg.g
                [ onMouseUpAt (ReturnPosition msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        MoveOverAt msg sh ->
            Svg.g
                [ onMoveAt (ReturnPosition msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        TouchStart msg sh ->
            Svg.g
                [ onTouchStart (Graphics msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        TouchEnd msg sh ->
            Svg.g
                [ onTouchEnd (Graphics msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        TouchStartAt msg sh ->
            Svg.g
                [ onTouchStartAt (ReturnPosition msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        TouchEndAt msg sh ->
            Svg.g
                [ onTouchStartAt (ReturnPosition msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        TouchMoveAt msg sh ->
            Svg.g
                [ onTouchMove (ReturnPosition msg) ]
                [ createSVG id w h (coalesce trans) sh ]

        Group shapes ->
            Svg.g [] <|
                List.indexedMap
                    (\n ->
                        createSVG
                            (id ++ "g" ++ String.fromInt n)
                            w
                            h
                        <|
                            coalesce trans
                    )
                    shapes

        GraphPaper s th c ->
            Svg.g []
                [ createSVG id w h (coalesce trans) <| createGraph (w,h) s th c ]

{-| Display HTML inside an SVG foreignObject.
-}
html : Float -> Float -> Html.Html userMsg -> Shape userMsg
html w h htm =
    ForeignObject w h htm



--Filling / outlining functions


{-| Fill a `Stencil` with a `Color`, creating a `Shape`.

    circle 10
        |> filled red

-}
filled : Color -> Stencil -> Shape userMsg
filled color stencil =
    Inked color Nothing stencil


{-| Make a `Shape` into a ghost. Mostly to be used inside of the clip operations.
-}
ghost : Stencil -> Shape userMsg
ghost stencil =
    Inked white Nothing stencil


{-| Repaint an already-`filled` `Shape`. This is helpful for repainting every `Shape` inside a `group` as well.

    group
        [ circle 10
            |> filled orange
        , rect 10 40
            |> filled blue
        ]
        |> repaint green

-}
repaint : Color -> Shape userMsg -> Shape userMsg
repaint color shape =
    case shape of
        Inked clr outline sh ->
            Inked color outline sh

        Move s sh ->
            Move s (repaint color sh)

        Rotate r sh ->
            Rotate r (repaint color sh)

        ScaleXY sx sy sh ->
            ScaleXY sx sy (repaint color sh)

        Group shapes ->
            Group (List.map (repaint color) shapes)

        AlphaMask sh1 sh2 ->
            AlphaMask sh1 (repaint color sh2)

        Clip shape1 shape2 ->
            Clip (repaint color shape1) shape2

        a ->
            a


{-| Outline a Stencil with a `LineType` and `Color`, creating a `Shape`.

    circle 10
        |> outlined (solid 5) red

-}
outlined : LineType -> Color -> Stencil -> Shape userMsg
outlined style outlineClr stencil =
    let
        lineStyle =
            ( style, outlineClr )
    in
    Inked (rgba 0 0 0 0) (Just lineStyle) stencil


{-| Add an outline to an already-filled `Shape`.
This only works with Shapes based on primary `Stencil`'s,
not on `Group` or `Shape`'s produced by `clip`, `union`, `subtract`, etc.

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
        Inked clr outline sh ->
            Inked clr (Just lineStyle) sh

        Move s sh ->
            Move s (addOutline style outlineClr sh)

        Rotate r sh ->
            Rotate r (addOutline style outlineClr sh)

        ScaleXY sx sy sh ->
            ScaleXY sx sy (addOutline style outlineClr sh)

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
        Inked (RGBA r g b a) (Just ( lineType, RGBA sr sg sb sa )) sh ->
            Inked (RGBA r g b (a * alpha)) (Just ( lineType, RGBA sr sg sb (sa * alpha) )) sh

        Inked (RGBA r g b a) Nothing sh ->
            Inked (RGBA r g b (a * alpha)) Nothing sh

        ForeignObject w h htm ->
            ForeignObject w h htm

        Move s sh ->
            Move s (makeTransparent alpha sh)

        Rotate r sh ->
            Rotate r (makeTransparent alpha sh)

        ScaleXY sx sy sh ->
            ScaleXY sx sy (makeTransparent alpha sh)

        Group list ->
            Group (List.map (makeTransparent alpha) list)

        Link s sh ->
            Link s (makeTransparent alpha sh)

        AlphaMask reg sh ->
            AlphaMask reg (makeTransparent alpha sh)

        Clip reg sh ->
            Clip (makeTransparent alpha reg) sh

        Everything ->
            Everything

        Tap userMsg sh ->
            Tap userMsg (makeTransparent alpha sh)

        TapAt userMsg sh ->
            TapAt userMsg (makeTransparent alpha sh)

        EnterShape userMsg sh ->
            EnterShape userMsg (makeTransparent alpha sh)

        EnterAt userMsg sh ->
            EnterAt userMsg (makeTransparent alpha sh)

        Exit userMsg sh ->
            Exit userMsg (makeTransparent alpha sh)

        ExitAt userMsg sh ->
            ExitAt userMsg (makeTransparent alpha sh)

        MouseDown userMsg sh ->
            MouseDown userMsg (makeTransparent alpha sh)

        MouseDownAt userMsg sh ->
            MouseDownAt userMsg (makeTransparent alpha sh)

        MouseUp userMsg sh ->
            MouseUp userMsg (makeTransparent alpha sh)

        MouseUpAt userMsg sh ->
            MouseUpAt userMsg (makeTransparent alpha sh)

        MoveOverAt userMsg sh ->
            MoveOverAt userMsg (makeTransparent alpha sh)

        TouchStart userMsg sh ->
            TouchStart userMsg (makeTransparent alpha sh)

        TouchEnd userMsg sh ->
            TouchEnd userMsg (makeTransparent alpha sh)

        TouchStartAt userMsg sh ->
            TouchStartAt userMsg (makeTransparent alpha sh)

        TouchEndAt userMsg sh ->
            TouchEndAt userMsg (makeTransparent alpha sh)

        TouchMoveAt userMsg sh ->
            TouchMoveAt userMsg (makeTransparent alpha sh)

        GraphPaper s th (RGBA r g b a) ->
            GraphPaper s th (RGBA r g b (a*alpha))



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

    custom [ ( 10, 5 ) ] 5 -- a line with dashes 10 long and spaces 5 long

    custom [ ( 10, 5 ), ( 20, 5 ) ] -- on for 10, off 5, on 20, off 5

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
size sze stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face sze bo i u s sel f c) str

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


{-| Apply to a `text` `Stencil` to right-align the text.
-}
alignRight : Stencil -> Stencil
alignRight stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s sel f AlignRight) str

        a ->
            a

{-| Apply to a `text` `Stencil` to centre the text.
-}
centered : Stencil -> Stencil
centered stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s sel f AlignCentred) str

        a ->
            a

{-| Apply to a `text` `Stencil` to left-align the text.
-}
alignLeft : Stencil -> Stencil
alignLeft stencil =
    case stencil of
        Text (Face si bo i u s sel f c) str ->
            Text (Face si bo i u s sel f AlignLeft) str

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

_Use this sparingly as support for each font will vary across browsers and devices._

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
    RGBA (ssc r) (ssc g) (ssc b) 1


{-| Define a colour given its red, green, blue and alpha components.
Alpha is a decimal number (`Float`) from 0 to 1 representing the level of transparency.
-}
rgba : Float -> Float -> Float -> Float -> Color
rgba r g b a =
    RGBA (ssc r) (ssc g) (ssc b) (ssa a)


ssc : number -> number
ssc n =
    clamp 0 255 n


ssa : Float -> Float
ssa n =
    clamp 0 1 n


pairToString : ( Float, Float ) -> String
pairToString ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y


createBezierString : ( Float, Float ) -> List ( ( Float, Float ), ( Float, Float ) ) -> String
createBezierString first list =
    "M " ++ pairToString first ++ String.concat (List.map bezierStringHelper list)


bezierStringHelper : ( ( Float, Float ), ( Float, Float ) ) -> String
bezierStringHelper ( ( a, b ), ( c, d ) ) =
    " Q " ++ pairToString ( a, b ) ++ " " ++ pairToString ( c, d )


mkAlpha : Color -> String
mkAlpha (RGBA _ _ _ a) =
    String.fromFloat a


mkRGB : Color -> String
mkRGB (RGBA r g b _) =
    "#" ++ (toHex <| round r) ++ (toHex <| round g) ++ (toHex <| round b)


toHex : Int -> String
toHex dec =
    let
        first =
            dec // 16

        second =
            modBy 16 dec
    in
    toHexHelper first ++ toHexHelper second


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
    case convert h s l of
        ( r, g, b ) ->
            RGBA r g b 1


{-| Define a colour given its hue, saturation, light and alpha components.
Alpha is a decimal number (`Float`) from 0 to 1 representing the level of transparency.
-}
hsla : Float -> Float -> Float -> Float -> Color
hsla h s l a =
    case convert h s l of
        ( r, g, b ) ->
            RGBA r g b (ssa a)



-- Clip path functionality


{-| Clipping to a shape

    Cut out the `Shape` on the left using the `Shape` on the right.

-}
clip : Shape userMsg -> Shape userMsg -> Shape userMsg
clip shape1 shape2 =
    Clip shape1 shape2


{-| Shape union

    Combine two `Shape`s together into one to use with clip.

-}
union : Shape userMsg -> Shape userMsg -> Shape userMsg
union shape1 shape2 =
    Group [ shape1, shape2 ]


{-| Shape subtraction

    Subtract the `Shape` on the right from the `Shape` on the left.

-}
subtract : Shape userMsg -> Shape userMsg -> Shape userMsg
subtract shape1 shape2 =
    AlphaMask (Group [ Everything, shape1 |> repaint black ]) shape2


{-| The whole region outside the given `Shape`.
-}
outside : Shape userMsg -> Shape userMsg
outside shape =
    AlphaMask (Group [ Everything, shape |> repaint black ]) shape



{-
   Contributed by Jack You.
-}


convert : Float -> Float -> Float -> ( Float, Float, Float )
convert hue sat lit =
    let
        hue_ =
            modFloat hue (2 * pi)

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
    hue / degrees 60


findX : Float -> Float -> Float
findX chroma hue =
    chroma * (1 - abs (modFloat (findHue_ hue) 2 - 1))


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
