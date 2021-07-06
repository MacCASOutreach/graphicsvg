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
    , LineType, noline, solid, dotted, dashed, longdash, dotdash, custom
    , text, size, bold, italic, underline, strikethrough, centered, alignLeft, alignRight, selectable, sansserif, serif, fixedwidth, customFont
    , move, rotate, scale, scaleX, scaleY, mirrorX, mirrorY, skewX, skewY
    , clip, union, subtract, outside, ghost
    , notifyTap, notifyTapAt, notifyEnter, notifyEnterAt, notifyLeave, notifyLeaveAt, notifyMouseMoveAt, notifyMouseDown, notifyMouseDownAt, notifyMouseUp, notifyMouseUpAt, notifyTouchStart, notifyTouchStartAt, notifyTouchEnd, notifyTouchEndAt, notifyTouchMoveAt
    , makeTransparent, addHyperlink, puppetShow
    , graphPaper, graphPaperCustom, map
    , gradient, radialGradient, stop, transparentStop, rotateGradient
    , Color, black, blank, blue, brown, charcoal, darkBlue, darkBrown, darkCharcoal, darkGray, darkGreen, darkGrey, darkOrange, darkPurple, darkRed, darkYellow, gray, green, grey, hotPink, lightBlue, lightBrown, lightCharcoal, lightGray, lightGreen, lightGrey, lightOrange, lightPurple, lightRed, lightYellow, orange, pink, purple, red, white, yellow
    , Transform, ident, moveT, rotateT, scaleT, skewT, rotateAboutT, transform
    , Msg(..), createSVG
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

This feature is still very much experimental. Cross-platform support is
not guaranteed and weird things can happen.

@docs html


# Curves

@docs curve, Pull, curveHelper


# Line Styles

@docs LineType, noline, solid, dotted, dashed, longdash, dotdash, custom


# Text

@docs text, size, bold, italic, underline, strikethrough, centered, alignLeft, alignRight, selectable, sansserif, serif, fixedwidth, customFont


# Transformations

@docs move, rotate, scale, scaleX, scaleY, mirrorX, mirrorY, skewX, skewY


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


# Let there be gradients!

@docs stop, transparentStop, gradient, radialGradient, rotateGradient

# Advanced Transformations

_Advanced section warning!_ These functions provide a way to interface on a lower
level to the transformations normally handled in the background by GraphicSVG.
Most users should be happy to use the regular functions applied directly to shapes,
which are provided in the section above this one.

@docs Transform, ident, moveT, rotateT, scaleT, skewT, rotateAboutT, transform

# More Advanced Things

Don't worry about these unless you *_really_* know what you're doing!

@docs Msg, createSVG

-}

{- Library created by Chris Schankula and Dr. Christopher Anand
   for the McMaster University Computing and Software Outreach Program
   and CompSci 1JC3, with input and testing from the rest of the Outreach
   team.
   Last updated: January 25th, 2019
-}

import Array
import Browser exposing (UrlRequest)
import Browser.Dom exposing (Viewport, getViewportOf)
import Browser.Events exposing (onKeyDown, onKeyPress, onKeyUp, onResize)
import Browser.Navigation exposing (Key)
import Char
import Dict
import Html
import Html.Attributes
import Html.Events
import Json.Decode as D exposing (..)
import String exposing (..)
import Svg exposing (Attribute)
import Svg.Attributes exposing (..)
import Task
import Time exposing (..)
import Tuple
import Url exposing (Url)
import Color


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
    = Inked (Maybe Color) (Maybe ( LineType, Color )) Stencil
    | ForeignObject Float Float (Html.Html userMsg)
    | Move ( Float, Float ) (Shape userMsg)
    | Rotate Float (Shape userMsg)
    | Scale Float Float (Shape userMsg)
    | Skew Float Float (Shape userMsg)
    | Transformed Transform (Shape userMsg)
    | Group (List (Shape userMsg))
    | GroupOutline (Shape userMsg)
    | AlphaMask (Shape userMsg) (Shape userMsg)
    | Clip (Shape userMsg) (Shape userMsg)
    | Everything
    | Notathing
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


type FontAlign
    = AlignLeft
    | AlignCentred
    | AlignRight


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

        Scale sx sy shape ->
            Scale sx sy (map f shape)

        Skew skx sky shape ->
            Skew skx sky (map f shape)

        Transformed tm shape ->
            Transformed tm (map f shape)

        Link href shape ->
            Link href (map f shape)

        AlphaMask sh1 sh2 ->
            AlphaMask (map f sh1) (map f sh2)

        Clip sh1 sh2 ->
            Clip (map f sh1) (map f sh2)

        Everything ->
            Everything

        Notathing ->
            Notathing

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

        GroupOutline cmbndshp ->
            GroupOutline (map f cmbndshp)

        GraphPaper s th c ->
            GraphPaper s th c


{-| To apply the function over all the shapes in the given `Collage` using the `map` function.
(Ask if you don't know what this means.)
-}
mapCollage : (a -> b) -> Collage a -> Collage b
mapCollage f (Collage w h shapes) =
    Collage w h (List.map (map f) shapes)


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
    = Solid Color.Color
    | Gradient Gradient

type Gradient =
      RadialGradient (List Stop)
    | LinearGradient Float {- rotation -} (List Stop)

{-| Create a radial gradient from a list of colour stops.
-}
radialGradient : List Stop -> Color
radialGradient stops =
    Gradient <| RadialGradient stops

{-| Create a linear gradient from a list of colour stops.
-}
gradient : List Stop -> Color
gradient stops =
    Gradient <| LinearGradient 0 stops

{-| Rotate a linear gradient by a certain angle in radians.
-}
rotateGradient : Float -> Color -> Color
rotateGradient r grad =
    case grad of
        Gradient (LinearGradient rot stops) -> Gradient (LinearGradient (rot + r) stops)
        radialGrad -> radialGrad

type Stop =
    Stop Float {- stop position -} Float {- transparency -} Color.Color {- colour -}

{-| A colour stop in a gradient. This takes a colour and a position.
-}
stop : Color -> Float -> Stop
stop col pos =
    case col of
        Solid colour -> Stop pos 1 colour
        _ -> Stop pos 1 (Color.rgba 0 0 0 0)

{-| A colour stop with transparency.
-}
transparentStop : Color -> Float -> Float -> Stop
transparentStop col pos alpha =
    case col of
        Solid colour -> Stop pos alpha colour
        _ -> Stop pos 1 (Color.rgba 0 0 0 alpha)

createGradientSVG : String -> (Float, Float) -> Gradient -> Svg.Svg userMsg
createGradientSVG id (wid, hei) grad =
    let
        isRadial = case grad of
            RadialGradient _ -> True
            _ -> False

        squareSize = if wid > hei then 2*wid else 2*hei

        w = case grad of
            RadialGradient stops ->
                case List.head <| List.reverse stops of
                    Just (Stop pos _ _) -> pos
                    Nothing -> 0
            LinearGradient _ stops ->
                case List.head <| List.reverse stops of
                    Just (Stop pos _ _) -> pos
                    Nothing -> 0


        createStop : Stop -> Svg.Svg userMsg
        createStop (Stop pos trans colour) =
            let
                start = if isRadial then 0 else (1 - w/squareSize)/2 * 100
                percent = if isRadial then pos / w * 100 else start + pos/squareSize * 100
                percentTxt = String.fromFloat percent ++ "%"
                colourTxt = "stop-color:" ++ mkRGB colour ++ ";"
                opacityTxt = "stop-opacity:" ++ String.fromFloat trans ++ ";"
            in
            Svg.stop [Svg.Attributes.offset percentTxt, Svg.Attributes.style (colourTxt ++ opacityTxt)] []
        defs =     Svg.defs []
                       [
                           case grad of
                               LinearGradient _ stops ->
                                   Svg.linearGradient
                                        [ Svg.Attributes.id (id ++ "gradient")
                                        , Svg.Attributes.gradientTransform <| "rotate(" ++ String.fromFloat rotation ++ "rad)"
                                        --, Svg.Attributes.x1 "0%"
                                        --, Svg.Attributes.y1 "0%"
                                        --, Svg.Attributes.x2 "100%"
                                        --, Svg.Attributes.y2 "0%"

                                        , Svg.Attributes.gradientTransform ("rotate("++ String.fromFloat rotation ++ "rad)")
                                        ]
                                       (List.map createStop stops)
                               RadialGradient stops ->
                                   Svg.radialGradient
                                        [ Svg.Attributes.id (id ++ "gradient")
                                        , Svg.Attributes.cx "0"--(String.fromFloat (squareSize/2))
                                        , Svg.Attributes.cy "0"--(String.fromFloat (squareSize/2))
                                        , Svg.Attributes.r (String.fromFloat w)
                                        , Svg.Attributes.gradientUnits "userSpaceOnUse"
                                        ]
                                       (List.map createStop stops)
                       ]
        rotation =
            case grad of
                LinearGradient rot stops ->
                    rot * 180 / pi
                _ -> 0
    in
        Svg.g [mask ("url(#" ++ id ++ "mask)")]
            [ defs
            , Svg.rect
                   ([ x <| String.fromFloat <| -squareSize / 2
                   , y <| String.fromFloat <| -squareSize / 2
                   , width <| String.fromFloat squareSize
                   , height <| String.fromFloat squareSize
                   , fill ("url(#" ++ id ++ "gradient)")
                   , Svg.Attributes.id (id ++ "grad")
                   , Svg.Attributes.transform <| "rotate(" ++ String.fromFloat rotation ++")"
                   ]
                   )
                   []
            ]

{-| The `LineType` type is used to define the appearance of an outline for a `Stencil`.
`LineType` also defines the appearence of `line` and `curve`.
-}
type LineType
    = NoLine
    | Unbroken Float
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


{-| _Advanced Function Warning!_ `app` takes one parameter of its own type of the form:

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

                    (Collage initW initH _) =
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
        ([ onResize (\_ _ -> WindowResize Nothing)
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

        (Collage cw ch _) =
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

        WindowResize mWH ->
            case mWH of
                Just ( w, h ) ->
                    ( ( userModel
                      , { gModel
                            | sw = Basics.toFloat w
                            , sh = Basics.toFloat h
                        }
                      )
                    , Cmd.none
                    )
                Nothing ->
                    ( (userModel, gModel)
                    , getViewportSize
                    )

        ReturnPosition message ( x, y ) ->
            let
                ( newModel, userCmds ) =
                    userUpdate
                        (message (convertCoords ( x, y ) gModel))
                        userModel
            in
            ( ( newModel, gModel ), mapUserCmd userCmds )

        NoOp -> (( userModel, gModel ), Cmd.none)


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

        (Collage w h shapes) =
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


{-| _Advanced Function Warning!_ `ellieApp` takes one parameter of its own type of the form:

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

                    (Collage initW initH _) =
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
        [ getViewportSize
        , userCmd
        ]

getViewportSize : Cmd (Msg userMsg)
getViewportSize = Task.attempt
            (\rvp -> case rvp of
                        Ok vp ->                    
                            WindowResize
                                <| Just ( round vp.viewport.width, round vp.viewport.height )
                        Err _ -> NoOp
            )
            (getViewportOf "render")

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
    | WindowResize (Maybe ( Int, Int ))
    | ReturnPosition (( Float, Float ) -> userMsg) ( Float, Float )
    | NoOp


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
    Polygon <|
        List.map
            (ptOnCircle r (Basics.toFloat n) << Basics.toFloat)
            (List.range 0 n)


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
    GraphPaper s 1 (Solid <| Color.fromRgba { red = 135, green = 206, blue = 250, alpha = 1})


{-| Creates graph paper with squares of a given size, with a user-defined thickness and colour.
-}
graphPaperCustom : Float -> Float -> Color -> Shape userMsg
graphPaperCustom s th c =
    GraphPaper s th c


createGraph : ( Float, Float ) -> Float -> Float -> Color -> Shape userMsg
createGraph ( w, h ) s th c =
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

        dlta =
            frac / Basics.toFloat ni
    in
    Polygon <|
        if frac > 0 then
            [ ( 0, 0 ), wedgeHelper r -frac ]
                ++ List.map
                    (wedgeHelper r
                        << (*) dlta
                        << Basics.toFloat
                    )
                    (List.range -ni ni)
                ++ [ wedgeHelper r frac, ( 0, 0 ) ]

        else
            []


wedgeHelper : Float -> Float -> ( Float, Float )
wedgeHelper r cn =
    let
        angle =
            turns (0.5 * cn)
    in
    ( r * cos angle, r * sin angle )


ptOnCircle : Float -> Float -> Float -> ( Float, Float )
ptOnCircle r n cn =
    let
        angle =
            turns (cn / n)
    in
    ( r * cos angle, r * sin angle )


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
        Inked _ _ (BezierPath ( a, b ) list) ->
            group [ shape, generateCurveHelper ( a, b ) list ]

        Move s sh ->
            Move s (curveHelper sh)

        Rotate r sh ->
            Rotate r (curveHelper sh)

        Scale sx sy sh ->
            Scale sx sy (curveHelper sh)

        Skew skx sky sh ->
            Skew skx sky (curveHelper sh)

        Transformed tm sh ->
            Transformed tm (curveHelper sh)

        Group list ->
            Group (List.map curveHelper list)

        GroupOutline cmbndshp ->
            GroupOutline (curveHelper cmbndshp)

        Link s sh ->
            Link s (curveHelper sh)

        AlphaMask reg sh ->
            AlphaMask reg (curveHelper sh)

        Clip reg sh ->
            Clip reg (curveHelper sh)

        Tap userMsg sh ->
            Tap userMsg (curveHelper sh)

        TapAt userMsg sh ->
            TapAt userMsg (curveHelper sh)

        EnterShape userMsg sh ->
            EnterShape userMsg (curveHelper sh)

        EnterAt userMsg sh ->
            EnterAt userMsg (curveHelper sh)

        Exit userMsg sh ->
            Exit userMsg (curveHelper sh)

        ExitAt userMsg sh ->
            ExitAt userMsg (curveHelper sh)

        MouseDown userMsg sh ->
            MouseDown userMsg (curveHelper sh)

        MouseDownAt userMsg sh ->
            MouseDownAt userMsg (curveHelper sh)

        MouseUp userMsg sh ->
            MouseUp userMsg (curveHelper sh)

        MouseUpAt userMsg sh ->
            MouseUpAt userMsg (curveHelper sh)

        MoveOverAt userMsg sh ->
            MoveOverAt userMsg (curveHelper sh)

        TouchStart userMsg sh ->
            TouchStart userMsg (curveHelper sh)

        TouchEnd userMsg sh ->
            TouchEnd userMsg (curveHelper sh)

        TouchStartAt userMsg sh ->
            TouchStartAt userMsg (curveHelper sh)

        TouchEndAt userMsg sh ->
            TouchEndAt userMsg (curveHelper sh)

        TouchMoveAt userMsg sh ->
            TouchMoveAt userMsg (curveHelper sh)

        -- no changes for the rest...
        Inked clr ln sh ->
            Inked clr ln sh

        ForeignObject w h htm ->
            ForeignObject w h htm

        Everything ->
            Everything

        Notathing ->
            Notathing

        GraphPaper s th clr ->
            GraphPaper s th clr


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


matrixMult : Transform -> Transform -> Transform
matrixMult ( ( a, c, e ), ( b, d, f ) ) ( ( a1, c1, e1 ), ( b1, d1, f1 ) ) =
    ( ( a * a1 + c * b1, a * c1 + c * d1, e + a * e1 + c * f1 )
    , ( b * a1 + d * b1, b * c1 + d * d1, f + b * e1 + d * f1 )
    )


{-| The identity or "starting" matrix. Applying this matrix to a shape is the equivalent
of doing no transformations at all. The matrix itself looks like

```
ident = ( ( 1 , 0 , 0 ) , ( 0 , 1 , 0 ) )
```

or,

```
 1 0 0
 0 1 0
```

-}
ident : Transform
ident =
    ( ( 1, 0, 0 )
    , ( 0, 1, 0 )
    )


{-| A matrix representing an SVG transformation matrix of the form:

```
( ( a , c , e ) , ( b , d , f ) )
```

or

```
 a c e
 b d f
```

The a, c, b and d control transformations such as skew, rotate and scale;
e and f control translation.

These matrices are best built up by starting with the identity matrix and
applying any number of `*T` functions (see below); for example,

```
myTransform =
    ident
        |> scaleT 2 2
        |> rotateT (degrees 30)
        |> moveT (0, 50)
```

-}
type alias Transform =
    ( ( Float, Float, Float ), ( Float, Float, Float ) )


{-| Apply a move (translation) transformation to a transformation matrix.
This is designed to be used as part of a pipe:

```
ident
    |> moveT (15,-30.5)
```

-}
moveT : ( Float, Float ) -> Transform -> Transform
moveT ( u, v ) ( ( a, c, tx ), ( b, d, ty ) ) =
    ( ( a, c, tx + a * u + c * v )
    , ( b, d, ty + b * u + d * v )
    )


{-| Apply a rotation transformation to a transformation matrix.
This is designed to be used as part of a pipe:

```
ident
    |> moveT (15,-30.5)
    |> rotateT (degrees -30)
```

-}
rotateT : Float -> Transform -> Transform
rotateT rad ( ( a, c, tx ), ( b, d, ty ) ) =
    let
        sinX =
            sin rad

        cosX =
            cos rad
    in
    ( ( a * cosX + c * sinX, c * cosX - a * sinX, tx )
    , ( b * cosX + d * sinX, d * cosX - b * sinX, ty )
    )


{-| Apply a scale transformation to a transformation matrix. The first argument
scales in x and the second argument scales in y. This is designed
to be used as part of a pipe:

```
ident
    |> moveT (15,-30.5)
    |> rotateT (degrees -30)
    |> scaleT 4 0.4
```

-}
scaleT : Float -> Float -> Transform -> Transform
scaleT sx sy ( ( a, c, tx ), ( b, d, ty ) ) =
    ( ( a * sx, c * sy, tx )
    , ( b * sx, d * sy, ty )
    )


{-| Apply a skew transformation to a matrix. The first argument
skews in x and the second argument skews in y. This is designed
to be used as part of a pipe:

```
ident
    |> moveT (15,-30.5)
    |> rotateT (degrees -30)
    |> scaleT 4 0.4
    |> skewT 0.5 1.3
```

-}
skewT : Float -> Float -> Transform -> Transform
skewT skx sky ( ( a, c, tx ), ( b, d, ty ) ) =
    let
        tanX =
            tan -skx

        tanY =
            tan -sky
    in
    ( ( a + c * tanY, c + a * tanX, tx )
    , ( b + d * tanY, d + b * tanX, ty )
    )


{-| Apply a rotation about a given point to a `Transform` matrix. For example,
the following transform will rotate a `Shape` 30 degrees about the point (0,50):

```
rotateAbout050 =
    ident
        |> rotateAboutT (0, 50) (degrees 30)
```

-}
rotateAboutT : ( Float, Float ) -> Float -> Transform -> Transform
rotateAboutT ( u, v ) rad ( ( a, c, tx ), ( b, d, ty ) ) =
    let
        sinX =
            sin rad

        cosX =
            cos rad
    in
    ( ( a * cosX + c * sinX, c * cosX - a * sinX, tx + a * u + c * v - v * (c * cosX - a * sinX) - u * (a * cosX + c * sinX) )
    , ( b * cosX + d * sinX, d * cosX - b * sinX, ty + b * u + d * v - v * (d * cosX - b * sinX) - u * (b * cosX + d * sinX) )
    )


{-| Manually transform a shape using a `Transform` matrix. Matrix multiplication will
be used to apply the given matrix to any transformations of the current
shape. This is designed to be used in the usual way in a pipe:

```
circle 10
    |> filled red
    |> transform moveLeft50

moveLeft50 =
    ident
        |> moveT (50,0)
```

NOTE: Transformations generated using pipes this way are applied backwards compared
to the "regular" `Shape userMsg` transformation functions. For example, `rect0` and
`rect1` below are equivalent:

```
myTransform =
    ident
        |> scaleT 2 2
        |> rotateT (degrees 30)
        |> moveT (0, 50)

rect0 =
    rect 20 10
        |> filled red
        |> transform myTransform

rect1 =
    rect 20 10
        |> filled red
        |> move (0, 50)
        |> rotate (degrees 30)
        |> scale 2
```

On the other hand, single transformations produce a result consistent with the
`Shape userMsg` transformations. `rect2` is also equivalent to the two above:

```
moveRight50 =
    ident
        |> moveT (50,0)

scale2 =
    ident
        |> scaleT 2 2

rotate30 =
    ident
        |> rotateT (degrees 30)

rect2 =
    rect 20 10
        |> filled red
        |> transform moveRight50
        |> transform scale2
        |> transform rotate30
```

However, chaining together transformations in this way is discouraged because
it is less efficient than the regular `Shape userMsg` transformations in
the "Transformations" section.

-}
transform : Transform -> Shape userMsg -> Shape userMsg
transform tm sh =
    Transformed tm sh


{-| The Collage type represents the drawable surface of the window which contains
a (x, y) pair of horizontal and vertical dimensions (arbitrary units,
not necessarily in pixels) to which the drawing surface will be scaled,
and the \`List' of Shapes to be drawn on the drawing surface.
-}
type Collage userMsg
    = Collage Float Float (List (Shape userMsg))


{-| Creates a blank canvas on which you can draw. Takes a width, height and a
list of `Shape`s. Use this in your `view` functions in the three types of Apps above:

    collage 500 500
        [
            circle 10 |> filled red
        ]

-}
collage : Float -> Float -> List (Shape userMsg) -> Collage userMsg
collage w h shapes =
    Collage w h shapes


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
        , id "render"
        ]
        (cPath w h
            :: [ Svg.g
                    [ clipPath "url(#cPath)" ]
                    (List.indexedMap
                        (\n -> createSVG (String.fromInt n) w h ident Graphics ReturnPosition)
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
flippedComparison ( f, _ ) ( s, _ ) =
    compare s f



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
    D.map2 (\x y -> ( x, -y )) (D.field "offsetX" D.float) (D.field "offsetY" D.float)


onTapAt : (( Float, Float ) -> userMsg) -> Html.Attribute userMsg
onTapAt msg =
    Html.Events.on "click"
        (D.map msg mousePosDecoder)


onEnterAt : (( Float, Float ) -> userMsg) -> Html.Attribute userMsg
onEnterAt msg =
    Html.Events.on "mouseover"
        (D.map msg mousePosDecoder)


onLeaveAt : (( Float, Float ) -> userMsg) -> Html.Attribute userMsg
onLeaveAt msg =
    Html.Events.on "mouseleave"
        (D.map msg mousePosDecoder)


onMoveAt : (( Float, Float ) -> userMsg) -> Html.Attribute userMsg
onMoveAt msg =
    Html.Events.on "mousemove"
        (D.map msg mousePosDecoder)


onMouseDownAt : (( Float, Float ) -> userMsg) -> Html.Attribute userMsg
onMouseDownAt msg =
    Html.Events.on "mousedown"
        (D.map msg mousePosDecoder)


onMouseUpAt : (( Float, Float ) -> userMsg) -> Html.Attribute userMsg
onMouseUpAt msg =
    Html.Events.on "mouseup"
        (D.map msg mousePosDecoder)


onTouchStart : userMsg -> Html.Attribute userMsg
onTouchStart msg =
    Html.Events.on "touchstart" (D.succeed msg)


onTouchStartAt : (( Float, Float ) -> userMsg) -> Html.Attribute userMsg
onTouchStartAt msg =
    Html.Events.on "touchstart"
        (D.map (msg << touchToPair) touchDecoder)


onTouchEndAt : (( Float, Float ) -> userMsg) -> Html.Attribute userMsg
onTouchEndAt msg =
    Html.Events.on "touchend"
        (D.map (msg << touchToPair) touchDecoder)


onTouchEnd : userMsg -> Html.Attribute userMsg
onTouchEnd msg =
    Html.Events.on "touchend" (D.succeed msg)


onTouchMove : (( Float, Float ) -> userMsg) -> Html.Attribute userMsg
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

{-| Create Svg from a Shape. This is considered an advanced function and
is usually not used. Instead, use collage as part of a regular GraphicSVG
app or create a widget with GraphicSVG.Widget. 
-}
createSVG : String -> Float -> Float -> Transform -> (a -> b) -> (((Float,Float) -> a) -> (Float,Float) -> b) -> Shape a -> Svg.Svg b
createSVG id w h trans msgWrapper positionWrapper shape =
    case shape of
        Inked fillClr lt stencil ->
            let
                ( ( a, c, tx ), ( b, d, ty ) ) =
                    trans

                attrs =
                    case fillClr of
                        Just (Gradient _) ->
                            clrAttrs ++ strokeAttrs
                        _ -> transAttrs ++ clrAttrs ++ strokeAttrs

                transAttrs =
                    [ Svg.Attributes.transform <|
                        "matrix("
                            ++ (String.concat <|
                                    List.intersperse "," <|
                                        List.map
                                            String.fromFloat
                                            [ a, -b, c, -d, tx, -ty ]
                               )
                            ++ ")"
                    ]

                nonexistBody =
                    case fillClr of
                        Nothing -> True
                        _ -> False
                
                clrAttrs =
                    case fillClr of
                        Nothing -> [ fill "none"]
                        Just (Solid bodyClr) ->
                                [ fill (mkRGB bodyClr)
                                , fillOpacity (mkAlpha bodyClr)
                                ]
                        Just (Gradient _) ->
                                [
                                    Svg.Attributes.id id
                                ,   fill (mkRGB <| Color.rgb 255 255 255)
                                ]

                strokeAttrs =
                    case lt of
                        Nothing ->
                            []

                        Just ( Unbroken th, Solid strokeClr ) ->
                            let nonStroke =
                                    let opcty = getAlpha strokeClr
                                    in th <= 0 || opcty <= 0
                            in
                            if nonStroke then [] else
                                [ strokeWidth (String.fromFloat th)
                                , stroke (mkRGB strokeClr)
                                , strokeOpacity (mkAlpha strokeClr)
                                ]

                        Just ( Broken dashes th, Solid strokeClr ) ->
                            let nonStroke =
                                    let opcty = getAlpha strokeClr
                                    in th <= 0 || opcty <= 0 ||
                                        List.all (\( on, _ ) -> on == 0) dashes
                            in
                            if nonStroke then [] else
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

                        Just ( _, _ ) ->
                            []
                basicShape = case stencil of
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
                                                             AlignCentred ->
                                                                 "middle"

                                                             AlignLeft ->
                                                                 "start"

                                                             AlignRight ->
                                                                 "end"

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
                                                         ++ clrAttrs ++ strokeAttrs
                                                     )
                                                     [ Svg.text str ]

                gradientDefs =
                        case fillClr of
                            Just (Gradient gr) ->
                                Just (createGradientSVG id (w, h) gr)
                            _ -> Nothing

            in
            if nonexistBody && List.isEmpty strokeAttrs then Svg.g [] []
            else case gradientDefs of
                Just gDefs ->
                    Svg.g transAttrs
                        [
                            Svg.mask [Svg.Attributes.id (id ++ "mask")]
                                [
                                    basicShape
                                ]
                        ,  gDefs
                        ]
                Nothing ->
                    basicShape

        ForeignObject fw fh htm ->
            let
                ( ( a, c, tx ), ( b, d, ty ) ) =
                    trans
            in
            Svg.foreignObject
                [ width <| String.fromFloat fw
                , height <| String.fromFloat fh
                , Svg.Attributes.transform
                    <| "matrix(" ++ (String.concat <| List.intersperse ","
                                        <| List.map
                                            String.fromFloat
                                            [ a, -b, -c, d, tx, -ty ]) ++ ")"
                ] [ Html.map msgWrapper htm ]

        Move v sh ->
            createSVG id w h (moveT v trans) msgWrapper positionWrapper  sh

        Everything ->
            createSVG id w h ident msgWrapper positionWrapper  (rect w h |> filled white)

        Notathing ->
            createSVG id w h ident msgWrapper positionWrapper  (rect w h |> filled black)

        Rotate deg sh ->
            createSVG id w h (rotateT deg trans) msgWrapper positionWrapper  sh

        Scale sx sy sh ->
            createSVG id w h (scaleT sx sy trans) msgWrapper positionWrapper  sh

        Skew sx sy sh ->
            createSVG id w h (skewT sx sy trans) msgWrapper positionWrapper  sh

        Transformed tm sh ->
            createSVG id w h (matrixMult trans tm) msgWrapper positionWrapper  sh

        Link href sh ->
            Svg.a
                [ xlinkHref href, target "_blank" ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        AlphaMask region sh ->
            Svg.g []
                [ Svg.defs []
                    [ Svg.mask
                        [ Svg.Attributes.id ("m" ++ id) ]
                        [ createSVG
                            (id ++ "m")
                            w
                            h
                            trans
                            msgWrapper 
                            positionWrapper
                            (Group [ Everything, region |> repaint black ])
                        ]
                    ]
                , Svg.g
                    [ Svg.Attributes.mask ("url(#m" ++ id ++ ")") ]
                    [ createSVG (id ++ "mm") w h trans msgWrapper positionWrapper sh ]
                ]

        Clip region sh ->
            Svg.g []
                [ Svg.defs []
                    [ Svg.mask
                        [ Svg.Attributes.id ("c" ++ id) ]
                        [ createSVG
                            (id ++ "c")
                            w
                            h
                            trans
                            msgWrapper
                            positionWrapper
                            (Group [ Notathing, region |> repaint white ])
                        ]
                    ]
                , Svg.g
                    [ Svg.Attributes.mask ("url(#c" ++ id ++ ")") ]
                    [ createSVG (id ++ "cc") w h trans msgWrapper positionWrapper sh ]
                ]

        Tap msg sh ->
            Svg.g
                [ Html.Events.onClick (msgWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        TapAt msg sh ->
            Svg.g
                [ onTapAt (positionWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        EnterShape msg sh ->
            Svg.g
                [ Html.Events.onMouseEnter (msgWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        EnterAt msg sh ->
            Svg.g
                [ onEnterAt (positionWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        Exit msg sh ->
            Svg.g
                [ Html.Events.onMouseLeave (msgWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        ExitAt msg sh ->
            Svg.g
                [ onLeaveAt (positionWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        MouseDown msg sh ->
            Svg.g
                [ Html.Events.onMouseDown (msgWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        MouseDownAt msg sh ->
            Svg.g
                [ onMouseDownAt (positionWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        MouseUp msg sh ->
            Svg.g
                [ Html.Events.onMouseUp (msgWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        MouseUpAt msg sh ->
            Svg.g
                [ onMouseUpAt (positionWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        MoveOverAt msg sh ->
            Svg.g
                [ onMoveAt (positionWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        TouchStart msg sh ->
            Svg.g
                [ onTouchStart (msgWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        TouchEnd msg sh ->
            Svg.g
                [ onTouchEnd (msgWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        TouchStartAt msg sh ->
            Svg.g
                [ onTouchStartAt (positionWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        TouchEndAt msg sh ->
            Svg.g
                [ onTouchStartAt (positionWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        TouchMoveAt msg sh ->
            Svg.g
                [ onTouchMove (positionWrapper msg) ]
                [ createSVG id w h trans msgWrapper positionWrapper sh ]

        Group shapes ->
            Svg.g [] <|
                List.indexedMap
                    (\n ->
                        createSVG
                            (id ++ "g" ++ String.fromInt n)
                            w
                            h
                            trans
                            msgWrapper
                            positionWrapper
                    )
                    shapes

        GroupOutline cmbndshp ->
            createSVG id w h trans msgWrapper positionWrapper cmbndshp

        GraphPaper s th c ->            
            if th <= 0 || s < 2 * th then
                Svg.g [] []

            else
                createSVG id w h trans msgWrapper positionWrapper <|
                    createGraph ( w, h ) s th c


{-| Display HTML inside an SVG foreignObject.
-}
html : Float -> Float -> Html.Html userMsg -> Shape userMsg
html w h htm =
    ForeignObject w h htm



--Filling / outlining functions


{-| Fill a `Stencil` with a `Color`, creating a `Shape`;
Note that any `Stencil` converted to a `Shape` this way can be made
transparent (for instance by using the `blank` `Color`) but will not be
"click-through", meaning that the body will catch any attached notifications
and not let them through to any visible `Shape`(s) revealed beneath them.

    circle 10
        |> filled red

-}
filled : Color -> Stencil -> Shape userMsg
filled color stencil =
    Inked (Just color) Nothing stencil


{-| Make a `Shape` into a ghost. Mostly to be used inside of the clip operations.
Note that although the `blank` `Color` is transparent, it will still catch
notifications enabled on the `Shape` or block them if not enabled on the `Shape`.
-}
ghost : Stencil -> Shape userMsg
ghost stencil =
    Inked (Just blank) Nothing stencil


{-| Repaint an already-`filled` `Shape`. This is helpful for repainting every `Shape` inside a `group` as well.

Repaints the outline the same color as the body of the shape including the outline, if used;
Note that this can repaint with a transparent `Color` (for instance, the `blank` `Color`) but
will never have the ability to be "click-through" so that it will always block (or catch)
notifications to covered `Shape`(s) revealed beneath.

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
        Inked _ Nothing st ->
            Inked (Just color) Nothing st

        Inked _ (Just ( lt, _ )) st ->
            Inked (Just color) (Just ( lt, color )) st

        Move s sh ->
            Move s (repaint color sh)

        Rotate r sh ->
            Rotate r (repaint color sh)

        Scale sx sy sh ->
            Scale sx sy (repaint color sh)

        Skew skx sky sh ->
            Skew skx sky (repaint color sh)

        Transformed tm sh ->
            Transformed tm (repaint color sh)

        Group shapes ->
            Group (List.map (repaint color) shapes)

        GroupOutline cmbndshp ->
            GroupOutline (repaint color cmbndshp)

        Link s sh ->
            Link s (repaint color sh)

        AlphaMask reg sh ->
            AlphaMask reg (repaint color sh)

        Clip reg sh ->
            Clip reg (repaint color sh)

        Tap userMsg sh ->
            Tap userMsg (repaint color sh)

        TapAt userMsg sh ->
            TapAt userMsg (repaint color sh)

        EnterShape userMsg sh ->
            EnterShape userMsg (repaint color sh)

        EnterAt userMsg sh ->
            EnterAt userMsg (repaint color sh)

        Exit userMsg sh ->
            Exit userMsg (repaint color sh)

        ExitAt userMsg sh ->
            ExitAt userMsg (repaint color sh)

        MouseDown userMsg sh ->
            MouseDown userMsg (repaint color sh)

        MouseDownAt userMsg sh ->
            MouseDownAt userMsg (repaint color sh)

        MouseUp userMsg sh ->
            MouseUp userMsg (repaint color sh)

        MouseUpAt userMsg sh ->
            MouseUpAt userMsg (repaint color sh)

        MoveOverAt userMsg sh ->
            MoveOverAt userMsg (repaint color sh)

        TouchStart userMsg sh ->
            TouchStart userMsg (repaint color sh)

        TouchEnd userMsg sh ->
            TouchEnd userMsg (repaint color sh)

        TouchStartAt userMsg sh ->
            TouchStartAt userMsg (repaint color sh)

        TouchEndAt userMsg sh ->
            TouchEndAt userMsg (repaint color sh)

        TouchMoveAt userMsg sh ->
            TouchMoveAt userMsg (repaint color sh)

        -- no changes for the rest...
        ForeignObject w h htm ->
            ForeignObject w h htm

        Everything ->
            Everything

        Notathing ->
            Notathing

        GraphPaper s th _ ->
            GraphPaper s th color


{-| Outline a Stencil with a `LineType` and `Color`, creating a `Shape`;
Note that this is the only way to convert a `Stencil` into a `Shape` that is
"click-through" as if the body is not just transparent but doesn't exist at all.
It works for all `Stencil`'s except for `Text` which can never be made "click-through"
due to a permanent transparent background area that cannot be disabled.

    circle 10
        |> outlined (solid 5) red

-}
outlined : LineType -> Color -> Stencil -> Shape userMsg
outlined style outlineClr stencil =
    let
        lineStyle =
            case style of
                NoLine ->
                    Nothing

                _ ->
                    Just ( style, outlineClr )
    in
    Inked Nothing lineStyle stencil


{-| Add an outline to an already-filled `Shape`.

When applied to `Group`'s including `Shape`'s that have had `union` applied to them,
the outline will have half the expected stroke width only outside the `Shape`'s and
for `Shape`'s with `clip` or `subtract` applied to them, the half width stroke will be
interior to the composite shapes displayed.

The limitation is that when displaying "clip's" or "subtract's" containing "Group's",
the outline for the "Group's" won't be visibile at all (less common), nor will the
outline's for "clip's" or "subtract's" inside "Group's" be visible (more common);
also, the outline for the clipping or subtraction pattern may not be as expected when
the pattern is a "Group" as it won't appear at all in a "clip".

In these cases the workaround is just as before this was made available, to build up
the outlines desired using combinations of other shapes such as curves, clipped or
subtracted shapes, convential pre-applied outlines, etc.

Note that when applied to `Group`(s), `Clip`(s), and `AlphaMask`(s) (from subtracts),
the body colour may still be transparent so that `Shape`(s) can be revealed beneath,
but the bodies are not "click-through" so that notifications can be passed through to them,
and the transparent bodies can still capture notifications enabled on the resulting `Shape`(s).

    circle 10
        |> filled red
        |> addOutline (solid 5) white

-}
addOutline : LineType -> Color -> Shape userMsg -> Shape userMsg
addOutline style outlineClr shape =
    let
        lineStyle =
            case style of
                NoLine ->
                    Nothing

                _ ->
                    Just ( style, outlineClr )
    in
    case shape of
        Inked clr _ st ->
            Inked clr lineStyle st

        Move s sh ->
            Move s (addOutline style outlineClr sh)

        Rotate r sh ->
            Rotate r (addOutline style outlineClr sh)

        Scale sx sy sh ->
            Scale sx sy (addOutline style outlineClr sh)

        Skew skx sky sh ->
            Skew skx sky (addOutline style outlineClr sh)

        Transformed tm sh ->
            Transformed tm (addOutline style outlineClr sh)

        Group list ->
            let
                innerlist =
                    List.filterMap
                        (\shp ->
                            case shp of
                                -- remove old outline shape
                                GroupOutline _ ->
                                    Nothing

                                _ ->
                                    Just <| addOutline NoLine black shp
                        )
                        list
            in
            case innerlist of
                [] ->
                    {- should never happen -}
                    Group []

                hd :: [] ->
                    addOutline style outlineClr hd

                _ ->
                    if lineStyle == Nothing then
                        Group innerlist

                    else
                        let
                            outlnshp =
                                GroupOutline <|
                                    subtract
                                        (Group innerlist)
                                        (Group
                                            (List.map
                                                (addOutline style outlineClr)
                                                innerlist
                                            )
                                        )
                        in
                        Group <| innerlist ++ [ outlnshp ]

        -- don't add an outline to one that's already been added but it should
        -- never get here as GroupOutline should always be inside a Group which
        -- will already have processed it, whether to change or remove it...
        GroupOutline cmbndshp ->
            GroupOutline cmbndshp

        AlphaMask reg sh ->
            let
                ptrn =
                    addOutline NoLine black reg

                inside =
                    addOutline NoLine black sh
            in
            if lineStyle == Nothing then
                AlphaMask ptrn inside

            else
                let
                    ptrnlnd =
                        addOutline style outlineClr reg

                    newshp =
                        addOutline style outlineClr sh

                    ptrnoutln =
                        Clip inside ptrnlnd

                    shpoutln =
                        Clip inside newshp
                in
                AlphaMask ptrn <|
                    Group
                        [ inside
                        , GroupOutline <| Group [ shpoutln, ptrnoutln ]
                        ]

        Clip reg sh ->
            let
                ptrn =
                    addOutline NoLine black reg

                inside =
                    addOutline NoLine black sh
            in
            if lineStyle == Nothing then
                Clip ptrn inside

            else
                let
                    ptrnlnd =
                        addOutline style outlineClr (reg |> repaint blank)

                    newshp =
                        addOutline style outlineClr sh

                    ptrnoutln =
                        Clip inside ptrnlnd

                    shpoutln =
                        Clip inside newshp
                in
                Clip ptrn <|
                    Group
                        [ inside
                        , GroupOutline <| Group [ shpoutln, ptrnoutln ]
                        ]

        Link s sh ->
            Link s (addOutline style outlineClr sh)

        Tap userMsg sh ->
            Tap userMsg (addOutline style outlineClr sh)

        TapAt userMsg sh ->
            TapAt userMsg (addOutline style outlineClr sh)

        EnterShape userMsg sh ->
            EnterShape userMsg (addOutline style outlineClr sh)

        EnterAt userMsg sh ->
            EnterAt userMsg (addOutline style outlineClr sh)

        Exit userMsg sh ->
            Exit userMsg (addOutline style outlineClr sh)

        ExitAt userMsg sh ->
            ExitAt userMsg (addOutline style outlineClr sh)

        MouseDown userMsg sh ->
            MouseDown userMsg (addOutline style outlineClr sh)

        MouseDownAt userMsg sh ->
            MouseDownAt userMsg (addOutline style outlineClr sh)

        MouseUp userMsg sh ->
            MouseUp userMsg (addOutline style outlineClr sh)

        MouseUpAt userMsg sh ->
            MouseUpAt userMsg (addOutline style outlineClr sh)

        MoveOverAt userMsg sh ->
            MoveOverAt userMsg (addOutline style outlineClr sh)

        TouchStart userMsg sh ->
            TouchStart userMsg (addOutline style outlineClr sh)

        TouchEnd userMsg sh ->
            TouchEnd userMsg (addOutline style outlineClr sh)

        TouchStartAt userMsg sh ->
            TouchStartAt userMsg (addOutline style outlineClr sh)

        TouchEndAt userMsg sh ->
            TouchEndAt userMsg (addOutline style outlineClr sh)

        TouchMoveAt userMsg sh ->
            TouchMoveAt userMsg (addOutline style outlineClr sh)

        -- no changes for the rest...
        ForeignObject w h htm ->
            ForeignObject w h htm

        Everything ->
            Everything

        Notathing ->
            Notathing

        GraphPaper s th clr ->
            GraphPaper s th clr


multAlpha : Color.Color -> Float -> Color.Color
multAlpha color n =
    let
        colRec = Color.toRgba color
    in
        Color.fromRgba { colRec | alpha = colRec.alpha * n }

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
        Inked Nothing (Just ( lineType, Solid colour )) st ->
            Inked Nothing (Just ( lineType, Solid (multAlpha colour alpha))) st

        Inked (Just (Solid colour)) (Just ( lineType, Solid sColour )) st ->
            Inked (Just <| Solid (multAlpha colour alpha))
                  (Just ( lineType, Solid <| multAlpha sColour alpha)) st

        Inked (Just (Solid colour)) Nothing st ->
            Inked (Just <| Solid (multAlpha colour alpha)) Nothing st

        Inked a b c ->
            Inked a b c

        ForeignObject w h htm ->
            ForeignObject w h htm

        Move s sh ->
            Move s (makeTransparent alpha sh)

        Rotate r sh ->
            Rotate r (makeTransparent alpha sh)

        Scale sx sy sh ->
            Scale sx sy (makeTransparent alpha sh)

        Skew skx sky sh ->
            Skew skx sky (makeTransparent alpha sh)

        Transformed tm sh ->
            Transformed tm (makeTransparent alpha sh)

        Group list ->
            Group (List.map (makeTransparent alpha) list)

        GroupOutline cmbndshp ->
            GroupOutline (makeTransparent alpha cmbndshp)

        Link s sh ->
            Link s (makeTransparent alpha sh)

        AlphaMask reg sh ->
            AlphaMask reg (makeTransparent alpha sh)

        Clip reg sh ->
            Clip reg (makeTransparent alpha sh)

        Everything ->
            Everything

        Notathing ->
            Notathing

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

        GraphPaper s th (Solid colour) ->
            GraphPaper s th (Solid <| multAlpha colour alpha)

        GraphPaper s th (Gradient gr) ->
            GraphPaper s th (Gradient gr)



--Line styles


{-| Define a`LineType` that doesn't exist, doesn't appear.
-}
noline : () -> LineType
noline () =
    NoLine


{-| Define a solid `LineType` with the given width.
-}
solid : Float -> LineType
solid th =
    Unbroken th


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
    Scale s s shape


{-| Scale a `Shape` in the x-axis by a given factor.
-}
scaleX : Float -> Shape userMsg -> Shape userMsg
scaleX s shape =
    Scale s 1 shape


{-| Scale a `Shape` in the y-axis by a given factor.
-}
scaleY : Float -> Shape userMsg -> Shape userMsg
scaleY s shape =
    Scale 1 s shape


{-| Flip a `Shape` along the x-axis.
-}
mirrorX : Shape userMsg -> Shape userMsg
mirrorX shape =
    Scale -1 1 shape


{-| Flip a `Shape` along the y-axis.
-}
mirrorY : Shape userMsg -> Shape userMsg
mirrorY shape =
    Scale 1 -1 shape


{-| Skew a `Shape` along the x-axis.
-}
skewX : Float -> Shape userMsg -> Shape userMsg
skewX skx shape =
    Skew skx 0 shape


{-| Skew a `Shape` along the y-axis.
-}
skewY : Float -> Shape userMsg -> Shape userMsg
skewY sky shape =
    Skew 0 sky shape


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
    rgba r g b 1


{-| Define a colour given its red, green, blue and alpha components.
Alpha is a decimal number (`Float`) from 0 to 1 representing the level of transparency.
-}
rgba : Float -> Float -> Float -> Float -> Color
rgba r g b a =
    Solid <| Color.fromRgba { red = ssc r, green = ssc g, blue = ssc  b, alpha = ssa a }


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


getAlpha : Color.Color -> Float
getAlpha colour =
    (Color.toRgba colour).alpha

mkAlpha : Color.Color -> String
mkAlpha =
    String.fromFloat << getAlpha


mkRGB : Color.Color -> String
mkRGB colour =
    let
        col = Color.toRgba colour
    in
    "rgba(" ++ (String.fromFloat col.red) ++ "," ++ (String.fromFloat col.green) ++ "," ++ (String.fromFloat col.blue) ++ "," ++ (String.fromFloat col.alpha) ++ ")"


{-| Define a colour given its hue, saturation and light components.
-}
hsl : Float -> Float -> Float -> Color
hsl h s l =
    case convert h s l of
        ( r, g, b ) ->
            rgba r g b 1


{-| Define a colour given its hue, saturation, light and alpha components.
Alpha is a decimal number (`Float`) from 0 to 1 representing the level of transparency.
-}
hsla : Float -> Float -> Float -> Float -> Color
hsla h s l a =
    case convert h s l of
        ( r, g, b ) ->
            rgba r g b a



-- Clip path functionality


{-| Clipping to a shape

Cut out the `Shape` on the right using the `Shape` on the left.

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

Subtract the `Shape` on the left from the `Shape` on the right.

-}
subtract : Shape userMsg -> Shape userMsg -> Shape userMsg
subtract shape1 shape2 =
    AlphaMask shape1 shape2


{-| The whole region outside the given `Shape`.
-}
outside : Shape userMsg -> Shape userMsg
outside shape =
    AlphaMask shape shape



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
    rgba 255 105 180 1


{-| -}
hotPink : Color
hotPink =
    rgba 255 0 66 1


{-| -}
lightRed : Color
lightRed =
    rgba 239 41 41 1


{-| -}
red : Color
red =
    rgba 204 0 0 1


{-| -}
darkRed : Color
darkRed =
    rgba 164 0 0 1


{-| -}
lightOrange : Color
lightOrange =
    rgba 252 175 62 1


{-| -}
orange : Color
orange =
    rgba 245 121 0 1


{-| -}
darkOrange : Color
darkOrange =
    rgba 206 92 0 1


{-| -}
lightYellow : Color
lightYellow =
    rgba 255 233 79 1


{-| -}
yellow : Color
yellow =
    rgba 237 212 0 1


{-| -}
darkYellow : Color
darkYellow =
    rgba 196 160 0 1


{-| -}
lightGreen : Color
lightGreen =
    rgba 138 226 52 1


{-| -}
green : Color
green =
    rgba 115 210 22 1


{-| -}
darkGreen : Color
darkGreen =
    rgba 78 154 6 1


{-| -}
lightBlue : Color
lightBlue =
    rgba 114 159 207 1


{-| -}
blue : Color
blue =
    rgba 52 101 164 1


{-| -}
darkBlue : Color
darkBlue =
    rgba 32 74 135 1


{-| -}
lightPurple : Color
lightPurple =
    rgba 173 127 168 1


{-| -}
purple : Color
purple =
    rgba 117 80 123 1


{-| -}
darkPurple : Color
darkPurple =
    rgba 92 53 102 1


{-| -}
lightBrown : Color
lightBrown =
    rgba 233 185 110 1


{-| -}
brown : Color
brown =
    rgba 193 125 17 1


{-| -}
darkBrown : Color
darkBrown =
    rgba 143 89 2 1


{-| -}
black : Color
black =
    rgba 0 0 0 1


{-| -}
white : Color
white =
    rgba 255 255 255 1


{-| -}
lightGrey : Color
lightGrey =
    rgba 238 238 236 1


{-| -}
grey : Color
grey =
    rgba 211 215 207 1


{-| -}
darkGrey : Color
darkGrey =
    rgba 186 189 182 1


{-| -}
lightGray : Color
lightGray =
    rgba 238 238 236 1


{-| -}
gray : Color
gray =
    rgba 211 215 207 1


{-| -}
darkGray : Color
darkGray =
    rgba 186 189 182 1


{-| -}
lightCharcoal : Color
lightCharcoal =
    rgba 136 138 133 1


{-| -}
charcoal : Color
charcoal =
    rgba 85 87 83 1


{-| -}
darkCharcoal : Color
darkCharcoal =
    rgba 46 52 54 1


{-| -}
blank : Color
blank =
    rgba 0 0 0 0
