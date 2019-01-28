module GraphicSVG.Widget exposing (icon,Wrapper,Model,Msg,init,subscriptions,update,view)

{-| Include GraphicSVG animations and functionality within your own elm/html
app! Your existing code can utilize as many widgets as it likes; each one
receives its own co-ordinate system and view!

[See GitHub](https://github.com/MacCASOutreach/graphicsvg/tree/master/src/Templates/Widget/Widget.elm)
for a full example of widgets embedded inside Elm Html.

# Static Icons
Static icons can render shapes without requiring you to modify your models and messages
to include special things, as in the case of the Interactive Widgets below. Use this when
you don't need the user to interact with the shapes (i.e. you don't need to _receieve_
any messages from the shape). For example, avatars and icons can use this.

@docs icon

# Interactive Widgets
## Basic Types
The basic message and model types must be wrapped and contained within your app's
message type and model type respectively. These are mostly opaque data types of which
you do not need to know the gritty details. Simply include them in your app and
you're all set!

@docs Wrapper, Msg, Model

## Initialization
Some helper functions for initializing your widget(s). 

@docs init, subscriptions

## Updating a widget model

@docs update

## Rendering a widget

@docs view
-}

import GraphicSVG exposing(Shape(..), createSVG, ident)
import Html
import Svg
import Svg.Attributes exposing(width,height,style,viewBox,clipPath,x,y,id)
import Task
import Browser.Dom exposing (Viewport, getViewportOf)
import Browser.Events exposing(onResize)

{-|A static graphic that returns no messages. Use this when you don't need to
handle messages about clicking shapes, getting mouse positions, etc. This function
is simpler and requires no other modifications to your model and message types.

You must include a type like `NoOp` meaning "do nothing" as well as a unique ID for
the icon. For example to draw a circle with the name "Circle", use the following:

```
-- your message type should include a dummy NoOp message
type Msg =
      ..
    | NoOp
```
Then, use `icon` as follows in your view function:
```
icon NoOp "Circle" 50 50
    [
        circle 5
            |> filled blue
    ]
```
-}
icon : userMsg -> String -> Float -> Float -> List (Shape userMsg) -> Html.Html userMsg
icon noop iid w h shapes =
    Svg.map (\_ -> noop) <| Svg.svg
        [ width "100%"
        , height "100%"
        , viewBox
            (String.fromFloat (-w / 2)
                ++ " "
                ++ String.fromFloat (-h / 2)
                ++ " "
                ++ String.fromFloat w
                ++ " "
                ++ String.fromFloat h
            )
        , id iid
        ]
        (cPath iid w h
            :: [ Svg.g
                    [ clipPath ("url(#cPath"++iid++")") ]
                    (List.indexedMap
                        (\n -> createSVG (iid ++ String.fromInt n) w h ident)
                        shapes
                    )
               ]
        )

{-|A type alias representing a constructor of your type which wraps the
Widget.Msg type. This will allow you to catch and pipe all messages to the 
`Widget.update` function to maintain the states of your app's various widgets.
Most often, this is simply a constructor of your main type that looks something 
like:

```
-- your app's Message type
type Msg =
      ...
    | WidgetMessage Widget.Msg
    | ...
```

You can easily expand this constructor to include, for example, an integer or custom 
union type representing each of the widgets in your app. Your update function could 
then route the messages to where they need to go, and save the states of your various
widgets where they need to be saved!
-}
type alias Wrapper userMsg =
    Msg userMsg -> userMsg

{-|The state of a given widget. This is considered an opaque type; as such it
is not necessary to know what is inside for your app to function. It stores
information about the size of the widget and the co-ordinate system for purposes
of rendering and sending messages with mouse position. It must be kept up to date
using subscriptions and commands, and can be created with the `init` function. This
is most often included in your app's model, e.g.

```
-- your app's Model type
type alias Model = 
    {
        ...
    ,   widgetState : Widget.Model
        ...
    }
```
Several of these could easily be saved in a structure such as a dictionary
to maintain the state of several widgets at once!
-}
type alias Model userMsg = 
    {
        cw : Float
    ,   ch : Float
    ,   ww : Int
    ,   wh : Int
    ,   msgWrapper : Wrapper userMsg
    ,   id : String
    }

{-|The messages a widget can send and receive. It is only necessary to pipe these
by including them in your message type. Ensure that you send each and every message
received in your main update function to the `Widget.update` function to maintain
consistency. See the above discussion under `Wrapper` for a more detailed idea of
how to use this.
-}
type alias Msg userMsg
    = GraphicSVG.Msg userMsg

{-|The init function takes the
size of the widget as well as the wrapper message and a unique string identifier of the 
widget. You must ensure that the identifier is unique, otherwise undefined behaviour can
occur related to notification functions (especially the `*At` functions). You are free to 
change the size or idenfier at any time throughout the execution of your program, simply
by calling init again and storing the model. However, note that each time init is called
for a given widget, its accompanying command must also be executed.

**Note:** You should issue the command given by `init` each time the widget becomes 
visible again after being offscreen, to ensure that the co-ordinate system is kept 
up-to-date. Therefore, storing the command for use later is a good idea.
-}

init : Float -> Float -> Wrapper userMsg -> String -> (Model userMsg, Cmd userMsg)
init w h msgWrapper id = 
    ({
        cw = w
    ,   ch = h
    ,   ww = 0
    ,   wh = 0
    ,   msgWrapper = msgWrapper
    ,   id = id
    },
    getContainerSize id msgWrapper
    )

getContainerSize id msgWrapper =
    Cmd.map msgWrapper <| Task.attempt (\rvp -> case rvp of
                            Ok vp -> GraphicSVG.WindowResize <| Just (round vp.viewport.width, round vp.viewport.height)
                            _ -> GraphicSVG.NoOp
                            ) (getViewportOf id)

{-|Generate a subscription related to each widget in your app. It must be
active whenever the widget in question is onscreen. It can be active when the widget
is not onscreen, however this is not necessary and may be a waste of resources.
-}
subscriptions : Wrapper userMsg -> Sub userMsg
subscriptions msgWrapper =
    Sub.map msgWrapper <| onResize (\_ _ -> GraphicSVG.WindowResize Nothing)

{-|Helper function to update the state of the widget. This can be considered a
black box and should be used to simply update the Model stored inside of your
app's model. Be sure to pipe every message through this update, to ensure the
consistency of the widget's model and that you receive all notifications coming
from the widget.
-}
update : Msg userMsg -> Model userMsg -> (Model userMsg, Cmd userMsg)
update msg model =
    case msg of
        GraphicSVG.Graphics userMsg -> (model, Task.perform identity (Task.succeed userMsg))
        GraphicSVG.WindowResize mWH -> 
            case mWH of
                Just (w,h) -> ( {model | ww = w, wh = h}, Cmd.none)
                Nothing ->
                    (model, getContainerSize model.id model.msgWrapper)
        GraphicSVG.ReturnPosition toMsg (x,y) -> (model, Task.perform identity (Task.succeed <| toMsg ((x-toFloat model.ww/2)/toFloat model.ww*model.cw,(y+toFloat model.wh/2)/toFloat model.wh*model.ch)))
        GraphicSVG.NoOp -> (model, Cmd.none)

cPath : String -> Float -> Float -> Svg.Svg (Msg userMsg)
cPath id w h =
    Svg.defs []
        [ Svg.clipPath
            [ Svg.Attributes.id ("cPath"++id) ]
            [ Svg.rect
                [ width (String.fromFloat w)
                , height (String.fromFloat h)
                , x (String.fromFloat (-w / 2))
                , y (String.fromFloat (-h / 2))
                ]
                []
            ]
        ]

{-|Helper function which takes the widget's opaque model as well as a list
of shapes to include in the widget. Widgets are compatible with all
notification functions available in GraphicSVG. Each widget is given its
own co-ordinate system defined by the widget's model. The `*At` notification
functions return points relative to the widget. Widgets are clipped so they
fit the aspect ratio defined in the widget's model.
-}
view : Model userMsg -> List (Shape userMsg) -> Html.Html userMsg
view model shapes =
    Svg.map model.msgWrapper <| Svg.svg
        [ width "100%"
        , height "100%"
        , id model.id
        , viewBox
            (String.fromFloat (-model.cw / 2)
                ++ " "
                ++ String.fromFloat (-model.ch / 2)
                ++ " "
                ++ String.fromFloat model.cw
                ++ " "
                ++ String.fromFloat model.ch
            )
        ]
        (cPath model.id model.cw model.ch
            :: [ Svg.g
                    [ clipPath ("url(#cPath"++model.id++")") ]
                    (List.indexedMap
                        (\n -> createSVG (model.id ++ String.fromInt n) model.cw model.ch ident)
                        shapes
                    )
               ]
        )