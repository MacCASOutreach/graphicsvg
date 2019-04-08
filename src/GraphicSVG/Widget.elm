module GraphicSVG.Widget exposing (icon,Model,Msg,init,subscriptions,update,view,ViewOption,defaultViewOption,noViewOption,viewCustom)

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

@docs Msg, Model

## Initialization
Some helper functions for initializing your widget(s). 

@docs init, subscriptions

## Updating a widget model

@docs update

## Rendering a widget

@docs view

### Custom view
When the default resizing behaviour of the widget is limiting, use the functions 
from this section.

@docs ViewOption

@docs defaultViewOption

@docs noViewOption

@docs viewCustom
-}

import GraphicSVG exposing(Shape(..), createSVG, ident)
import Html
import Svg
import Svg.Attributes exposing(width,height,viewBox,clipPath,x,y,id)
import Html.Attributes exposing(style)
import Task
import Browser.Dom exposing (Viewport, getViewportOf)
import Browser.Events exposing(onResize)

{-|A static graphic that returns no messages. Use this when you don't need to
handle messages about clicking shapes, getting mouse positions, etc. This function
is simpler and requires no other modifications to your model and message types.

Then, use `icon` as follows in your view function:
```
icon "Circle" 50 50
    [
        circle 5
            |> filled blue
    ]
```
Note that including any of the `notify*` functions will result in a type error.
This is because the type `Shape Never` is a shape which can never send any
messages. This is great! It means you can't compile with messages accidentally.
It also means that, for compatibility with older code, you might need to change
that code's type signature to be more generic (e.g. Shape a). And, for older
code with any `notify*`, you'll have to use a proper `Widget` (below), even if
you don't plan on using the messages.
-}
icon : String -> Float -> Float -> List (Shape Never) -> Html.Html a
icon iid w h shapes =
    Svg.svg
        [ viewBox
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
        <| cPath iid w h
            :: [ Svg.g
                    [ clipPath ("url(#cPath"++iid++")") ]
                    <| List.indexedMap
                        (\n -> createSVG (iid ++ String.fromInt n) w h ident never (\toMsg xy -> never <| toMsg xy))
                        shapes
                    
               ]

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
type alias Model = 
    {
        cw : Float
    ,   ch : Float
    ,   ww : Float
    ,   wh : Float
    ,   id : String
    }

{-|The messages a widget can send and receive. It is only necessary to pipe these
by including them in your message type. Ensure that you send each and every message
received in your main update function to the `Widget.update` function to maintain
consistency. See the above discussion under `Wrapper` for a more detailed idea of
how to use this.
-}
type Msg =
    WidgetResize (Maybe (Float,Float))

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

init : Float -> Float -> String -> (Model, Cmd Msg)
init w h id = 
    ({
        cw = w
    ,   ch = h
    ,   ww = 0
    ,   wh = 0
    ,   id = id
    },
    getContainerSize id
    )

getContainerSize id =
    Task.attempt 
        (\rvp -> case rvp of
                    Ok vp -> WidgetResize <| Just (vp.viewport.width, vp.viewport.height)
                    _ -> WidgetResize <| Just (0,0)
                    ) (getViewportOf id)

{-|Generate a subscription related to each widget in your app. It must be
active whenever the widget in question is onscreen. It can be active when the widget
is not onscreen, however this is not necessary and may be a waste of resources.
-}
subscriptions : Sub Msg
subscriptions =
    onResize (\_ _ -> WidgetResize Nothing)

{-|Helper function to update the state of the widget. This can be considered a
black box and should be used to simply update the Model stored inside of your
app's model. Be sure to pipe every message through this update, to ensure the
consistency of the widget's model and that you receive all notifications coming
from the widget.
-}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        WidgetResize mWH ->
            case mWH of
                Just (w,h) ->
                    ({ model | ww = w, wh = h }, Cmd.none)
                Nothing ->
                    ( model, getContainerSize model.id )


cPath : String -> Float -> Float -> Svg.Svg userMsg
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
{-|Options to set a Widget's attributes in a more constrained way.
-}
type ViewOption userMsg =
      ViewWidth String
    | ViewHeight String
    | ViewStyle String String

{-|The default view options where a widget resizes to the size of the container
it's inside of. This is the view option used in the non-custom `view` function.
-}
defaultViewOption : List (ViewOption userMsg)
defaultViewOption =
    [
        ViewWidth "100%"
    ,   ViewHeight "100%"
    ]

{-|A type of view that sets no style attributes limiting the size of the widget
as part of your Html.
-}
noViewOption : List (ViewOption userMsg)
noViewOption = []

convertViewOption : ViewOption userMsg -> Html.Attribute userMsg
convertViewOption vo =
    case vo of
        ViewWidth w -> style "width" w
        ViewHeight h -> style "height" h
        ViewStyle s o -> style s o

{-|Helper function which takes the widget's opaque model as well as a list
of shapes to include in the widget. Widgets are compatible with all
notification functions available in GraphicSVG. Each widget is given its
own co-ordinate system defined by the widget's model. The `*At` notification
functions return points relative to the widget (as long as the user has
properly piped together the commands and subscriptions to their copy of 
`Widget.Model`. Widgets are clipped so they fit the aspect ratio defined in 
the widget's model.
-}
view : Model -> List (Shape userMsg) -> Html.Html userMsg
view = viewCustom defaultViewOption

{-|Like `view`, but with the ability to set view options that determine
how the widget fits into your existing Html.

See `view` for more details.
-}
viewCustom : List (ViewOption userMsg) -> Model -> List (Shape userMsg) -> Html.Html userMsg
viewCustom viewOptions model shapes =
    let
        positionWrapper toMsg (x,y) = toMsg <| convertCoords model.ww model.wh model.cw model.ch (x,y)
    in
    Svg.svg
        ([ id model.id
         , viewBox
            (String.fromFloat (-model.cw / 2)
                ++ " "
                ++ String.fromFloat (-model.ch / 2)
                ++ " "
                ++ String.fromFloat model.cw
                ++ " "
                ++ String.fromFloat model.ch
            )
        ] ++ List.map convertViewOption viewOptions)
        (cPath model.id model.cw model.ch
            :: [ Svg.g
                    [ clipPath ("url(#cPath"++model.id++")") ]
                    (List.indexedMap
                        (\n -> createSVG (model.id ++ String.fromInt n) model.cw model.ch ident identity positionWrapper)
                        shapes
                    )
               ]
        )


convertCoords : Float -> Float -> Float -> Float -> ( Float, Float ) -> ( Float, Float )
convertCoords ww sh cw ch ( x, y ) =
    let
        aspectout =
            if not (sh == 0) then
                ww / sh

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
                ww / cw

            else if scaledInY then
                sh / ch

            else
                1
    in
    ( (x - ww / 2) / cscale
    , (y + sh / 2) / cscale
    )