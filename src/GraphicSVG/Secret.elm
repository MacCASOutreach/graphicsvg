module GraphicSVG.Secret exposing 
    (Stencil(..), Shape(..), Color(..), Gradient(..), Stop(..), Transform, LineType(..), FontAlign(..), Face(..), Font(..), Pull(..))

{-| Advanced Secret module! This is for people who want to access the
underlying types in the library so you can do advanced things. Most people
don't need this much detail! I recommend you look at the source of this 
module to determine how to use these.

# Shapes and Stencils
@docs Stencil, Shape

# Colours and Gradients
@docs Color, Gradient, Stop

# Raw Transformations
@docs Transform

# LineTypes
@docs LineType

# Text and Fonts
@docs FontAlign, Face, Font

# Curve Pulls
@docs Pull

-}

import Html
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

{-| The `Color` type is used for filling or outlining a `Stencil`.
-}
type Color
    = Solid Color.Color
    | Gradient Gradient

{-| A type representing radial and linear gradients.
-}
type Gradient =
      RadialGradient (List Stop)
    | LinearGradient Float {- rotation -} (List Stop)

{-| A type representing stops in a gradient. Consists of one constructor 
with inputs for the position, transparency and colour.
-}
type Stop =
    Stop Float {- stop position -} Float {- transparency -} Color.Color {- colour -}


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


{-| The `LineType` type is used to define the appearance of an outline for a `Stencil`.
`LineType` also defines the appearence of `line` and `curve`.
-}
type LineType
    = NoLine
    | Unbroken Float
    | Broken (List ( Float, Float )) Float


{-| A simple algebraic data type with three constructors:
- AlignLeft
- AlignCentred
- AlignRight
-}
type FontAlign
    = AlignLeft
    | AlignCentred
    | AlignRight



{-| The `Face` type describes the appearance of a text `Stencil`.
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