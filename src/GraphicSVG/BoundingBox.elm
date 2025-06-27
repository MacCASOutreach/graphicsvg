module GraphicSVG.BoundingBox exposing (..)
import GraphicSVG.Types exposing (
    BoundingBox,
    Shape(..),
    Transform,
    Stencil(..),
    Face(..),
    Font(..),
    FontAlign(..)
    
    )
import GraphicSVG.Transforms exposing (ident, moveT, rotateT, scaleT, skewT, matrixMult)

-- Compute the bounding box of a shape with an accumulated transformation
getBoundingBox : Shape userMsg -> BoundingBox
getBoundingBox shape =
    getBoundingBoxWithTransform shape ident

-- Helper function to compute bounding box with an accumulated transformation matrix
getBoundingBoxWithTransform : Shape userMsg -> Transform -> BoundingBox
getBoundingBoxWithTransform shape trans =
    case shape of
        Inked _ _ stencil ->
            stencilBoundingBox stencil |> applyTransform trans

        ForeignObject w h _ ->
            { minX = -w / 2, minY = -h / 2, maxX = w / 2, maxY = h / 2 }
                |> applyTransform trans

        Move (dx, dy) sh ->
            getBoundingBoxWithTransform sh (moveT (dx, dy) trans)

        Rotate angle sh ->
            getBoundingBoxWithTransform sh (rotateT angle trans)

        Scale sx sy sh ->
            getBoundingBoxWithTransform sh (scaleT sx sy trans)

        Skew skx sky sh ->
            getBoundingBoxWithTransform sh (skewT skx sky trans)

        Transformed tm sh ->
            getBoundingBoxWithTransform sh (matrixMult trans tm)

        Group shapes ->
            case shapes of
                [] ->
                    { minX = 0, minY = 0, maxX = 0, maxY = 0 }
                _ ->
                    let
                        boxes = List.map (\sh -> getBoundingBoxWithTransform sh trans) shapes
                        minXs = List.map .minX boxes
                        minYs = List.map .minY boxes
                        maxXs = List.map .maxX boxes
                        maxYs = List.map .maxY boxes
                    in
                    { minX = List.minimum minXs |> Maybe.withDefault 0
                    , minY = List.minimum minYs |> Maybe.withDefault 0
                    , maxX = List.maximum maxXs |> Maybe.withDefault 0
                    , maxY = List.maximum maxYs |> Maybe.withDefault 0
                    }

        GroupOutline sh ->
            getBoundingBoxWithTransform sh trans

        AlphaMask region sh ->
            intersectBoundingBox
                (getBoundingBoxWithTransform region trans)
                (getBoundingBoxWithTransform sh trans)

        Clip region sh ->
            intersectBoundingBox
                (getBoundingBoxWithTransform region trans)
                (getBoundingBoxWithTransform sh trans)

        Everything ->
            { minX = -10000, minY = -10000, maxX = 10000, maxY = 10000 }

        Notathing ->
            { minX = 0, minY = 0, maxX = 0, maxY = 0 }

        Link _ sh ->
            getBoundingBoxWithTransform sh trans

        Tap _ sh ->
            getBoundingBoxWithTransform sh trans

        TapAt _ sh ->
            getBoundingBoxWithTransform sh trans

        EnterShape _ sh ->
            getBoundingBoxWithTransform sh trans

        EnterAt _ sh ->
            getBoundingBoxWithTransform sh trans

        Exit _ sh ->
            getBoundingBoxWithTransform sh trans

        ExitAt _ sh ->
            getBoundingBoxWithTransform sh trans

        MouseDown _ sh ->
            getBoundingBoxWithTransform sh trans

        MouseDownAt _ sh ->
            getBoundingBoxWithTransform sh trans

        MouseUp _ sh ->
            getBoundingBoxWithTransform sh trans

        MouseUpAt _ sh ->
            getBoundingBoxWithTransform sh trans

        MoveOverAt _ sh ->
            getBoundingBoxWithTransform sh trans

        TouchStart _ sh ->
            getBoundingBoxWithTransform sh trans

        TouchEnd _ sh ->
            getBoundingBoxWithTransform sh trans

        TouchStartAt _ sh ->
            getBoundingBoxWithTransform sh trans

        TouchEndAt _ sh ->
            getBoundingBoxWithTransform sh trans

        TouchMoveAt _ sh ->
            getBoundingBoxWithTransform sh trans

        GraphPaper _ _ _ ->
            { minX = -10000, minY = -10000, maxX = 10000, maxY = 10000 }

-- Apply a transformation matrix to a bounding box
applyTransform : Transform -> BoundingBox -> BoundingBox
applyTransform ( (a, c, e), (b, d, f) ) box =
    let
        corners = [ (box.minX, box.minY), (box.minX, box.maxY), (box.maxX, box.minY), (box.maxX, box.maxY) ]
        transformed = List.map (\(x, y) -> (a * x + c * y + e, b * x + d * y + f)) corners
        xs = List.map Tuple.first transformed
        ys = List.map Tuple.second transformed
    in
    { minX = List.minimum xs |> Maybe.withDefault 0
    , minY = List.minimum ys |> Maybe.withDefault 0
    , maxX = List.maximum xs |> Maybe.withDefault 0
    , maxY = List.maximum ys |> Maybe.withDefault 0
    }

-- Compute the bounding box of a stencil
stencilBoundingBox : Stencil -> BoundingBox
stencilBoundingBox stencil =
    case stencil of
        Circle r ->
            { minX = -r, minY = -r, maxX = r, maxY = r }

        Rect w h ->
            { minX = -w / 2, minY = -h / 2, maxX = w / 2, maxY = h / 2 }

        RoundRect w h _ ->
            { minX = -w / 2, minY = -h / 2, maxX = w / 2, maxY = h / 2 }

        Oval w h ->
            { minX = -w / 2, minY = -h / 2, maxX = w / 2, maxY = h / 2 }

        Polygon points ->
            pointsBoundingBox points

        Path points ->
            pointsBoundingBox points

        BezierPath start points ->
            let
                allPoints = start :: List.concatMap (\(p1, p2) -> [p1, p2]) points
            in
            pointsBoundingBox allPoints


        Text (Face size bold italic _ _ _ font align) str ->
            let
                -- Base width estimate: adjust for font type
                charWidthFactor =
                    case font of
                        Serif -> 0.65
                        Sansserif -> 0.6
                        FixedWidth -> 0.7
                        Custom "Arial" -> 0.55 
                        Custom _ -> 0.6 -- Default for other custom fonts

                charWidth = size * charWidthFactor
                width = toFloat (String.length str) * charWidth
                height = size * 1.2
                -- Adjust x bounds based on text alignment
                (xMin, xMax) =
                    case align of
                        AlignLeft -> (0, width)
                        AlignCentred -> (-width / 2, width / 2)
                        AlignRight -> (-width, 0)
            in
            { minX = xMin
            , minY = -height / 2
            , maxX = xMax
            , maxY = height / 2
            }

-- Compute bounding box for a list of points
pointsBoundingBox : List (Float, Float) -> BoundingBox
pointsBoundingBox points =
    case points of
        [] ->
            { minX = 0, minY = 0, maxX = 0, maxY = 0 }
        _ ->
            let
                xs = List.map Tuple.first points
                ys = List.map Tuple.second points
            in
            { minX = List.minimum xs |> Maybe.withDefault 0
            , minY = List.minimum ys |> Maybe.withDefault 0
            , maxX = List.maximum xs |> Maybe.withDefault 0
            , maxY = List.maximum ys |> Maybe.withDefault 0
            }

-- Intersect two bounding boxes
intersectBoundingBox : BoundingBox -> BoundingBox -> BoundingBox
intersectBoundingBox box1 box2 =
    { minX = max box1.minX box2.minX
    , minY = max box1.minY box2.minY
    , maxX = min box1.maxX box2.maxX
    , maxY = min box1.maxY box2.maxY
    }