module GraphicSVG.Statistics exposing (..)

import GraphicSVG exposing(Collage(..), serif)
import GraphicSVG.Secret exposing(Shape(..), Stencil(..))
import Dict exposing(Dict)
import Dict exposing (merge)


mergeDicts dict0 dict1 =
    Dict.merge
        (\k v dict -> dict |> Dict.insert k v)
        (\k v0 v1 dict -> dict |> Dict.insert k (v0+v1))
        (\k v dict -> dict |> Dict.insert k v)
        dict0
        dict1
        Dict.empty
countCollageStencilTypes : Collage userMsg -> Dict String Int
countCollageStencilTypes (Collage _ _ shapes) = 
    List.foldl mergeDicts Dict.empty <| List.map countStencilTypes shapes

countStencilTypes : Shape userMsg -> Dict String Int
countStencilTypes sh = 
    let
        singleShape name = Dict.singleton name 1
    in
    case sh of
        Inked _ _ stencil -> case stencil of
            Circle _        -> singleShape "circle"
            Rect _ _        -> singleShape "rectangle"
            RoundRect _ _ _ -> singleShape "rounded rectangle"
            Oval _ _        -> singleShape "oval"
            BezierPath _ _  -> singleShape "bezier path"
            Polygon _       -> singleShape "polygon"
            Path _          -> singleShape "path"
            Text _ _        -> singleShape "text"
        ForeignObject _ _ _ -> 
            singleShape "foreign object"
        Move _ shape -> 
            singleShape "move" 
                |> mergeDicts (countStencilTypes shape)
        Rotate _ shape -> 
            singleShape "rotate"
                |> mergeDicts (countStencilTypes shape)
        Scale _ _ shape -> 
            singleShape "scale"
                |> mergeDicts (countStencilTypes shape)
        Skew _ _ shape -> 
            singleShape "skew"
                |> mergeDicts (countStencilTypes shape)
        Transformed _ shape -> 
            singleShape "transformed"
                |> mergeDicts (countStencilTypes shape)
        Group shapes -> List.foldl mergeDicts Dict.empty <| List.map countStencilTypes shapes
        GroupOutline shape -> 
            singleShape "group outline"
                |> mergeDicts (countStencilTypes shape)
        AlphaMask shape0 shape1 -> 
            singleShape "alpha mask" 
                |> mergeDicts (countStencilTypes shape0) 
                |> mergeDicts (countStencilTypes shape1)
        Clip shape0 shape1 -> 
            singleShape "clip"
                |> mergeDicts (countStencilTypes shape0) 
                |> mergeDicts (countStencilTypes shape1)
        Everything -> singleShape "everything"
        Notathing -> singleShape "not a thing"
        Link _ shape -> 
            singleShape "link"
                |> mergeDicts (countStencilTypes shape)
        Tap _ shape -> 
            singleShape "tap"
                |> mergeDicts (countStencilTypes shape)
        TapAt _ shape -> 
            singleShape "tap at"
                |> mergeDicts (countStencilTypes shape)
        EnterShape _ shape -> 
            singleShape "enter shape"
                |> mergeDicts (countStencilTypes shape)
        EnterAt _ shape -> 
            singleShape "enter at"
                |> mergeDicts (countStencilTypes shape)
        Exit _ shape -> 
            singleShape "exist"
                |> mergeDicts (countStencilTypes shape)
        ExitAt _ shape -> 
            singleShape "exit at"
                |> mergeDicts (countStencilTypes shape)
        MouseDown _ shape -> 
            singleShape "mouse down"
                |> mergeDicts (countStencilTypes shape)
        MouseDownAt _ shape -> 
            singleShape "mouse down at"
                |> mergeDicts (countStencilTypes shape)
        MouseUp _ shape -> 
            singleShape "mouse up"
                |> mergeDicts (countStencilTypes shape)
        MouseUpAt _ shape -> 
            singleShape "mouse up at"
                |> mergeDicts (countStencilTypes shape)
        MoveOverAt _ shape -> 
            singleShape "move over at"
                |> mergeDicts (countStencilTypes shape)
        TouchStart _ shape -> 
            singleShape "touch start"
                |> mergeDicts (countStencilTypes shape)
        TouchEnd _ shape -> 
            singleShape "touch end"
                |> mergeDicts (countStencilTypes shape)
        TouchStartAt _ shape -> 
            singleShape "touch start at"
                |> mergeDicts (countStencilTypes shape)
        TouchEndAt _ shape -> 
            singleShape "touch end at"
                |> mergeDicts (countStencilTypes shape)
        TouchMoveAt _ shape -> 
            singleShape "touch move at"
                |> mergeDicts (countStencilTypes shape)
        GraphPaper _ _ _ -> Dict.empty

countCollageStencils : Collage userMsg -> Int
countCollageStencils (Collage _ _ shapes) =
    List.sum <| List.map countStencils shapes

countStencils : Shape userMsg -> Int
countStencils sh = case sh of
    Inked _ _ _ -> 1
    ForeignObject _ _ _ -> 1
    Move _ shape -> countStencils shape
    Rotate _ shape -> countStencils shape
    Scale _ _ shape -> countStencils shape
    Skew _ _ shape -> countStencils shape
    Transformed _ shape -> countStencils shape
    Group shapes -> List.sum <| List.map countStencils shapes
    GroupOutline shape -> countStencils shape
    AlphaMask shapes0 shapes1 -> countStencils shapes0 + countStencils shapes1
    Clip shape0 shape1 -> countStencils shape0 + countStencils shape1
    Everything -> 0
    Notathing -> 0
    Link _ shape -> countStencils shape
    Tap _ shape -> countStencils shape
    TapAt _ shape -> countStencils shape
    EnterShape _ shape -> countStencils shape
    EnterAt _ shape -> countStencils shape
    Exit _ shape -> countStencils shape
    ExitAt _ shape -> countStencils shape
    MouseDown _ shape -> countStencils shape
    MouseDownAt _ shape -> countStencils shape
    MouseUp _ shape -> countStencils shape
    MouseUpAt _ shape -> countStencils shape
    MoveOverAt _ shape -> countStencils shape
    TouchStart _ shape -> countStencils shape
    TouchEnd _ shape -> countStencils shape
    TouchStartAt _ shape -> countStencils shape
    TouchEndAt _ shape -> countStencils shape
    TouchMoveAt _ shape -> countStencils shape
    GraphPaper _ _ _ -> 0

collageDepth : Collage userMsg -> Int
collageDepth (Collage _ _ shapes) =
    Maybe.withDefault 0 <| List.maximum <| List.map groupDepth shapes

groupDepth : Shape userMsg -> Int
groupDepth sh = case sh of
    Inked _ _ _ -> 1
    ForeignObject _ _ _ -> 1
    Move _ shape -> groupDepth shape
    Rotate _ shape -> groupDepth shape
    Scale _ _ shape -> groupDepth shape
    Skew _ _ shape -> groupDepth shape
    Transformed _ shape -> groupDepth shape
    Group shapes -> 1 + (Maybe.withDefault 0 <| List.maximum <| List.map groupDepth shapes)
    GroupOutline shape -> groupDepth shape
    AlphaMask shapes0 shapes1 -> groupDepth shapes0 + groupDepth shapes1
    Clip shape0 shape1 -> groupDepth shape0 + groupDepth shape1
    Everything -> 1
    Notathing -> 1
    Link _ shape -> groupDepth shape
    Tap _ shape -> groupDepth shape
    TapAt _ shape -> groupDepth shape
    EnterShape _ shape -> groupDepth shape
    EnterAt _ shape -> groupDepth shape
    Exit _ shape -> groupDepth shape
    ExitAt _ shape -> groupDepth shape
    MouseDown _ shape -> groupDepth shape
    MouseDownAt _ shape -> groupDepth shape
    MouseUp _ shape -> groupDepth shape
    MouseUpAt _ shape -> groupDepth shape
    MoveOverAt _ shape -> groupDepth shape
    TouchStart _ shape -> groupDepth shape
    TouchEnd _ shape -> groupDepth shape
    TouchStartAt _ shape -> groupDepth shape
    TouchEndAt _ shape -> groupDepth shape
    TouchMoveAt _ shape -> groupDepth shape
    GraphPaper _ _ _ -> 0

countCollageTransformations : Collage userMsg -> Int
countCollageTransformations (Collage _ _ shapes) =
    List.sum <| List.map countTransformations shapes

countTransformations : Shape userMsg -> Int
countTransformations sh = case sh of
    Inked _ _ _ -> 0
    ForeignObject _ _ _ -> 0
    Move _ shape -> 1 + countTransformations shape
    Rotate _ shape -> 1 + countTransformations shape
    Scale _ _ shape -> 1 + countTransformations shape
    Skew _ _ shape -> 1 + countTransformations shape
    Transformed _ shape -> 1 + countTransformations shape
    Group shapes -> List.sum <| List.map countTransformations shapes
    GroupOutline shape -> countTransformations shape
    AlphaMask shapes0 shapes1 -> countTransformations shapes0 + countTransformations shapes1
    Clip shape0 shape1 -> countTransformations shape0 + countTransformations shape1
    Everything -> 0
    Notathing -> 0
    Link _ shape -> countTransformations shape
    Tap _ shape -> countTransformations shape
    TapAt _ shape -> countTransformations shape
    EnterShape _ shape -> countTransformations shape
    EnterAt _ shape -> countTransformations shape
    Exit _ shape -> countTransformations shape
    ExitAt _ shape -> countTransformations shape
    MouseDown _ shape -> countTransformations shape
    MouseDownAt _ shape -> countTransformations shape
    MouseUp _ shape -> countTransformations shape
    MouseUpAt _ shape -> countTransformations shape
    MoveOverAt _ shape -> countTransformations shape
    TouchStart _ shape -> countTransformations shape
    TouchEnd _ shape -> countTransformations shape
    TouchStartAt _ shape -> countTransformations shape
    TouchEndAt _ shape -> countTransformations shape
    TouchMoveAt _ shape -> countTransformations shape
    GraphPaper _ _ _ -> 0

countCollageInteractiveShapes : Collage userMsg -> Int
countCollageInteractiveShapes (Collage _ _ shapes) =
    List.sum <| List.map countInteractiveShapes shapes

countInteractiveShapes : Shape userMsg -> Int
countInteractiveShapes sh = case sh of
    Inked _ _ _ -> 0
    ForeignObject _ _ _ -> 0
    Move _ shape -> countInteractiveShapes shape
    Rotate _ shape -> countInteractiveShapes shape
    Scale _ _ shape -> countInteractiveShapes shape
    Skew _ _ shape -> countInteractiveShapes shape
    Transformed _ shape -> countInteractiveShapes shape
    Group shapes -> List.sum <| List.map countInteractiveShapes shapes
    GroupOutline shape -> countInteractiveShapes shape
    AlphaMask shapes0 shapes1 -> countInteractiveShapes shapes0 + countInteractiveShapes shapes1
    Clip shape0 shape1 -> countInteractiveShapes shape0 + countInteractiveShapes shape1
    Everything -> 0
    Notathing -> 0
    Link _ shape -> 1 + countInteractiveShapes shape
    Tap _ shape -> 1 + countInteractiveShapes shape
    TapAt _ shape -> 1 + countInteractiveShapes shape
    EnterShape _ shape -> 1 + countInteractiveShapes shape
    EnterAt _ shape -> 1 + countInteractiveShapes shape
    Exit _ shape -> 1 + countInteractiveShapes shape
    ExitAt _ shape -> 1 + countInteractiveShapes shape
    MouseDown _ shape -> 1 + countInteractiveShapes shape
    MouseDownAt _ shape -> 1 + countInteractiveShapes shape
    MouseUp _ shape -> 1 + countInteractiveShapes shape
    MouseUpAt _ shape -> 1 + countInteractiveShapes shape
    MoveOverAt _ shape -> 1 + countInteractiveShapes shape
    TouchStart _ shape -> 1 + countInteractiveShapes shape
    TouchEnd _ shape -> 1 + countInteractiveShapes shape
    TouchStartAt _ shape -> 1 + countInteractiveShapes shape
    TouchEndAt _ shape -> 1 + countInteractiveShapes shape
    TouchMoveAt _ shape -> 1 + countInteractiveShapes shape
    GraphPaper _ _ _ -> 0