module Tests.BoundingBoxTest exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import GraphicSVG.BoundingBox exposing (getBoundingBox)

-- Model includes the currently selected test case
type alias Model =
    { time : Float
    , selectedTest : Int
    }

-- Msg types for button interactions and animation
type Msg
    = Tick Float GetKeyState
    | SelectTest Int

-- Initial model with test case 0 selected
init : Model
init =
    { time = 0
    , selectedTest = 0
    }

-- Update function to handle button clicks and ticks
update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick t _ ->
            { model | time = t }
        SelectTest testId ->
            { model | selectedTest = testId }

-- List of test cases
testCases : List (String, Shape Msg)
testCases =
    [ 
     ("Complex Transformations",
        roundedRect 40 20 5
            |> filled green
            |> rotate (degrees -45)
            |> skewX 0.25
            |> scale 1.2
            |> scaleX 2
            |> move (10, 20)
      )

    , ("Simple Circle", circle 20 |> filled red |> addOutline (solid 1) black)
    , ("Rotated Rect", rect 30 50 |> filled blue |> rotate (degrees 45))
    , ("Stacked Rotations", roundedRect 40 20 5 |> filled green |> rotate (degrees -30) |> rotate (degrees 30) |> move (0, 40))
    , ("Scaled Polygon", polygon [(0, 0), (30, 0), (15, 25)] |> filled yellow |> addOutline (solid 2) purple |> scale 1.5)
    , ("Skewed Oval", oval 25 15 |> filled orange  |> move (-20, 0) |> skewX 0.25)
    , ("Text with Rotation", text "Test" |> size 20 |> alignRight |> filled black |> rotate (degrees 30) |> move (30, 30) )
    , ("Group of Shapes",
        group
            [ circle 20 |> filled red |> move (-30, 10)
            , rect 30 20 |> filled blue |> rotate (degrees 45) |> move (20, -20)
            ]
            |> scale 0.8
      )
    , ("Nested Groups",
        group
            [ group
                [ oval 25 15 |> filled orange |> move (-20, 0)
                , rect 20 20 |> filled pink |> rotate (degrees -60) |> scale 1.2
                ]
                |> scale 0.7
                |> move (40, 0)
            , circle 15 |> filled yellow |> move (0, -30)
            ]
            |> rotate (degrees 15)
      )
    , ("Multiple Rotations",
        circle 20
            |> filled red
            |> rotate (degrees 45)
            |> rotate (degrees -30)
            |> rotate (degrees 15)
            |> move (0, 30)
      )
    , ("Clip",
        rect 40 40
            |> filled blue
            |> clip (circle 15 |> filled black)
            |> move (0, 20)
      )
    , ("Empty Group", group [])
    , ("Large Group with Mixed Transformations",
        group
            [ circle 25 |> filled red |> move (-40, 0) |> rotate (degrees 30)
            , rect 20 40 |> filled blue |> scale 1.3 |> move (20, 20)
            , roundedRect 30 20 5 |> filled green |> skewY 0.2 |> move (-10, -30)
            , polygon [(0, 0), (20, 0), (10, 30)] |> filled yellow |> rotate (degrees -45)
            , text "Mixed" |> size 15 |> filled black |> move (30, -20)
            ]
            |> scale 0.9
            |> rotate (degrees 10)
      )
    , ("Deeply Nested Rotations",
        circle 10
            |> filled red
            |> rotate (degrees 20)
            |> rotate (degrees -40)
            |> rotate (degrees 60)
            |> move (0, 50)
      )
    ]

-- Generate shapes for the collage
myShapes : Model -> List (Shape Msg)
myShapes model =
    let
        -- Get the selected test case
        (testName, selectedShape) =
            List.drop model.selectedTest testCases
                |> List.head
                |> Maybe.withDefault (List.head testCases |> Maybe.withDefault ("None", group []))

        -- Compute bounding box
        boundingBox = getBoundingBox selectedShape

        -- Visualize bounding box as a rectangle
        boundingBoxRect =
            rect (boundingBox.maxX - boundingBox.minX) (boundingBox.maxY - boundingBox.minY)
                |> outlined (dashed 1) black
                |> move ((boundingBox.minX + boundingBox.maxX) / 2, (boundingBox.minY + boundingBox.maxY) / 2)

        -- Display bounding box coordinates
        boundingBoxText =
            text (Debug.toString boundingBox)
                |> filled black
                |> scale 0.25
                |> move (-90, -50)

        -- Create buttons for selecting test cases
        buttons =
            testCases
                |> List.indexedMap
                    (\index (name, _) ->
                        let
                            isSelected = index == model.selectedTest
                            buttonColor = if isSelected then lightBlue else lightGray
                        in
                        group
                            [ rect 80 15
                                |> filled buttonColor
                                |> addOutline (solid 1) black
                            , text name
                                |> size 8
                                |> filled black
                                |> move (-38, -3)
                            ]
                            |> move (-80, 50 - toFloat index * 20)
                            |> notifyTap (SelectTest index)
                    )

        -- Title for the selected test case
        title =
            text ("Test Case: " ++ testName)
                |> size 10
                |> filled black
                |> move (-90, 50)
    in
    [ group [ selectedShape, boundingBoxRect, boundingBoxText, title ]
    , group buttons
    |> scale 0.25
    |> move (-40, 30)
    
    ] 

-- View function
view : Model -> Collage Msg
view model =
    collage 192 128 (myShapes model)

-- Main application
main : GameApp Model Msg
main =
    gameApp Tick
        { model = init
        , view = view
        , update = update
        , title = "Bounding Box Test Suite"
        }