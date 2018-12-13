module Main exposing (main)

import Array
import Bitwise
import Browser.Events exposing (onKeyPress)
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (GameApp, GetKeyState, KeyState(..), Keys(..), gameApp)
import Html exposing (button)
import Html.Attributes exposing (style)
import Json.Decode as D
import Time



-- Test description instruction testobject expectedanswer


type Test
    = Test String String (Model -> Shape Msg) ( Float, Float )


type alias Tests =
    List Test


test0 : Test
test0 =
    Test
        "This test program depends on the functionality of the mouse;"
        "Thus, if there is no response when you click on the following rectangle, you can't proceed:"
        (\_ ->
            rect 40 15
                |> filled grey
                |> addOutline (solid 0.5) black
                |> move ( 200, 202.5 )
                |> notifyTap (Notify ( 0, 0 ))
        )
        ( 0, 0 )


test1 : Test
test1 =
    Test
        "Testing time ticks as a user indicated pass/fail test."
        "You should see number of seconds spent testing in the left box, click it if so, the fail box if not:"
        (\model ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text (String.fromInt model.time)
                        |> size 10
                        |> centered
                        |> outlined (solid 1) green
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                ]
        )
        ( 0, 0 )


test2 : Test
test2 =
    Test
        "Testing user keyboard input (space out errors); click the Submit box any time to give up."
        "Type \"McMaster rocks!\" without the quotes to appear in the left box, then click Submit:"
        (\model ->
            group
                [ group
                    [ rect 90 15 |> filled grey |> addOutline (solid 0.5) black
                    , text model.input
                        |> size 10
                        |> centered
                        |> outlined (solid 0.5) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 150, 197.5 )
                , group
                    [ rect 50 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Submit..."
                        |> size 10
                        |> centered
                        |> outlined (solid 0.5) black
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 197.5 )
                    |> notifyTap
                        (Notify
                            (if model.input == "McMaster rocks!" then
                                ( 0, 0 )

                             else
                                ( 10, 10 )
                            )
                        )
                ]
        )
        ( 0, 0 )



{--
test3 : Test
test3 =
    Test
        "Testing keyboard input of wasd and arrow keys; click the Fail box any time to give up."
        "Move the green dot around the box with the wasd and arrow keys, then click the box to pass:"
        (\_ ->
            group
                [ group
                    [ rect 90 90 |> filled grey |> addOutline (solid 0.5) black
                    , circle 3 |> filled darkGreen |> addOutline (solid 0.5) black
                        |> move model.position
                    ]
                    |> move ( 150, 160 ) |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!" |> size 10 |> centered
                        |> outlined (solid 1) red |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 ) |> notifyTap (Notify ( 10, 10 ))
                ])
        ( 0, 0 )
--}


test4 : Test
test4 =
    Test
        "Testing getting the coordinates of the clicked or tapped position; click Fail to give up."
        "Click the little green dot in the grey box as accurately as possible to pass:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , circle 1
                        |> filled darkGreen
                        |> notifyTapAt Notify
                    ]
                    |> move ( 175, 202.5 )
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                ]
        )
        ( 175, 202.5 )


test5 : Test
test5 =
    Test
        "Testing mouse entering a shape; click Fail to give up."
        "Sweep the mouse slowly across the green dot to pass:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , circle 1
                        |> filled darkGreen
                        |> notifyEnter (Notify ( 0, 0 ))
                    ]
                    |> move ( 175, 202.5 )
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                ]
        )
        ( 0, 0 )


test6 : Test
test6 =
    Test
        "Testing mouse entering a shape with position; click Fail to give up."
        "Sweep the mouse slowly from left to right across the green line to pass:"
        (\_ ->
            let
                mapf msg =
                    case msg of
                        Notify ( x, _ ) ->
                            Notify ( x, 0 )

                        _ ->
                            msg
            in
            group
                [ group
                    [ rect 90 90 |> filled grey |> addOutline (solid 0.5) black
                    , line ( 0, 44 ) ( 0, -44 )
                        |> outlined (solid 1) darkGreen
                        |> notifyEnterAt Notify
                        |> map mapf
                    ]
                    |> move ( 150, 160 )
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                ]
        )
        ( 150, 0 )


test7 : Test
test7 =
    Test
        "Testing mouse entering a shape with position; click Fail to give up."
        "Sweep the mouse slowly from top to bottom across the green line to pass:"
        (\_ ->
            let
                mapf msg =
                    case msg of
                        Notify ( _, y ) ->
                            Notify ( 0, y )

                        _ ->
                            msg
            in
            group
                [ group
                    [ rect 90 90 |> filled grey |> addOutline (solid 0.5) black
                    , line ( 44, 0 ) ( -44, 0 )
                        |> outlined (solid 1) darkGreen
                        |> notifyEnterAt Notify
                        |> map mapf
                    ]
                    |> move ( 150, 160 )
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                ]
        )
        ( 0, 160 )


test8 : Test
test8 =
    Test
        "Testing mouse leaving a shape; click Fail to give up."
        "Sweep the mouse slowly across the green dot to pass:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , circle 1
                        |> filled darkGreen
                        |> notifyLeave (Notify ( 0, 0 ))
                    ]
                    |> move ( 175, 202.5 )
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                ]
        )
        ( 0, 0 )


test9 : Test
test9 =
    Test
        "Testing mouse leaving a shape with position; click Fail to give up."
        "Sweep the mouse slowly left to right across the green line to pass:"
        (\_ ->
            let
                mapf msg =
                    case msg of
                        Notify ( x, _ ) ->
                            Notify ( x, 0 )

                        _ ->
                            msg
            in
            group
                [ group
                    [ rect 90 90 |> filled grey |> addOutline (solid 0.5) black
                    , line ( 0, 44 ) ( 0, -44 )
                        |> outlined (solid 1) darkGreen
                        |> notifyEnterAt Notify
                        |> map mapf
                    ]
                    |> move ( 150, 160 )
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                ]
        )
        ( 151, 0 )


test10 : Test
test10 =
    Test
        "Testing mouse leaving a shape with position; click Fail to give up."
        "Sweep the mouse slowly top to bottom across the green line to pass:"
        (\_ ->
            let
                mapf msg =
                    case msg of
                        Notify ( _, y ) ->
                            Notify ( 0, y )

                        _ ->
                            msg
            in
            group
                [ group
                    [ rect 90 90 |> filled grey |> addOutline (solid 0.5) black
                    , line ( -44, 0 ) ( 44, 0 )
                        |> outlined (solid 1) darkGreen
                        |> notifyEnterAt Notify
                        |> map mapf
                    ]
                    |> move ( 150, 160 )
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                ]
        )
        ( 0, 161 )


test11 : Test
test11 =
    Test
        "Testing getting the positon of the mouse moved over a shape; click Fail to give up."
        "Move the mouse over the green dot in the grey box as accurately as possible to pass:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , circle 1
                        |> filled darkGreen
                        |> notifyMouseMoveAt Notify
                    ]
                    |> move ( 175, 202.5 )
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                ]
        )
        ( 175, 202.5 )


test12 : Test
test12 =
    Test
        "Testing holding the mouse button down over a shape; click Fail to give up."
        "Hold the mouse button down over the green dot to pass:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , circle 1
                        |> filled darkGreen
                        |> notifyMouseDown (Notify ( 0, 0 ))
                    ]
                    |> move ( 175, 202.5 )
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                ]
        )
        ( 0, 0 )


test13 : Test
test13 =
    Test
        "Testing holding the mouse button down over a shape with position; click Fail to give up."
        "Hold the mouse button down over the green dot to pass:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , circle 1
                        |> filled darkGreen
                        |> notifyMouseDownAt Notify
                    ]
                    |> move ( 175, 202.5 )
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                ]
        )
        ( 175, 202.5 )


test14 : Test
test14 =
    Test
        "Testing releasing the mouse button over a shape; click Fail to give up."
        "Release the mouse button over the green dot to pass:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , circle 1
                        |> filled darkGreen
                        |> notifyMouseUp (Notify ( 0, 0 ))
                    ]
                    |> move ( 175, 202.5 )
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                ]
        )
        ( 0, 0 )


test15 : Test
test15 =
    Test
        "Testing releasing the mouse button over a shape with position; click Fail to give up."
        "Release the mouse button over the green dot to pass:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , circle 1
                        |> filled darkGreen
                        |> notifyMouseUpAt Notify
                    ]
                    |> move ( 175, 202.5 )
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                ]
        )
        ( 175, 202.5 )


test16 : Test
test16 =
    Test
        "Testing touching a shape; click Fail to give up."
        "Touch the green dot at the center of the grey square to pass:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , circle 1 |> filled darkGreen
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTouchStart (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                ]
        )
        ( 0, 0 )


test17 : Test
test17 =
    Test
        "Testing touching a shape with position; click Fail to give up."
        "Touch the green dot at the center of the grey square to pass:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , circle 1 |> filled darkGreen
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTouchStartAt Notify
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                ]
        )
        ( 175, 202.5 )


test18 : Test
test18 =
    Test
        "Testing touching and lifting from a shape; click Fail to give up."
        "Touch then lift from the green dot at the center of the grey square to pass:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , circle 1 |> filled darkGreen
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTouchEnd (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                ]
        )
        ( 0, 0 )


test19 : Test
test19 =
    Test
        "Testing touching and lifting from a shape with position; click Fail to give up."
        "Touch then lift from the green dot at the center of the grey square to pass:"
        (\_ ->
            let
                mapf msg =
                    case msg of
                        Notify ( x, _ ) ->
                            Notify ( x, 0 )

                        _ ->
                            msg
            in
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , circle 1 |> filled darkGreen
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTouchEndAt Notify
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                ]
        )
        ( 175, 202.5 )


test20 : Test
test20 =
    Test
        "Testing touch moving over a shape with position; click Fail to give up."
        "Touch and move your finger over the dot at the center of the grey square to pass:"
        (\_ ->
            let
                mapf msg =
                    case msg of
                        Notify ( _, _ ) ->
                            Notify ( 0, 0 )

                        _ ->
                            msg
            in
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , circle 3 |> filled darkGreen
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTouchMoveAt Notify
                    |> map mapf
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                ]
        )
        ( 0, 0 )


test21 : Test
test21 =
    Test
        "Testing open and closed polygons; click Pass or Fail as appropriate."
        "Do you see a \"C\" shaped open polygon on the left, closed polygon on the right below?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , openPolygon [ ( 100, 100 ), ( -100, 100 ), ( -100, -100 ), ( 100, -100 ) ]
                    |> outlined (solid 1) black
                    |> move ( -125, 0 )
                , polygon [ ( 100, 100 ), ( -100, 100 ), ( -100, -100 ), ( 100, -100 ), ( 0, 0 ) ]
                    |> outlined (solid 1) black
                    |> move ( 125, 0 )
                ]
        )
        ( 0, 0 )


test22 : Test
test22 =
    Test
        "Testing equilateral, right, isoseles and SAS triangles; click Pass or Fail as appropriate."
        "Do you see the above triangles clockwise from top left below?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , triangle 50
                    |> outlined (solid 1) black
                    |> move ( -100, 50 )
                , rightTriangle 100 100 |> outlined (solid 1) black |> move ( 75, 0 )
                , isosceles 50 100 |> outlined (solid 1) black |> move ( 125, -125 )
                , sideAngleSide 100 (degrees 60) 75
                    |> outlined (solid 1) black
                    |> move ( -150, -125 )
                ]
        )
        ( 0, 0 )


test23 : Test
test23 =
    Test
        "Testing roundedRect, circle, oval and wedge; click Pass or Fail as appropriate."
        "Do you see the above shapeds in red, orange, blue and purple clockwise from top left below?:"
        (\model ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , let
                    r =
                        modBy 3 model.time |> toFloat
                  in
                  roundedRect 100 100 (50 * r / 2)
                    |> filled red
                    |> addOutline (solid 0.5) black
                    |> move ( -150, 0 )
                , circle 50
                    |> filled orange
                    |> addOutline (solid 0.5) black
                    |> move ( 150, 0 )
                , let
                    r =
                        -1 + modBy 3 model.time |> toFloat
                  in
                  oval (100 + 50 * r) 100
                    |> filled blue
                    |> addOutline (solid 0.5) black
                    |> move ( 150, -150 )
                , let
                    r =
                        modBy 5 model.time |> toFloat
                  in
                  wedge 50 (1 * r / 4)
                    |> filled purple
                    |> addOutline (solid 1) black
                    |> move ( -150, -150 )
                ]
        )
        ( 0, 0 )


test24 : Test
test24 =
    Test
        "Testing Bezier curves and curve helper; click Pass or Fail as appropriate."
        "Do you see a purple rounded rectangle (different than the pink true one) below with Pull points?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , group
                    [ roundedRect 200 200 100
                        |> outlined (solid 1) pink
                    , curve ( -100, 0 )
                        [ Pull ( -100, -100 ) ( 0, -100 )
                        , Pull ( 100, -100 ) ( 100, 0 )
                        , Pull ( 100, 100 ) ( 0, 100 )
                        , Pull ( -100, 100 ) ( -100, 0 )
                        ]
                        |> outlined (solid 1) darkPurple
                        |> curveHelper
                    ]
                ]
        )
        ( 0, 0 )


test25 : Test
test25 =
    Test
        "Testing Line types and variations; click Pass or Fail as appropriate."
        "Do you see solid, dashed, dotted, long dashed, dotdashed, and varying, lines left to right below?:"
        (\model ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , group
                    [ line ( -100, 100 ) ( -100, -100 )
                        |> outlined (solid 1) black
                    , line ( -60, 100 ) ( -60, -100 )
                        |> outlined (dotted 1) black
                    , line ( -20, 100 ) ( -20, -100 )
                        |> outlined (dashed 1) black
                    , line ( 20, 100 ) ( 20, -100 )
                        |> outlined (longdash 1) black
                    , line ( 60, 100 ) ( 60, -100 )
                        |> outlined (dotdash 1) black
                    , let
                        r =
                            -1 + modBy 3 model.time |> toFloat
                      in
                      line ( 100, 100 ) ( 100, -100 )
                        |> outlined (custom [ ( 10, 5 + 5 * r / 2 ) ] 1) black
                    ]
                ]
        )
        ( 0, 0 )


test26 : Test
test26 =
    Test
        "Testing some Text attributes not yet tested; click Pass or Fail as appropriate."
        "Do you see \"Abcdef\" in italic font with a strikethrough below?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , text "Abcdef"
                    |> size 50
                    |> centered
                    |> strikethrough
                    |> italic
                    |> outlined (solid 1) black
                ]
        )
        ( 0, 0 )


test27 : Test
test27 =
    Test
        "Testing selectable Text; click Pass or Fail as appropriate."
        "Select the below text (drag across it), copy to clipboard, and paste into address of browser?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , text "http://www.cas.mcmaster.ca/~anand/"
                    |> size 20
                    |> centered
                    |> selectable
                    |> outlined (solid 1) black
                ]
        )
        ( 0, 0 )


test28 : Test
test28 =
    Test
        "Testing hyperlinks added to a Shape, a Text in this case; click Pass or Fail as appropriate."
        "When the below text is double clicked, does your default browser open to the indicated web page?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , text "http://outreach.mcmaster.ca"
                    |> size 20
                    |> centered
                    |> outlined (solid 1) black
                    |> addHyperlink "http://outreach.mcmaster.ca"
                ]
        )
        ( 0, 0 )


test29 : Test
test29 =
    Test
        "Testing rotation and scaling; click Pass or Fail as appropriate."
        "Does the below wedge rotate counter-clockwise in eight steps and change size every step?:"
        (\model ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , let
                    sf =
                        1 + modBy 2 model.time |> toFloat
                  in
                  let
                    r =
                        modBy 8 model.time |> toFloat
                  in
                  wedge 50 0.125
                    |> outlined (solid 1) black
                    |> scale sf
                    |> rotate (turns (r / 8))
                ]
        )
        ( 0, 0 )


test30 : Test
test30 =
    Test
        "Testing changing transparency of shapes; click Pass or Fail as appropriate."
        "Does the darkgreen circle over the purple square disappear over 5 steps?:"
        (\model ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , square 200 |> filled lightPurple
                , let
                    r =
                        4 - modBy 5 model.time |> toFloat
                  in
                  circle 100
                    |> filled green
                    |> addOutline (solid 1) black
                    |> makeTransparent (r / 4)
                ]
        )
        ( 0, 0 )


test31 : Test
test31 =
    Test
        "Testing graphpaper, with fail of zero spacing; click Pass or Fail as appropriate."
        "Does the background graph paper changes size over 3 steps, disappearing on the 4th step?:"
        (\model ->
            group
                [ let
                    r =
                        modBy 4 model.time |> toFloat
                  in
                  graphPaperCustom (10 * r) 1 lightPurple
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , circle 10 |> outlined (solid 1) black
                ]
        )
        ( 0, 0 )


test32 : Test
test32 =
    Test
        "Testing inserting Html in the page; click on the Html button or Fail as appropriate."
        "Testing response of Html button:"
        (\_ ->
            group
                [ html 40
                    15
                    (button
                        [ style "border" "1px solid Black"
                        , style "width" "45px"
                        , style "height" "15px"
                        , style "font-size" "x-small"
                        , style "color" "DarkGreen"
                        ]
                        [ Html.text "Pass!!!" ]
                    )
                    |> move ( 155, 212.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                ]
        )
        ( 0, 0 )


test33 : Test
test33 =
    Test
        "Testing repaint; click Pass or Fail as appropriate."
        "Does the circle colour change blue to purple and back every second?:"
        (\model ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , let
                    r =
                        modBy 2 model.time |> toFloat
                  in
                  circle 50
                    |> outlined (solid 1) black
                    |> repaint
                        (if r == 0 then
                            blue

                         else
                            purple
                        )
                ]
        )
        ( 0, 0 )


test34 : Test
test34 =
    Test
        "Testing clip; click Pass or Fail as appropriate."
        "Is the blue circle clipped to only its right half?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , clip
                    (rect 100 100 |> filled purple |> move ( 50, 0 ))
                    (circle 50 |> filled blue)
                ]
        )
        ( 0, 0 )


test35 : Test
test35 =
    Test
        "Testing union; click Pass or Fail as appropriate."
        "Is the shape a blue circle jointed on the right side by a purple (underlying) circle?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , circle 50
                    |> filled blue
                    |> union (circle 50 |> filled purple |> move ( 50, 0 ))
                ]
        )
        ( 0, 0 )


test36 : Test
test36 =
    Test
        "Testing subtraction; click Pass or Fail as appropriate."
        "Is the shape a blue circle with a circle \"bite\" taken out of it on the left side?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , subtract
                    (circle 50 |> filled purple |> move ( -50, 0 ))
                    (circle 50 |> filled blue)
                ]
        )
        ( 0, 0 )


test37 : Test
test37 =
    Test
        "Testing outside; click Pass or Fail as appropriate."
        "Does the blue circle disappear every second, reappear the next?:"
        (\model ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , let
                    r =
                        modBy 2 model.time
                  in
                  circle 50
                    |> filled blue
                    |> addOutline (solid 2) red
                    |> (if r == 0 then
                            outside

                        else
                            move ( 0, 0 )
                       )
                ]
        )
        ( 0, 0 )


test38 : Test
test38 =
    Test
        "Testing ghost; click Pass or Fail as appropriate."
        "Can you see the pink circle through the hole inside the red outline?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , circle 40 |> filled pink
                , circle 50 |> ghost |> addOutline (solid 1) red
                ]
        )
        ( 0, 0 )


test39 : Test
test39 =
    Test
        "Testing adding outlines to groups; click Pass or Fail as appropriate."
        "Can you see a purple outline around the group of joined and separate shapes?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , group
                    [ roundedRect 100 100 30 |> filled green
                    , wedge 50 0.5 |> filled red |> rotate (turns 0.25) |> move ( 0, 50 )
                    , isosceles 50 100 |> filled blue |> rotate (turns 0.5) |> move ( 0, -50 )
                    , circle 50 |> filled orange |> move ( -150, 0 )
                    ]
                    |> addOutline (solid 5) darkPurple
                ]
        )
        ( 0, 0 )


test40 : Test
test40 =
    Test
        "Testing adding outlines to clips; click Pass or Fail as appropriate."
        "Can you see a purple outline around the orange circle clipped to the right side (interior lines)?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , clip
                    (rect 100 100 |> filled red |> move ( 50, 0 ))
                    (circle 50 |> filled orange)
                        |> addOutline (solid 5) purple
                ]
        )
        ( 0, 0 )


test41 : Test
test41 =
    Test
        "Testing adding outlines to subtracts; click Pass or Fail as appropriate."
        "Can you see a purple outline around the orange circle with a bite out on the right (interior lines)?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , subtract
                    (circle 50 |> filled red |> move ( -50, 0 ))
                    (circle 50 |> filled orange)
                        |> addOutline (solid 5) darkPurple
                ]
        )
        ( 0, 0 )


test42 : Test
test42 =
    Test
        "Testing adding a subtract (left) and a clip (right) to a group; click Pass or Fail as appropriate."
        "Notice that the occluding orange half circles left and right aren't outlined:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , group
                    [ roundedRect 100 100 30 |> filled green
                    , wedge 50 0.5 |> filled red |> rotate (turns 0.25) |> move ( 0, 50 )
                    , isosceles 50 100 |> filled blue |> rotate (turns 0.5) |> move ( 0, -50 )
                    , circle 50 |> filled orange |> move ( -150, 0 )
                    , circle 25 |> filled blue |> move ( -150, 0 )
                    , subtract
                        (rect 100 100 |> filled red |> move ( 50, 0 ))
                        (circle 50 |> filled orange)
                            |> move ( -25, 0 )
                    , clip
                        (rect 100 100 |> filled red |> move ( 50, 0 ))
                        (circle 50 |> filled orange)
                            |> move ( 25, 0 )
                    ]
                    |> addOutline (solid 5) darkPurple
                ]
        )
        ( 0, 0 )


test43 : Test
test43 =
    Test
        "Testing adding outlines to clip groups using groups; click Pass or Fail as appropriate."
        "Can you see there are no outlines because both the shape and pattern are groups?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , addOutline (solid 5) purple <|
                    clip
                        (group
                            [ rect 200 100 |> filled red |> move ( 0, 50 )
                            , clip
                                (rect 50 50 |> filled green |> move ( -25, -60 ))
                                (circle 25 |> filled red |> move ( -25, -35 ))
                            , subtract
                                (rect 50 50 |> filled green |> move ( 25, -10 ))
                                (circle 25 |> filled red |> move ( 25, -35 ))
                            ])
                        (group
                            [ rect 150 110 |> filled red
                            , circle 50 |> filled orange
                            , clip
                                (rect 100 100 |> filled green |> move ( -75, 0 ))
                                (circle 50 |> filled yellow |> move ( -25, 0 ))
                            , subtract
                                (rect 100 100 |> filled green |> move ( -25, 0 ))
                                (circle 50 |> filled yellow |> move ( 25, 0 ))
                            ])
                ]
        )
        ( 0, 0 )


test44 : Test
test44 =
    Test
        "Testing adding outlines to subtract groups as groups; click Pass or Fail as appropriate."
        "Can you see that only part of the subtract pattern is outlinee due to grouping losing exterior lines?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Pass!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , addOutline (solid 5) purple <|
                    subtract
                        (group
                            [ rect 200 100 |> filled red |> move ( 0, 50 )
                            , rect 100 10 |> filled red |> move ( 0, -15 )
                            , clip
                                (rect 50 50 |> filled green |> move ( -25, -60 ))
                                (circle 25 |> filled red |> move ( -25, -35 ))
                            , subtract
                                (rect 50 50 |> filled green |> move ( 25, -10 ))
                                (circle 25 |> filled red |> move ( 25, -35 ))
                            ])
                        (group
                            [ rect 150 110 |> filled red
                            , circle 50 |> filled orange
                            , clip
                                (rect 100 100 |> filled green |> move ( -75, 0 ))
                                (circle 50 |> filled yellow |> move ( -25, 0 ))
                            , subtract
                                (rect 100 100 |> filled green |> move ( -25, 0 ))
                                (circle 50 |> filled yellow |> move ( 25, 0 ))
                            ])
                ]
        )
        ( 0, 0 )


test45 : Test
test45 =
    Test
        "Testing Bezier curves and curve helper after notification; click curve to Pass or Fail as appropriate."
        "Do you see a purple rounded rectangle (different than the pink true one) below with Pull points?:"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , group
                    [ curve ( -100, 0 )
                        [ Pull ( -100, -100 ) ( 0, -100 )
                        , Pull ( 100, -100 ) ( 100, 0 )
                        , Pull ( 100, 100 ) ( 0, 100 )
                        , Pull ( -100, 100 ) ( -100, 0 )
                        ]
                        |> outlined (solid 1) darkPurple
                        |> notifyTap (Notify ( 0, 0 ))
                        |> curveHelper
                    , roundedRect 200 200 100
                        |> outlined (solid 1) pink
                    ]
                ]
        )
        ( 0, 0 )


test46 : Test
test46 =
    Test
        "Testing repaint after notification; click the circle to Pass or Fail as appropriate."
        "Does the circle colour change blue to purple and back every second?:"
        (\model ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , let
                    r =
                        modBy 2 model.time |> toFloat
                  in
                  circle 50
                    |> outlined (solid 1) black
                    |> notifyTap (Notify ( 0, 0 ))
                    |> repaint
                        (if r == 0 then
                            blue

                         else
                            purple
                        )
                ]
        )
        ( 0, 0 )


test47 : Test
test47 =
    Test
        "Tests adding outlines to shapes after notification; click Pass or Fail as appropriate."
        "Do you see black outlines on the pass/fail buttons?"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey
                    , text "Pass!!!"
                        |> size 10
                        |> bold
                        |> centered
                        |> filled darkGreen
                        |> move ( 0, -3 )
                    ]
                    |> move ( 175, 202.5 )
                    |> notifyTap (Notify ( 0, 0 ))
                    |> addOutline (solid 1) black
                , group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                ]
        )
        ( 0, 0 )


test48 : Test
test48 =
    Test
        "Tests ability to \"click-through\" transparent circle; click Fail as appropriate."
        "Can you make the test pass by clicking on the orange circle revealed below?"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , circle 30 |> filled orange |> notifyTap (Notify ( 0, 0 ))
                , circle 50|> outlined (solid 1) black
                ]
        )
        ( 0, 0 )


test49 : Test
test49 =
    Test
        "Tests ability to \"click-through\" transparent rectangle; click Fail as appropriate."
        "Can you make the test pass by clicking on the orange circle revealed below?"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , circle 30 |> filled orange |> notifyTap (Notify ( 0, 0 ))
                , rect 100 80 |> outlined (solid 1) black
                ]
        )
        ( 0, 0 )


test50 : Test
test50 =
    Test
        "Tests ability to \"click-through\" transparent rounded rectangle; click Fail as appropriate."
        "Can you make the test pass by clicking on the orange circle revealed below?"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , circle 30 |> filled orange |> notifyTap (Notify ( 0, 0 ))
                , roundedRect 100 80 30 |> outlined (solid 1) black
                ]
        )
        ( 0, 0 )


test51 : Test
test51 =
    Test
        "Tests ability to \"click-through\" transparent oval; click Fail as appropriate."
        "Can you make the test pass by clicking on the orange circle revealed below?"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , circle 30 |> filled orange |> notifyTap (Notify ( 0, 0 ))
                , oval 100 80 |> outlined (solid 1) black
                ]
        )
        ( 0, 0 )


test52 : Test
test52 =
    Test
        "Tests ability to \"click-through\" transparent triangular polygon; click Fail as appropriate."
        "Can you make the test pass by clicking on the orange circle revealed below?"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , circle 30 |> filled orange |> notifyTap (Notify ( 0, 0 ))
                , triangle 80 |> outlined (solid 1) black
                ]
        )
        ( 0, 0 )


test53 : Test
test53 =
    Test
        "Tests ability to \"click-through\" transparent open polyline; click Fail as appropriate."
        "Can you make the test pass by clicking on the orange circle revealed below?"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , circle 30 |> filled orange |> notifyTap (Notify ( 0, 0 ))
                , openPolygon [ ( 50, 40 ), ( -50, 40 ), ( -50, -40 ), ( 50, -40 ) ]
                    |> outlined (solid 1) black
                ]
        )
        ( 0, 0 )


test54 : Test
test54 =
    Test
        "Tests ability to \"click-through\" transparent Bezier curve; click Fail as appropriate."
        "Can you make the test pass by clicking on the orange circle revealed below?"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , circle 30 |> filled orange |> notifyTap (Notify ( 0, 0 ))
                , curve ( 50, 0 )
                    [ Pull ( 50, 40 ) ( 0, 40 )
                    , Pull ( -50, 40 ) ( -50, 0 )
                    , Pull ( -50, -40 ) ( 0, -40 )
                    , Pull ( 50, -40 ) ( 50, 0 )
                    ] |> outlined (solid 1) black
                ]
        )
        ( 0, 0 )


test55 : Test
test55 =
    Test
        "Tests ability to \"click-through\" transparent text; click Fail as appropriate."
        "Can you make the test pass by clicking on the orange circle revealed below?"
        (\_ ->
            group
                [ group
                    [ rect 40 15 |> filled grey |> addOutline (solid 0.5) black
                    , text "Failed!!!"
                        |> size 10
                        |> centered
                        |> outlined (solid 1) red
                        |> move ( 0, -3 )
                    ]
                    |> move ( 225, 202.5 )
                    |> notifyTap (Notify ( 10, 10 ))
                , circle 30 |> filled orange |> move ( 0, 30 ) |> notifyTap (Notify ( 0, 0 ))
                , text "Boo!!!" |> size 100 |> centered |> outlined (solid 1) black
                ]
        )
        ( 0, 0 )


testfinished : Test
testfinished =
    Test
        "Completed all tests; nothing more to do but enjoy the puppetShow."
        "Take a note of the passed and failed tests for debugging; refresh the page to restart."
        (\model ->
            group
                (let
                    vec =
                        200

                    v0 =
                        vec * cos (turns <| toFloat model.tick / 128)

                    v1 =
                        vec * cos (turns <| 0.25 + toFloat model.tick / 128)

                    v2 =
                        vec * cos (turns <| 0.5 + toFloat model.tick / 128)
                 in
                 puppetShow 500
                    500
                    [ ( 200 + v0
                      , circle 50
                            |> filled red
                            |> addOutline (solid 0.5) black
                            |> move (fromPolar ( v0, degrees 45 ))
                      )
                    , ( 200 + v1
                      , square 100
                            |> filled green
                            |> addOutline (solid 0.5) black
                            |> move (fromPolar ( v1, degrees -45 ))
                      )
                    , ( 200 + v2
                      , triangle 50
                            |> filled blue
                            |> addOutline (solid 0.5) black
                            |> move (fromPolar ( v2, degrees 135 ))
                      )
                    ]
                )
        )
        ( 0, 0 )


tests : Tests
tests =
    [ test0
    , test1
    , test2

    --    , test3
    , test4
    , test5
    , test6
    , test7
    , test8
    , test9
    , test10
    , test11
    , test12
    , test13
    , test14
    , test15
    , test16
    , test17
    , test18
    , test19
    , test20
    , test21
    , test22
    , test23
    , test24
    , test25
    , test26
    , test27
    , test28
    , test29
    , test30
    , test31
    , test32
    , test33
    , test34
    , test35
    , test36
    , test37
    , test38
    , test39
    , test40
    , test41
    , test42
    , test43
    , test44
    , test45
    , test46
    , test47
    , test48
    , test49
    , test50
    , test51
    , test52
    , test53
    , test54
    , test55

    --    , testn etc.
    ]


type alias Model =
    { tick : Int
    , timeold : Int
    , time : Int
    , input : String
    , tests : Tests
    , testnum : Int
    , numpassed : Int
    }


initModel : Tests -> Model
initModel tsts =
    { tick = 0
    , timeold = 0
    , time = -1
    , input = ""
    , tests = tsts
    , testnum = 0
    , numpassed = 0
    }


type Msg
    = Tick Time.Posix
    | KeyClick Int
    | Notify ( Float, Float )
    | NoOp


keyset : List Char
keyset =
    "mcaster ok1" |> String.toList


charTable : Array.Array Char
charTable =
    Array.fromList <|
        String.toList
            " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick posixtime ->
            let
                secs =
                    Time.posixToMillis posixtime // 1000
            in
            if secs - model.timeold < 1 then
                { model | tick = Bitwise.and (model.tick + 1) 0x7F }

            else
                { model | timeold = secs, time = model.time + 1 }

        KeyClick keycode ->
            case Array.get (keycode - 32) charTable of
                Nothing ->
                    model

                Just chr ->
                    { model
                        | input =
                            String.right 15 <|
                                model.input
                                    ++ String.fromChar chr
                    }

        {--
        Tick tick ( keyf, wasd, arrows ) ->
            let testkey kyst =
                    case kyst of
                        [] -> ""
                        (frst :: restkeys) ->
                            let fstr = String.fromChar frst in
                            if keyf (Key fstr) == JustDown then (
                                if keyf Shift == Down then (
                                    if fstr == "1" then "!"
                                    else String.toUpper fstr )
                                else fstr )
                            else testkey restkeys
            in
            { model
            | time = tick |> floor
            , input = String.right 15 <| model.input ++ testkey keyset
            , position =
                let ( ox, oy ) = model.position
                    ( xw, yw ) = wasd
                    ( xa, ya ) = arrows
                in ( ox + xw + xa, oy + yw + ya )
            }
--}
        Notify ( x, y ) ->
            case model.tests of
                [] ->
                    model

                (Test _ _ _ expectedanswer) :: resttests ->
                    let
                        ( tx, ty ) =
                            expectedanswer

                        ( xerr, yerr ) =
                            ( abs <| x - tx, abs <| y - ty )
                    in
                    { model
                        | tests = resttests
                        , testnum = model.testnum + 1
                        , numpassed =
                            model.numpassed
                                + (if xerr <= 5 && yerr <= 5 then
                                    1

                                   else
                                    0
                                  )
                    }

        _ ->
            model


view : Model -> GraphicSVG Msg
view model =
    let
        (Test description instruction testsubject _) =
            case model.tests of
                [] ->
                    testfinished

                currtest :: resttests ->
                    currtest
    in
    collage 500
        500
        [ text "GraphicSVG Test Program"
            |> size 20
            |> underline
            |> bold
            |> centered
            |> filled blue
            |> addOutline (solid 0.5) black
            |> move ( 0, 230 )
        , text ("Tests passed:  " ++ String.fromInt model.numpassed)
            |> size 15
            |> bold
            |> centered
            |> filled green
            |> addOutline (solid 0.5) black
            |> move ( -200, 230 )
        , text ("Tests failed:  " ++ String.fromInt (model.testnum - model.numpassed))
            |> size 15
            |> bold
            |> centered
            |> filled red
            |> addOutline (solid 0.5) black
            |> move ( 200, 230 )
        , text
            ("Test number "
                ++ String.fromInt model.testnum
                ++ ":  "
                ++ description
            )
            |> size 10
            |> outlined (solid 0.5) black
            |> move ( -250, 210 )
        , text instruction
            |> size 10
            |> outlined (solid 0.5) black
            |> move ( -250, 195 )
        , testsubject model
        ]


{--}
main : App () Model Msg
main =
    app
        { init = \_ _ _ -> ( initModel tests, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view =
            \model ->
                { title = "Interactive GraphicSVG Tests"
                , body = view model
                }
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onKeyPress (D.map KeyClick (D.field "keyCode" D.int))
                    , Time.every (1000 / 30) Tick
                    ]
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }
--}



{--
main : GameApp Model Msg
main =
    gameApp Tick
        { model = initModel tests
        , update = update
        , view = view
        }
--}
