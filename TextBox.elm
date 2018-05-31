module Main exposing (..)

import GraphicSVG exposing (..)
import Random
import Array exposing (Array)
import List
import Set exposing (Set)
import Dict exposing (Dict)
import Debug exposing (log)
import Html as H exposing (Html, node)
import Html.Attributes exposing (placeholder, value, style, type_)
import Json.Encode
import Debug exposing (log)
import Window
import Tuple exposing (first, second)
import Task
import Html exposing (input)
import Html.Events exposing (onInput)


type Msg
    = Tick Float GetKeyState
    | Username String
    | Password String


main =
    cmdApp Tick
        { init =
            ( { time = 0, user = "", password = "" }
            , Cmd.none
            )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )

        Username user ->
            ( { model | user = user }, Cmd.none )

        Password pass ->
            ( { model | password = pass }, Cmd.none )



--List.map (\state -> )


textBox txt w h place msg =
    move ( -w / 2, h / 2 ) <|
        html (w * 1.5) (h * 1.5) <|
            input
                [ placeholder place
                , onInput msg
                , value txt
                , style
                    [ ( "width", toString w ++ "px" )
                    , ( "height", toString h ++ "px" )

                    --, ( "padding", "0" )
                    , ( "margin-top", "1%" )
                    ]
                ]
                []


passwordBox txt w h place msg =
    move ( -w / 2, h / 2 ) <|
        html (w * 1.5) (h * 1.5) <|
            input
                [ placeholder place
                , onInput msg
                , value txt
                , type_ "password"
                , style
                    [ ( "width", toString w ++ "px" )
                    , ( "height", toString h ++ "px" )

                    --, ( "padding", "0" )
                    , ( "margin-top", "1%" )
                    , ( "margin-bottom", "1%" )
                    ]
                ]
                []


view model =
    collage
        500
        --winX
        500
        --winY
        [ textBox model.user 100 30 "Username" Username
        , passwordBox model.password 100 30 "Password" Password |> move ( 0, -50 ) |> rotate (model.time)
        , text ("You entered Username: " ++ model.user ++ " and Password " ++ model.password) |> centered |> filled black |> move ( 0, 50 )
        , group [ roundedRect 80 30 5 |> filled lightGrey, text "Log In" |> centered |> filled black |> move ( 0, -3 ) ] |> move ( 0, -100 )
        ]
