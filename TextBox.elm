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
import String
import Char exposing (isDigit, isUpper, isLower)


type Msg
    = Tick Float GetKeyState
    | Username String
    | Password String



main : CmdProgram Model Msg
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t _ ->
            ( { model | time = t }, Cmd.none )

        Username user ->
            ( { model | user = String.filter (\char -> isUpper char || isLower char || isDigit char) user }, Cmd.none )

        Password pass ->
            ( { model | password = pass }, Cmd.none )


type alias Model = { password : String, time : Float, user : String }

--List.map (\state -> )

textBox : String -> Float -> Float -> String -> (String -> Msg) -> Shape Msg
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
                    , ( "margin-top", "1px" )
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
                    , ( "margin-top", "1px" )
                    , ( "margin-bottom", "1px" )
                    ]
                ]
                []

view : Model -> Collage Msg
view model =
    collage
        500
        --winX
        500
        --winY
        [ textBox model.user 100 30 "Username" Username
        , passwordBox model.password 100 30 "Password" Password |> move ( 0, -50 ) |> rotate (model.time)
        , group [(text ("You entered Username: " ++ model.user ++ " and Password " ++ model.password) |> centered |> filled black) 
           ><% (circle 10 |> ghost |> move (100 * sin model.time, 0))] |> move ( 0, 50 )
        , group [ roundedRect 80 30 5 |> filled lightGrey, text "Log In" |> centered |> filled black |> move ( 0, -3 ) ] |> move ( 0, -100 )
        ]
