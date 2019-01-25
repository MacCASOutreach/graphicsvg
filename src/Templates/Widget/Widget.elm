import Html exposing(..)
import Html.Attributes exposing(..)
import Browser
import GraphicSVG exposing(..)
import GraphicSVG.Widget as Widget
import Html.Events exposing (onClick)


type alias Model =
    { count : Int
    , widgetOneState : Widget.Model Msg
    , widgetTwoState : Widget.Model Msg
    , x : Float
    , y : Float
    }


initialModel : (Model, Cmd Msg)
initialModel =
    let
        (wstate0,wcmd0) = Widget.init 50 50 WidgetOneMsg "widget0"
        (wstate1,wcmd1) = Widget.init 50 15 WidgetTwoMsg "widget1"
    in
    ({ count = 0
     , widgetOneState = wstate0
     , widgetTwoState = wstate1
     , x = 0
     , y = 0
    }
    ,Cmd.batch [wcmd0,wcmd1]
    )


type Msg =
      WidgetOneMsg (Widget.Msg Msg)
    | WidgetTwoMsg (Widget.Msg Msg)
    | MoveTo (Float,Float)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        WidgetOneMsg wMsg ->
            let
                (newWState,wCmd) = Widget.update wMsg model.widgetOneState
            in
            ({ model | widgetOneState = newWState
                        }
            , wCmd)

        WidgetTwoMsg wMsg ->
            let
                (newWState,wCmd) = Widget.update wMsg model.widgetTwoState
            in
            ({ model | widgetTwoState = newWState
                        }
            , wCmd)


        MoveTo (x,y) -> ( {model | x = x, y= y}, Cmd.none)


view : Model -> Html Msg
view model =
    div []
        [ div [style "width" "25%", style "margin-left" "auto", style "margin-right" "auto"]
            [
                Widget.view model.widgetOneState
                [
                    square 50
                        |> filled gray
                        |> notifyMouseMoveAt MoveTo
                ,   circle 1
                        |> filled blue
                        |> move (model.x,model.y)
                        |> notifyMouseMoveAt MoveTo
                ]
            ]
        , div [style "width" "75%", style "margin-left" "auto", style "margin-right" "auto"]
            [
                Widget.view model.widgetTwoState
                [
                    square 50
                        |> filled darkGray
                        |> notifyMouseMoveAt MoveTo
                ,   triangle 1
                        |> filled red
                        |> move (model.x,model.y)
                        |> notifyMouseMoveAt MoveTo
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , view = view
        , update = update
        , subscriptions = \_ ->
            Sub.batch
                [
                    Widget.subscriptions WidgetOneMsg 
                ,   Widget.subscriptions WidgetTwoMsg 
                ]
        }
