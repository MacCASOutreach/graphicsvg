import Html exposing(Html,div)
import Html.Attributes exposing (style)
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
        (wstate0,wcmd0) = Widget.init 50 25 WidgetOneMsg "widget0"
        (wstate1,wcmd1) = Widget.init 90 25 WidgetTwoMsg "widget1"
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
    | NoOp


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

        NoOp -> (model, Cmd.none) --NoOp should not modify your state


view : Model -> Html Msg
view model =
    div []
        [ div [style "width" "49.75%", {-style "margin-left" "auto", style "margin-right" "auto",-}style "float" "left"]
            [
                Widget.view model.widgetOneState
                [
                    rect 50 25
                        |> filled gray
                        |> notifyMouseMoveAt MoveTo
                ,   circle 1
                        |> filled blue
                        |> move (model.x,model.y)
                        |> notifyMouseMoveAt MoveTo
                ,   text "Widget #1 (50x25)"
                        |> fixedwidth
                        |> size 2
                        |> filled black
                        |> move (-25,11)
                ]
            ]
        , div [style "width" "49.75%",style "float" "right"]
            [
                Widget.icon NoOp "Static" 50 25
                [
                    rect 50 25
                        |> filled gray
                ,   square 2
                        |> filled green
                        |> move (model.x,model.y)
                ,   text "Icon (50x25) [Not Interactive]"
                        |> fixedwidth
                        |> size 2
                        |> filled black
                        |> move (-25,11)
                ]
            ]
        , div [style "width" "75%", style "margin-left" "auto", style "margin-right" "auto"]
            [
                Widget.view model.widgetTwoState
                [
                    rect 90 25
                        |> filled darkGray
                        |> notifyMouseMoveAt MoveTo
                ,   rect 50 25
                        |> filled black
                        |> makeTransparent 0.3
                        |> notifyMouseMoveAt MoveTo
                ,   triangle 1
                        |> filled red
                        |> move (model.x,model.y)
                        |> notifyMouseMoveAt MoveTo
                ,   text "Widget #2 (90x25)"
                        |> fixedwidth
                        |> size 2
                        |> filled black
                        |> move (-45,11)
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
