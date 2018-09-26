import GraphicSVG exposing (..)
import GraphicSVG.App exposing (graphicsApp,GraphicsApp)

main : GraphicsApp
main = graphicsApp 
    { view = view }

view : Collage ()
view = collage 192 168 
    [ circle 50 |> filled yellow
    , circle 10
        |> filled black
        |> move (20,10)
    , circle 10 
        |> filled black
        |> move (-20,10)
    , curve (-20,-20) [Pull (0, -40) (20, -20)]
        |> outlined (solid 5) black
    ]
