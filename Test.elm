import GraphicSVG exposing(..)

main = graphicsApp{view=view}

view = collage 500 500 [circle 10 |> filled red]