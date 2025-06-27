module GraphicSVG.Transforms exposing (..)
import GraphicSVG.Types exposing (Transform)

ident : Transform
ident =
    ( ( 1, 0, 0 )
    , ( 0, 1, 0 )
    )

moveT : ( Float, Float ) -> Transform -> Transform
moveT ( u, v ) ( ( a, c, tx ), ( b, d, ty ) ) =
    ( ( a, c, tx + a * u + c * v )
    , ( b, d, ty + b * u + d * v )
    )

rotateT : Float -> Transform -> Transform
rotateT rad ( ( a, c, tx ), ( b, d, ty ) ) =
    let
        sinX =
            sin rad

        cosX =
            cos rad
    in
    ( ( a * cosX + c * sinX, c * cosX - a * sinX, tx )
    , ( b * cosX + d * sinX, d * cosX - b * sinX, ty )
    )


scaleT : Float -> Float -> Transform -> Transform
scaleT sx sy ( ( a, c, tx ), ( b, d, ty ) ) =
    ( ( a * sx, c * sy, tx )
    , ( b * sx, d * sy, ty )
    )


skewT : Float -> Float -> Transform -> Transform
skewT skx sky ( ( a, c, tx ), ( b, d, ty ) ) =
    let
        tanX =
            tan -skx

        tanY =
            tan -sky
    in
    ( ( a + c * tanY, c + a * tanX, tx )
    , ( b + d * tanY, d + b * tanX, ty )
    )


rotateAboutT : ( Float, Float ) -> Float -> Transform -> Transform
rotateAboutT ( u, v ) rad ( ( a, c, tx ), ( b, d, ty ) ) =
    let
        sinX =
            sin rad

        cosX =
            cos rad
    in
    ( ( a * cosX + c * sinX, c * cosX - a * sinX, tx + a * u + c * v - v * (c * cosX - a * sinX) - u * (a * cosX + c * sinX) )
    , ( b * cosX + d * sinX, d * cosX - b * sinX, ty + b * u + d * v - v * (d * cosX - b * sinX) - u * (b * cosX + d * sinX) )
    )



-- Matrix multiplication for transformations
matrixMult : Transform -> Transform -> Transform
matrixMult ( ( a, c, e ), ( b, d, f ) ) ( ( a1, c1, e1 ), ( b1, d1, f1 ) ) =
    ( ( a * a1 + c * b1, a * c1 + c * d1, e + a * e1 + c * f1 )
    , ( b * a1 + d * b1, b * c1 + d * d1, f + b * e1 + d * f1 )
    )