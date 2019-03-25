module Arrow exposing (ArrowSpec, FloatPosition, arrow, arrowBase, defaultArrow)

import Json.Decode as Decode
import String exposing (fromFloat)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)



-- Model


type alias FloatPosition =
    { x : Float, y : Float }


type alias Shot =
    { arrow : ArrowSpec
    , pos : ( Float, Float )
    }


type alias ArrowSpec =
    Float


defaultArrow : ArrowSpec
defaultArrow =
    0.65


arrow shot =
    let
        ( x, y ) =
            shot.pos
    in
    Svg.g
        [ transform
            ("translate("
                ++ fromFloat x
                ++ ", "
                ++ fromFloat y
                ++ ")"
            )
        ]
    <|
        arrowBase defaultArrow


arrowBase : Float -> List (Svg msg)
arrowBase radius =
    [ Svg.circle
        [ cx "0"
        , cy "0"
        , r (fromFloat radius)
        , fill "white"
        , stroke "black"
        , strokeWidth (fromFloat 0.1)
        , id "arrowCircle"
        ]
        []
    , Svg.path [ d "M -0.3 0 L 0.3 0 M 0 -0.3 L 0 0.3", stroke "black", strokeWidth "0.1", id "Center" ] []
    ]
