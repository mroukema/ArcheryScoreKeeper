module Arrow exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


-- Model


type alias Position =
    { x : Float
    , y : Float
    }


type alias ArrowSpec =
    { radius : Float
    , pos : Position
    }


defaultArrow : Position -> ArrowSpec
defaultArrow =
    ArrowSpec 0.65



-- View


arrowSpecToArrowSvg : ArrowSpec -> Svg msg
arrowSpecToArrowSvg arrowSpec =
    g
        [ transform ("translate(" ++ (toString arrowSpec.pos.x) ++ ", " ++ (toString arrowSpec.pos.y) ++ ")") ]
        [ Svg.circle
            [ cx "0"
            , cy "0"
            , r (toString arrowSpec.radius)
            , fill "white"
            , stroke "black"
            , strokeWidth (toString 0.1)
            ]
            []
        , Svg.path [ d "M -0.3 0 L 0.3 0 M 0 -0.3 L 0 0.3", stroke "black", strokeWidth "0.1", id "Center" ] []
        ]


arrow : Position -> Svg msg
arrow pos =
    arrowSpecToArrowSvg (defaultArrow pos)
