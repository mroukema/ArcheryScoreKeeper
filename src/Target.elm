module Target exposing (Target)

import Score exposing (Score)
import Svg


type alias Pos =
    { x : Float
    , y : Float
    }


type TargetSpec
    = Svg SvgTargetSpec


type alias SvgTargetSpec =
    { elements : List SvgElement
    , defaultScore : Score
    , scoreFunc : Pos -> Score
    }


type alias Target =
    {}


type SvgElement
    = Decorative (List ()) (List ())
    | Functional Score (List ()) (List ())
