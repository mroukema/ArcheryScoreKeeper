module Lib.Target exposing (Target)

{-| -}

import Lib.Score exposing (Score(..))
import Svg
import Svg.Attributes as SvgAttr



-- Types


{-| -}
type alias Pos =
    { x : Float
    , y : Float
    }


{-| -}
type alias Metadata =
    {}


spec =
    SvgSpec
        (\_ ->
            { elements = []
            , defaultScore = Score "-" 0
            }
        )
        (\_ -> Score "X" 10)


{-| -}
type TargetSpec model msg
    = SvgSpec (model -> SvgTargetSpec msg) (Pos -> Score)


{-| -}
type alias SvgTargetSpec msg =
    { elements : List (SvgElement msg)
    , defaultScore : Score

    --    , scoreFunc : Pos -> Score
    }


{-| -}
type Target model msg
    = Target (TargetSpec model msg) ScoringRules Metadata


{-| -}
type alias ScoringRules =
    { innerX : Bool
    , inner10 : Bool
    , lineBreaks : LineBreakOptions
    }


{-| -}
type LineBreakOptions
    = Up
    | Down
    | Center


{-| -}
type SvgElement msg
    = Decorative (List (Svg.Attribute msg)) (List (Svg.Svg msg))
    | Functional Score (List (Svg.Attribute msg)) (List (Svg.Svg msg))



-- Functions


{-| -}
lineBreakDistanceCorrection : LineBreakOptions -> Float -> (Float -> Float)
lineBreakDistanceCorrection rule arrowRadius =
    case rule of
        Up ->
            (+) (Basics.negate arrowRadius)

        Down ->
            (+) arrowRadius

        Center ->
            Basics.identity
