module Target exposing (Target)

{-| -}

import Score exposing (Score(..))
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
            , scoreFunc = \_ -> Score "X" 10
            }
        )


{-| -}
type TargetSpec model msg
    = SvgSpec (model -> SvgTargetSpec msg)


{-| -}
type alias SvgTargetSpec msg =
    { elements : List (SvgElement msg)
    , defaultScore : Score
    , scoreFunc : Pos -> Score
    }


{-| -}
type alias Target model msg =
    { spec : TargetSpec model msg
    , rules : ScoringRules
    , meta : Metadata
    }


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
