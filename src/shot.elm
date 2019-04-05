module Shot exposing (FloatPosition, Shot)

--TODO fix loose coupling


type alias ArrowSpec =
    Float


type alias FloatPosition =
    { x : Float
    , y : Float
    }


{-| Describes where the shot landed on target and what sort of arrow was used
-}
type alias Shot =
    { arrowRadius : ArrowSpec
    , pos : FloatPosition
    }
