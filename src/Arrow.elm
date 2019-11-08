module Arrow exposing (Arrow, ArrowMetadata, ArrowPos)


type alias ArrowPos =
    { x : Float
    , y : Float
    }


type alias Arrow =
    { radius : Float
    , pos : ArrowPos
    , metadata : ArrowMetadata
    }


type alias ArrowMetadata =
    {}
