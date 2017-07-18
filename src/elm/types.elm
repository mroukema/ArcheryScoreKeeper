module Types exposing (..)


type alias IntPosition =
    { x : Int
    , y : Int
    }


type alias FloatPosition =
    { x : Float
    , y : Float
    }


type alias BoundingBox =
    { bottom : Float
    , height : Float
    , left : Float
    , right : Float
    , top : Float
    , width : Float
    }


type alias ViewBox =
    { left : Float
    , top : Float
    , width : Float
    , height : Float
    }
