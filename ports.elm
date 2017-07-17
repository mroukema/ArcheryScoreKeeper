port module Ports exposing (..)


type alias BoundingBox =
    { bottom : Float
    , height : Float
    , left : Float
    , right : Float
    , top : Float
    , width : Float
    }



-- port for sending strings out to JavaScript


port getClientBoundingBox : String -> Cmd msg



-- port for listening for suggestions from JavaScript


port boundingBoxResult : (BoundingBox -> msg) -> Sub msg
