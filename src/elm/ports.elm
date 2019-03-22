port module Ports exposing (boundingBoxResult, getClientBoundingBox)

import Types exposing (BoundingBox)



-- port for sending strings out to JavaScript


port getClientBoundingBox : String -> Cmd msg



-- port for listening for suggestions from JavaScript


port boundingBoxResult : (BoundingBox -> msg) -> Sub msg
