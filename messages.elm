module Messages exposing (Msg(..))

import Mouse exposing (..)
import Arrow exposing (Position)
import Ports exposing (BoundingBox)


type Msg
    = NoOp
    | PlaceMouseCoor Mouse.Position
    | BoundingBoxResult BoundingBox
