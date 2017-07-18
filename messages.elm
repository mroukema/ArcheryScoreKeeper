module Messages exposing (Msg(..))

import Types exposing (IntPosition, BoundingBox)


type Msg
    = NoOp
    | ArrowDragStart Int IntPosition
    | ArrowDrag Int IntPosition
    | ArrowDragEnd Int IntPosition
    | SelectArrow Int
    | PlaceMouseCoor IntPosition
    | BoundingBoxResult BoundingBox
