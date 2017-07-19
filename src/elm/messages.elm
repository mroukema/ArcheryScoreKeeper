module Messages exposing (Msg(..))

import Types exposing (IntPosition, BoundingBox)


type Msg
    = NoOp
    | ArrowDragPotentialStart Int IntPosition
    | ArrowDragStart IntPosition
    | ArrowDrag IntPosition
    | ArrowDragEnd IntPosition
    | SelectArrow Int
    | DeselectArrow
    | PlaceMouseCoor IntPosition
    | BoundingBoxResult BoundingBox
