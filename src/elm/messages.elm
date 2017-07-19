module Messages exposing (Msg(..))

import Types exposing (IntPosition, BoundingBox)


type Msg
    = NoOp
    | ArrowDragPotentialStart Int IntPosition
    | ArrowDragStart IntPosition BoundingBox
    | ArrowDrag IntPosition
    | ArrowDragEnd IntPosition
    | SelectArrow Int
    | DeselectArrow
    | PlaceMouseCoor IntPosition
    | PlaceArrow IntPosition BoundingBox
    | BoundingBoxResult BoundingBox
