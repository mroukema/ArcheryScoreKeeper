module Messages exposing (Msg(..))

import Types exposing (BoundingBox, IntPosition)


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
