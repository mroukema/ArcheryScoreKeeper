module Messages exposing (Msg(..), makeStagedMessage)

import Types exposing (IntPosition, BoundingBox)


makeStagedMessage : (IntPosition -> BoundingBox -> Msg) -> IntPosition -> Msg
makeStagedMessage messagePartial intPosition =
    StageMsgPartial (messagePartial intPosition)


type Msg
    = NoOp
    | ArrowDragPotentialStart Int IntPosition BoundingBox
    | ArrowDrag IntPosition
    | ArrowDragEnd IntPosition BoundingBox
    | SelectArrow Int
    | DeselectArrow
    | PlaceArrow IntPosition BoundingBox
    | StageMsgPartial (BoundingBox -> Msg)
    | BoundingBoxResult (Maybe (BoundingBox -> Msg)) BoundingBox
