module Shot exposing (..)

import Svg exposing (..)
import Score exposing (Score)
import Arrow exposing (ArrowSpec, arrowSpecToArrowSvg, arrowSpecToSelectedArrowSvg, defaultArrow)
import Messages exposing (Msg)


type alias Shot =
    { arrow : ArrowSpec
    , score : Score
    }


arrow : ( Int, ArrowSpec ) -> Bool -> Bool -> Svg Msg
arrow indexedShot isSelected isDragInProgress =
    case isSelected of
        True ->
            arrowSpecToSelectedArrowSvg indexedShot isDragInProgress

        False ->
            arrowSpecToArrowSvg indexedShot
