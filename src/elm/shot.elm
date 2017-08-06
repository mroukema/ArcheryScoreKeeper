module Shot exposing (..)

import Svg exposing (..)
import Score exposing (Score)
import Arrow exposing (ArrowSpec, arrowSpecToArrowSvg, arrowSpecToSelectedArrowSvg, arrowSpecToDragArrowSvg, defaultArrow)
import Messages exposing (Msg)


type alias Shot =
    { arrow : ArrowSpec
    , score : Score
    }


arrow : ( Int, ArrowSpec ) -> Bool -> Bool -> Svg Msg
arrow indexedShot isSelected isDragInProgress =
    let
        selectedArrow =
            case isDragInProgress of
                True ->
                    arrowSpecToDragArrowSvg

                False ->
                    arrowSpecToSelectedArrowSvg
    in
        case isSelected of
            True ->
                selectedArrow indexedShot

            False ->
                arrowSpecToArrowSvg indexedShot
