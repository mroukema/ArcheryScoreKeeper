module Shot exposing (Shot, arrow)

import Arrow exposing (ArrowSpec, arrowSpecToArrowSvg, arrowSpecToDragArrowSvg, arrowSpecToSelectedArrowSvg, defaultArrow)
import Messages exposing (Msg)
import Score exposing (Score)
import Svg exposing (..)


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
