module Shot exposing (..)

import Svg exposing (..)
import Score exposing (Score)
import Arrow exposing (ArrowSpec, arrowSpecToArrowSvg, arrowSpecToSelectedArrowSvg, defaultArrow)
import Messages exposing (Msg)


type alias Shot =
    { arrow : ArrowSpec
    , score : Score
    }


arrow : ( Int, ArrowSpec ) -> Bool -> Svg Msg
arrow indexedShot isSelected =
    case isSelected of
        True ->
            arrowSpecToSelectedArrowSvg indexedShot

        False ->
            arrowSpecToArrowSvg indexedShot
