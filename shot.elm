module Shot exposing (..)

import Svg exposing (..)
import Score exposing (Score)
import Arrow exposing (ArrowSpec, arrowSpecToArrowSvg, defaultArrow)
import Messages exposing (Msg)


type alias Shot =
    { arrow : ArrowSpec
    , score : Score
    }


arrow : ( Int, ArrowSpec ) -> Svg Msg
arrow indexedShot =
    arrowSpecToArrowSvg indexedShot
