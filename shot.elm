module Shot exposing (..)

import Svg exposing (..)
import Score exposing (Score)
import Arrow exposing (ArrowSpec, arrowSpecToArrowSvg, defaultArrow)


type alias Shot =
    { arrow : ArrowSpec
    , score : Score
    }


arrow : Shot -> Svg msg
arrow shot =
    arrowSpecToArrowSvg shot.arrow
