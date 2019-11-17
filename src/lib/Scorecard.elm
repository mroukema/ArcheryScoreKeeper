module Lib.Scorecard exposing (Scorecard)

import Dict exposing (Dict)
import Lib.Score as Score
import Lib.Shot as Shot


{-| -}
type alias Scorecard =
    Dict Int End


{-| -}
type alias End =
    Dict Int Record


{-| -}
type Record
    = Nothing
    | Shot Shot.Shot
    | Score Score.Score



-- Functions


getEnd : Int -> Scorecard -> Maybe End
getEnd endNum scores =
    scores
        |> Dict.get endNum


{-| getScore
Retrieve a score from an End

    endNum: get
    shotNum: key used to access

-}
getScore : Int -> Int -> Scorecard -> Maybe Record
getScore endNum shotNum scores =
    getEnd endNum scores
        |> Maybe.withDefault Dict.empty
        |> Dict.get shotNum
