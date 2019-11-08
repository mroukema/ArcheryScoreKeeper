module Scorecard exposing (Scorecard)

import Dict exposing (Dict)
import Score
import Shot


type alias Scorecard =
    Dict Int End


type alias End =
    Dict Int Record


type Record
    = Nothing
    | Shot Shot.Shot
    | Score Score.Score
