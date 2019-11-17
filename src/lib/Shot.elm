module Lib.Shot exposing (Shot)

import Lib.Arrow exposing (Arrow, Radius)
import Lib.Score exposing (Score)
import Lib.Target exposing (Target)


type alias Pos =
    { x : Float
    , y : Float
    }


type Shot
    = Shot Pos Arrow (Pos -> Score)


pos : Shot -> Pos
pos shot =
    case shot of
        Shot pos_ _ _ ->
            pos_


radius : Shot -> Radius
radius shot =
    case shot of
        Shot _ radius_ _ ->
            radius_



-- target : Shot -> Target () Never
-- target shot =
--     case shot of
--         Shot _ _ target_ ->
--             target_


arrow : Shot -> Arrow
arrow shot =
    case shot of
        Shot _ arrow_ _ ->
            arrow_
