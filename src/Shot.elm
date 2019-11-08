module Shot exposing (Shot)

import Arrow exposing (Arrow, ArrowPos)
import Target exposing (Target)


type Shot
    = Shot Arrow Target


shotPos : Shot -> ArrowPos
shotPos shot =
    case shot of
        Shot arrow _ ->
            arrow.pos


shotRadius : Shot -> Float
shotRadius shot =
    case shot of
        Shot arrow _ ->
            arrow.radius


shotTarget : Shot -> Target
shotTarget shot =
    case shot of
        Shot _ target ->
            target


shotArrow : Shot -> Arrow
shotArrow shot =
    case shot of
        Shot arrow _ ->
            arrow
