module Shot exposing (Shot)

import Arrow exposing (Arrow, ArrowPos)
import Target exposing (Target)


type Shot
    = Shot ArrowPos Arrow (Target () Never)


shotPos : Shot -> ArrowPos
shotPos shot =
    case shot of
        Shot pos _ _ ->
            pos


shotRadius : Shot -> Float
shotRadius shot =
    case shot of
        Shot _ arrow _ ->
            arrow.radius


shotTarget : Shot -> Target () Never
shotTarget shot =
    case shot of
        Shot _ _ target ->
            target


shotArrow : Shot -> Arrow
shotArrow shot =
    case shot of
        Shot _ arrow _ ->
            arrow
