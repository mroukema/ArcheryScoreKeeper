module Target exposing (Target, defaultScoringOptions, scorePos, target, tenRingTarget, translateClientToSvgCoordinates, viewBoxToAttributeString)

import Arrow
import Score exposing (Score)
import Shot exposing (Shot)
import String exposing (fromFloat, fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)



--Model


viewBoxToAttributeString : ViewBox -> String
viewBoxToAttributeString viewBox =
    fromFloat viewBox.left ++ " " ++ fromFloat viewBox.top ++ " " ++ fromFloat viewBox.width ++ " " ++ fromFloat viewBox.width


translateClientToSvgCoordinates : BoundingBox -> ViewBox -> IntPosition -> FloatPosition
translateClientToSvgCoordinates boundingBox veiwBox clientPos =
    { x =
        ((toFloat clientPos.x - boundingBox.left)
            * (veiwBox.width / boundingBox.width)
        )
            + veiwBox.left
    , y =
        ((toFloat clientPos.y - boundingBox.top)
            * (veiwBox.height / boundingBox.height)
        )
            + veiwBox.top
    }


type LineBreakOption
    = Up
    | Down
    | Center


type alias ScoringOptions =
    { lineBreak : LineBreakOption
    , inner10s : Bool
    , innerXs : Bool
    }


defaultScoringOptions : ScoringOptions
defaultScoringOptions =
    { lineBreak = Up
    , inner10s = False
    , innerXs = True
    }


scorePos : ScoringOptions -> TargetSpec -> Arrow.ArrowSpec -> Shot
scorePos options targetSpec arrow =
    List.foldr (foldOp options) (compareShot arrow) targetSpec


foldOp : ScoringOptions -> TargetRingSpec -> Shot -> Shot
foldOp options targetRingSpec currentShot =
    let
        lineBreak =
            getLineBreakOptionForCurrentRing options targetRingSpec
    in
    case
        withinRingBounds lineBreak targetRingSpec.radius currentShot.arrow
            && targetScoreGreater targetRingSpec currentShot
    of
        True ->
            { currentShot | score = targetRingSpec.score }

        False ->
            currentShot


getLineBreakOptionForCurrentRing : ScoringOptions -> TargetRingSpec -> LineBreakOption
getLineBreakOptionForCurrentRing options targetRingSpec =
    case targetRingSpec.score.label of
        "X" ->
            if options.innerXs then
                Down

            else
                options.lineBreak

        "10" ->
            if options.inner10s then
                Down

            else
                options.lineBreak

        default ->
            options.lineBreak


targetScoreGreater : TargetRingSpec -> Shot -> Bool
targetScoreGreater targetSpec currentShot =
    targetSpec.score.value > currentShot.score.value


withinRingBounds : LineBreakOption -> Float -> Arrow.ArrowSpec -> Bool
withinRingBounds lineBreak targetRingRadius arrow =
    let
        lineBreakDistanceCoorection =
            case lineBreak of
                Up ->
                    -1 * arrow.radius

                Down ->
                    arrow.radius

                Center ->
                    0
    in
    targetRingRadius > (distanceFromCenter arrow.pos + lineBreakDistanceCoorection)


compareShot : Arrow.ArrowSpec -> Shot
compareShot arrow =
    Shot
        arrow
        (Score "M" 0)


distanceFromCenter : FloatPosition -> Float
distanceFromCenter pos =
    distance { x = 0, y = 0 } pos


distance : FloatPosition -> FloatPosition -> Float
distance posA posB =
    sqrt
        (((posA.x - posB.x)
            * (posA.x - posB.x)
         )
            + ((posA.y - posB.y)
                * (posA.y - posB.y)
              )
        )


type alias Target msg =
    { view : Svg msg
    , spec : TargetSpec
    , viewBox : ViewBox
    }


type alias TargetSpec =
    List TargetRingSpec


type alias TargetRingSpec =
    { radius : Float
    , lineThickness : Float
    , ringColor : String
    , lineColor : String
    , score : Score
    }


tenRingSpec : TargetSpec
tenRingSpec =
    [ TargetRingSpec 39.9 0.2 "white" "black" (Score "1" 1)
    , TargetRingSpec 35.9 0.2 "white" "black" (Score "2" 2)
    , TargetRingSpec 32 0.2 "black" "black" (Score "3" 3)
    , TargetRingSpec 27.9 0.2 "black" "white" (Score "4" 4)
    , TargetRingSpec 24 0.2 "#41b7c8" "black" (Score "5" 5)
    , TargetRingSpec 19.9 0.2 "#41b7c8" "black" (Score "6" 6)
    , TargetRingSpec 15.9 0.2 "#fd1b14" "black" (Score "7" 7)
    , TargetRingSpec 11.9 0.2 "#fd1b14" "black" (Score "8" 8)
    , TargetRingSpec 7.9 0.2 "#fff535" "black" (Score "9" 9)
    , TargetRingSpec 3.9 0.2 "#fff535" "black" (Score "10" 10)
    , TargetRingSpec 1.9 0.1 "#fff535" "black" (Score "X" 10)
    ]



-- View


targetGenerator : List TargetRingSpec -> Svg msg
targetGenerator targetSpec =
    Svg.g
        []
        (List.concat
            [ List.map ringGenerator targetSpec
            , [ centerCrossHair ]
            ]
        )


ringGenerator : TargetRingSpec -> Svg msg
ringGenerator ringSpec =
    Svg.circle (ringSpecToAttributeList ringSpec) []


ringSpecToAttributeList : TargetRingSpec -> List (Attribute msg)
ringSpecToAttributeList ringSpec =
    [ cx "0"
    , cy "0"
    , r (fromFloat ringSpec.radius)
    , fill ringSpec.ringColor
    , stroke ringSpec.lineColor
    , strokeWidth (fromFloat ringSpec.lineThickness)
    ]


centerCrossHair : Svg msg
centerCrossHair =
    Svg.path [ d "M -0.2 0 L 0.2 0 M 0 -0.2 L 0 0.2", stroke "black", strokeWidth "0.1", id "Center" ] []


target : Target msg
target =
    { view = targetGenerator tenRingSpec
    , spec = tenRingSpec
    , viewBox = ViewBox -45 -45 90 90
    }


tenRingTarget : Target msg
tenRingTarget =
    { view = targetGenerator tenRingSpec
    , spec = tenRingSpec
    , viewBox = ViewBox -45 -45 90 90
    }
