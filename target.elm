module Target exposing (target, Target, viewBoxToAttributeString, BoundingBox, ViewBox, translateClientToSvgCoordinates, scorePos, defaultScoringOptions)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Score exposing (Score)
import Arrow
import Mouse
import Shot exposing (Shot)


--Model


type alias BoundingBox =
    { bottom : Float
    , height : Float
    , left : Float
    , right : Float
    , top : Float
    , width : Float
    }


type alias ViewBox =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


viewBoxToAttributeString : ViewBox -> String
viewBoxToAttributeString viewBox =
    (toString viewBox.x) ++ " " ++ (toString viewBox.y) ++ " " ++ (toString viewBox.width) ++ " " ++ (toString viewBox.width)


translateClientToSvgCoordinates : BoundingBox -> ViewBox -> Mouse.Position -> Arrow.Position
translateClientToSvgCoordinates bBox vBox clientPos =
    { x =
        (((toFloat clientPos.x) - bBox.left)
            * (vBox.width / bBox.width)
        )
            + (vBox.x)
    , y =
        (((toFloat clientPos.y) - bBox.top)
            * (vBox.height / bBox.height)
        )
            + vBox.y
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
scorePos options target arrow =
    List.foldl (foldOp options) (compareShot arrow) target



--List.foldl (a -> b -> b) b List a (::) target.spec arrow.pos


foldOp : ScoringOptions -> TargetRingSpec -> Shot -> Shot
foldOp options targetRingSpec currentShot =
    let
        lineBreak =
            getLineBreakOptionForCurrentRing options targetRingSpec
    in
        case
            (withinRingBounds lineBreak targetRingSpec.radius currentShot.arrow)
                && (targetScoreGreater targetRingSpec currentShot)
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
    not (currentShot.score.value > targetSpec.score.value)


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
        targetRingRadius > ((distanceFromCenter arrow.pos) + lineBreakDistanceCoorection)


compareShot : Arrow.ArrowSpec -> Shot
compareShot arrow =
    Shot
        arrow
        (Score "M" 0)


distanceFromCenter : Arrow.Position -> Float
distanceFromCenter pos =
    distance { x = 0, y = 0 } pos


distance : Arrow.Position -> Arrow.Position -> Float
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


tenRingTarget : TargetSpec
tenRingTarget =
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


targetGen : List TargetRingSpec -> List (Svg msg)
targetGen targetSpec =
    List.map ringGen targetSpec


ringGen : TargetRingSpec -> Svg msg
ringGen ringSpec =
    Svg.circle (ringSpecToAttributeList ringSpec) []


ringSpecToAttributeList : TargetRingSpec -> List (Attribute msg)
ringSpecToAttributeList ringSpec =
    [ cx "0"
    , cy "0"
    , r (toString ringSpec.radius)
    , fill ringSpec.ringColor
    , stroke ringSpec.lineColor
    , strokeWidth (toString ringSpec.lineThickness)
    ]


centerCrossHair : Svg msg
centerCrossHair =
    Svg.path [ d "M -0.2 0 L 0.2 0 M 0 -0.2 L 0 0.2", stroke "black", strokeWidth "0.1", id "Center" ] []


target : Target msg
target =
    { view =
        Svg.g
            []
            (List.concat
                [ (targetGen tenRingTarget)
                , [ centerCrossHair ]
                ]
            )
    , spec = tenRingTarget
    , viewBox = ViewBox -45 -45 90 90
    }
