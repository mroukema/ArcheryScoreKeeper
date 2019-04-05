module Target exposing (LineBreakOption, ScoreTarget, Target, TargetSpec, center, defaultScoringOptions, down, scorePos, tenRingScoreTarget, tenRingTarget, translateClientToSvgCoordinates, up, viewBoxToAttributeString)

import Browser.Dom as Dom
import Score exposing (Score)
import Shot exposing (FloatPosition, Shot)
import String exposing (fromFloat, fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)



--Model


viewBoxToAttributeString : ViewBox -> String
viewBoxToAttributeString viewBox =
    ""
        ++ fromFloat viewBox.left
        ++ " "
        ++ fromFloat viewBox.top
        ++ " "
        ++ fromFloat viewBox.width
        ++ " "
        ++ fromFloat viewBox.width


translateClientToSvgCoordinates : ViewBox -> Maybe Dom.Element -> IntPosition -> FloatPosition
translateClientToSvgCoordinates viewBox maybeElement clientPos =
    let
        elementDom =
            case maybeElement of
                Just dom ->
                    dom

                Nothing ->
                    { viewport = { width = 0, height = 0, x = 0, y = 0 }
                    , element = { width = 0, height = 0, x = 0, y = 0 }
                    , scene = { height = 0, width = 0 }
                    }

        viewport =
            elementDom.viewport

        element =
            elementDom.element
    in
    { x =
        ((toFloat clientPos.x - element.x + viewport.x) * (viewBox.width / element.width))
            + viewBox.left
    , y =
        ((toFloat clientPos.y - element.y + viewport.y) * (viewBox.height / element.height))
            + viewBox.top
    }


type alias IntPosition =
    { x : Int
    , y : Int
    }


type alias BoundingBox =
    { bottom : Float
    , height : Float
    , left : Float
    , right : Float
    , top : Float
    , width : Float
    }


type alias ViewBox =
    { left : Float
    , top : Float
    , width : Float
    , height : Float
    }


type alias ArrowSpec =
    Float


{-| The different ways shot breaking lines can be scored
-}
type LineBreakOption
    = Up
    | Down
    | Center


up =
    Up


down =
    Down


center =
    Center


type alias ScoringOptions =
    { lineBreak : LineBreakOption
    , inner10s : Bool
    , innerXs : Bool
    }


type alias ScoreTarget =
    { spec : TargetSpec
    , viewBox : ViewBox
    , scoringOptions : ScoringOptions
    }


defaultScoringOptions : ScoringOptions
defaultScoringOptions =
    { lineBreak = Up
    , inner10s = False
    , innerXs = True
    }


scorePos : ScoreTarget -> Shot -> Score
scorePos scoreTarget shot =
    scoreTarget.spec
        |> List.foldr
            (foldOp scoreTarget.scoringOptions shot)
            (Score "M" 0)


foldOp : ScoringOptions -> Shot -> TargetRingSpec -> Score -> Score
foldOp options shot targetRingSpec outputScore =
    let
        lineBreakDistanceCorrection =
            case getLineBreakBehaviour options targetRingSpec of
                Up ->
                    (+) (Basics.negate shot.arrowRadius)

                Down ->
                    (+) shot.arrowRadius

                Center ->
                    Basics.identity

        correctedShotDistance =
            distanceFromCenter shot.pos
                |> lineBreakDistanceCorrection
    in
    case
        (targetRingSpec.score.value > outputScore.value)
            && (targetRingSpec.radius > correctedShotDistance)
    of
        True ->
            targetRingSpec.score

        False ->
            outputScore


getLineBreakBehaviour : ScoringOptions -> TargetRingSpec -> LineBreakOption
getLineBreakBehaviour options targetRingSpec =
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


targetScoreGreater : TargetRingSpec -> Score -> Bool
targetScoreGreater targetSpec currentScore =
    targetSpec.score.value > currentScore.value


withinRingBounds : Float -> Float -> Bool
withinRingBounds centerDistance targetRingRadius =
    targetRingRadius > centerDistance


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
    Svg.g [] <|
        List.append
            (List.map ringGenerator targetSpec)
            [ centerCrossHair ]


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
    Svg.path
        [ d "M -0.2 0 L 0.2 0 M 0 -0.2 L 0 0.2"
        , stroke "black"
        , strokeWidth "0.1"
        , id "Center"
        ]
        []


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


tenRingScoreTarget =
    { scoringOptions = ScoringOptions Up False True
    , spec = tenRingSpec
    , viewBox = ViewBox -45 -45 90 90
    }
