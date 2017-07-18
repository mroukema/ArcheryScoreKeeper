module CurrentEndInputTarget exposing (..)

-- elm-lang imports

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (class)
import Svg exposing (svg)
import Svg.Attributes exposing (version, width, height, viewBox, id, viewBox, path, fill, stroke, d)
import Mouse


-- user iports

import Messages exposing (Msg)
import Target exposing (..)
import ShotPlacer exposing (shotPlacer)
import Shot exposing (Shot)
import Target exposing (..)
import Arrow


-- Model


type EndEntry
    = FilledEntry Shot
    | Empty


type alias End =
    { endEntries : Array EndEntry
    , shotsPerEnd : Int
    , endNumber : Int
    }


type alias CurrentEndControlData =
    { selectedArrowIndex : Int
    , viewBox : ViewBox
    , boundingBox : BoundingBox
    }


type alias CurrentEnd =
    { endData : End
    , controlData : CurrentEndControlData
    }


initInitialShotList : Int -> Array EndEntry
initInitialShotList numShots =
    Array.repeat numShots Empty


initialEnd : Int -> End
initialEnd shotsPerEnd =
    { endEntries = initInitialShotList shotsPerEnd
    , shotsPerEnd = shotsPerEnd
    , endNumber = 1
    }


initialControlData : CurrentEndControlData
initialControlData =
    CurrentEndControlData
        0
        (Target.ViewBox -45 -45 90 90)
        (Target.BoundingBox 0 0 0 0 0 0)


getShotsFromEnd : Array EndEntry -> List Shot
getShotsFromEnd endEntries =
    List.foldr appendIfIsShot [] (Array.toList endEntries)


appendIfIsShot : EndEntry -> List Shot -> List Shot
appendIfIsShot endEntry shotList =
    case endEntry of
        FilledEntry shot ->
            List.append shotList [ shot ]

        Empty ->
            shotList



-- View


view : CurrentEnd -> Html Messages.Msg
view model =
    div [ class "mdc-card mdc-layout-grid__cell mdc-layout-grid__cell--span-8" ]
        [ section [ class "mdc-card__media" ]
            [ svg
                [ version "1.1"
                , width "100%"
                , height "100%"
                , viewBox (viewBoxToAttributeString model.controlData.viewBox)
                , id "TargetSvg"
                ]
                [ target.view
                , shotPlacer (getShotsFromEnd <| model.endData.endEntries)
                ]
            ]
        , section [ class "mdc-card__media" ]
            [ h1 [ class "mdc-card__title mdc-card__title--large" ] [ currentEnd ]
            , h2 [ class "mdc-card__subtitle" ] [ text "In-Progress" ]
            ]
        , endEntryView model.endData
        ]


currentEnd : Html msg
currentEnd =
    Html.text "Current End"


endEntryView : End -> Html msg
endEntryView end =
    div
        [ class "mdc-card__horizontal-block" ]
        (List.concat
            [ [ endNumberSection <| toString <| end.endNumber ]
            , List.map endEntryRadioButton (Array.toList end.endEntries)
            , [ endTotalSection <| toString <| totalEndEntries end.endEntries ]
            ]
        )


totalEndEntries : Array EndEntry -> Int
totalEndEntries entries =
    Array.foldr addEntryValue 0 entries


addEntryValue : EndEntry -> Int -> Int
addEntryValue entryA sum =
    case entryA of
        FilledEntry shot ->
            shot.score.value + sum

        Empty ->
            sum


endNumberSection : String -> Html msg
endNumberSection endNumber =
    section
        [ class "mdc-card__media" ]
        [ h1 [] [ text endNumber ] ]


endTotalSection : String -> Html msg
endTotalSection endTotal =
    section
        [ class "mdc-card__media" ]
        [ h2 [] [ text endTotal ] ]


endEntryRadioButton : EndEntry -> Html msg
endEntryRadioButton endEntry =
    case endEntry of
        FilledEntry shot ->
            section
                [ class "mdc-card__media" ]
                [ h2 [] [ text shot.score.label ] ]

        Empty ->
            section
                [ class "mdc-card__media" ]
                [ h2 [] [ text "-" ] ]



-- Update


updateCurrentEnd : CurrentEndControlData -> End -> Mouse.Position -> BoundingBox -> ( End, CurrentEndControlData )
updateCurrentEnd controlData currentEnd mousePos boundingBox =
    let
        shot =
            (Target.scorePos
                Target.defaultScoringOptions
                target.spec
                (Arrow.defaultArrow
                    (translateClientToSvgCoordinates boundingBox controlData.viewBox mousePos)
                )
            )
    in
        ( { currentEnd
            | endEntries =
                (Array.set
                    controlData.selectedArrowIndex
                    (FilledEntry shot)
                    currentEnd.endEntries
                )
          }
        , { controlData | selectedArrowIndex = (controlData.selectedArrowIndex + 1) % currentEnd.shotsPerEnd }
        )
