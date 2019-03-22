module CurrentEndInputTarget exposing (CurrentEnd, CurrentEndControlData, End, EndEntry(..), addEntryValue, appendIfIsShot, currentEnd, endEntryRadioButton, endEntryView, endNumberSection, endTotalSection, firstEmptyIndexOrCurrent, getShotsFromEnd, initInitialShotList, initialControlData, initialEnd, selectArrowIndex, totalEndEntries, updateCurrentEnd, view)

-- elm-lang imports
-- user iports

import Array exposing (Array)
import Arrow
import Html exposing (..)
import Html.Attributes exposing (class)
import Messages exposing (Msg)
import Shot exposing (Shot)
import ShotPlacer exposing (shotPlacer)
import String exposing (fromFloat, fromInt)
import Svg exposing (svg)
import Svg.Attributes exposing (d, fill, height, id, path, stroke, version, viewBox, width)
import Target exposing (..)
import Types exposing (..)



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
    { selectedArrowIndex : Maybe Int
    , viewBox : ViewBox
    , boundingBox : BoundingBox
    , dragInProgress : Bool
    , currentEndIndex : Int
    }


type alias CurrentEnd =
    { endData : End
    , controlData : CurrentEndControlData
    }


initInitialShotList : Int -> Array EndEntry
initInitialShotList numShots =
    Array.repeat numShots Empty


initialEnd : Int -> Int -> End
initialEnd shotsPerEnd endNumber =
    { endEntries = initInitialShotList shotsPerEnd
    , shotsPerEnd = shotsPerEnd
    , endNumber = endNumber
    }


initialControlData : CurrentEndControlData
initialControlData =
    CurrentEndControlData
        (Just 0)
        (Types.ViewBox -45 -45 90 90)
        (Types.BoundingBox 0 0 0 0 0 0)
        False
        0


getShotsFromEnd : Array EndEntry -> List Shot
getShotsFromEnd endEntries =
    List.foldr appendIfIsShot [] (Array.toList endEntries)


appendIfIsShot : EndEntry -> List Shot -> List Shot
appendIfIsShot endEntry shotList =
    case endEntry of
        FilledEntry shot ->
            List.append [ shot ] shotList

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
                , shotPlacer
                    (List.indexedMap
                        (\a b -> ( a, b ))
                        (getShotsFromEnd <| model.endData.endEntries)
                    )
                    model.controlData.selectedArrowIndex
                    model.controlData.dragInProgress
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
            [ [ endNumberSection <| fromInt <| end.endNumber + 1 ]
            , List.map endEntryRadioButton (Array.toIndexedList end.endEntries)
            , [ endTotalSection <| fromInt <| totalEndEntries end.endEntries ]
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


endEntryRadioButton : ( Int, EndEntry ) -> Html msg
endEntryRadioButton ( index, endEntry ) =
    case endEntry of
        FilledEntry shot ->
            section
                [ class "mdc-card__media" ]
                [ h2 [ id (fromInt index) ] [ text shot.score.label ] ]

        Empty ->
            section
                [ class "mdc-card__media" ]
                [ h2 [ id (fromInt index) ] [ text "-" ] ]



-- Update


selectArrowIndex : Array EndEntry -> CurrentEndControlData -> Maybe Int -> CurrentEndControlData
selectArrowIndex endEntries controls index =
    case index of
        Just index_ ->
            { controls | selectedArrowIndex = index }

        Nothing ->
            { controls | selectedArrowIndex = firstEmptyIndexOrCurrent endEntries index }


firstEmptyIndexOrCurrent : Array EndEntry -> Maybe Int -> Maybe Int
firstEmptyIndexOrCurrent endEntries current =
    let
        emptyEntryIndex : Maybe Int
        emptyEntryIndex =
            List.foldr
                (\( index, entry ) currentIndex ->
                    case entry of
                        Empty ->
                            Just index

                        FilledEntry shot ->
                            currentIndex
                )
                Nothing
                (Array.toIndexedList endEntries)
    in
    case emptyEntryIndex of
        Just index ->
            Just index

        Nothing ->
            current


updateCurrentEnd : CurrentEndControlData -> End -> IntPosition -> BoundingBox -> ( End, CurrentEndControlData )
updateCurrentEnd controlData currentEndData mousePos boundingBox =
    let
        shot =
            Target.scorePos
                Target.defaultScoringOptions
                target.spec
                (Arrow.defaultArrow
                    (translateClientToSvgCoordinates boundingBox controlData.viewBox mousePos)
                )

        updatedEntries =
            case controlData.selectedArrowIndex of
                Just index ->
                    Array.set
                        index
                        (FilledEntry shot)
                        currentEndData.endEntries

                Nothing ->
                    currentEndData.endEntries
    in
    ( { currentEndData
        | endEntries = updatedEntries
      }
    , { controlData | selectedArrowIndex = firstEmptyIndexOrCurrent updatedEntries controlData.selectedArrowIndex }
    )
