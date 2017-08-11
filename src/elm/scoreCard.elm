module ScoreCard exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)


-- User Imports

import CurrentEndInputTarget exposing (..)
import Shot exposing (..)
import Score exposing (..)


-- Model


type alias Model =
    Array End


sumEnd : List Score -> Int
sumEnd model =
    List.foldr (+) 0 (List.map .value model)


extractScoresFromShots : List Shot -> List Score
extractScoresFromShots scores =
    List.map .score scores



-- View


view : Model -> Html msg
view model =
    Html.div [ class "mdc-layout-grid__inner" ]
        [ Html.div
            [ (class " mdc-card mdc-layout-grid__cell mdc-layout-grid__cell--span-8") ]
            (List.map
                endEntry
                (Array.toList model)
            )
        ]



--, totalElem <| sumEnd <| scores


endEntry : End -> Html msg
endEntry end =
    Html.div
        [ (class "mdc-card__horizontal-block ") ]
        [ ul
            []
            (List.concat
                [ [ endNumberView end.endNumber ]
                , (List.map
                    scoreEntryView
                    (Array.toList end.endEntries)
                  )
                , [ totalEndEntries end.endEntries
                        |> toString
                        |> endTotalView
                  ]
                ]
            )
        ]


endTotalView : String -> Html msg
endTotalView total =
    li [ class "ask-horizontal-list" ] [ text total ]


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


endNumberView : Int -> Html msg
endNumberView endNumber =
    li [ class "ask-horizontal-list" ] [ text <| toString endNumber ]


scoreEntryView : EndEntry -> Html msg
scoreEntryView endEntry =
    case endEntry of
        FilledEntry shot ->
            li [ class "ask-horizontal-list" ] [ text shot.score.label ]

        Empty ->
            li [ class "ask-horizontal-list" ] [ text "-" ]


individualScoresElem : List Shot -> Html msg
individualScoresElem scores =
    Html.span
        [ (class "scoresContainer") ]
        (List.map scoreEntry scores)


totalElem : Int -> Html msg
totalElem sum =
    Html.span
        [ (class "endTotal") ]
        [ h3 [] [ text (toString sum) ] ]


scoreEntry : Shot -> Html msg
scoreEntry shot =
    Html.section
        [ (class "scoreEntry") ]
        [ h3 [] [ text shot.score.label ] ]
