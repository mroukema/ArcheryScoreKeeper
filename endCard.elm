module EndCard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- User Imports

import Shot exposing (..)
import Score exposing (..)


-- Model


type alias Model =
    List Shot


sumEnd : List Score -> Int
sumEnd model =
    List.foldr (+) 0 (List.map .value model)


extractScoresFromShots : List Shot -> List Score
extractScoresFromShots scores =
    List.map .score scores



-- View


view : Model -> Html msg
view model =
    let
        scores =
            extractScoresFromShots <| model
    in
        Html.div
            [ (class "mdc-card") ]
            [ individualScoresElem model
            , totalElem <| sumEnd <| scores
            ]


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
