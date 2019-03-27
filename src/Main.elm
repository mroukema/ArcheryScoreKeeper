module Main exposing (main)

import Arrow exposing (arrow, selectedArrow)
import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Dict exposing (Dict)
import Element exposing (Element, fill, height, px, rgb255, rgba255, text, width)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Set exposing (Set)
import String exposing (fromFloat)
import Svg exposing (Svg, svg)
import Svg.Attributes as SvgAttr
import Svg.Events as SvgEvents
import Target exposing (tenRingTarget)
import Task



-- Init


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getViewport )



-- Model


type alias ArrowMsg =
    Arrow.Msg


type Msg
    = NoOp
    | WindowResize Int Int
    | ViewportResult Dom.Viewport
    | SelectEnd Int
    | DeselectEnd Int
    | SelectShot (Maybe ShotId)


type alias Model =
    { viewport : Maybe Dom.Viewport
    , scorecard : Scorecard
    , selectedEnds : Set Int
    , selectedShot : Maybe ShotId
    }


type alias ViewModel =
    { viewsize : Element.Length
    , selectedRecord : Maybe RecordSelection
    , selectedEnds : Scorecard
    , unselectedEnds : Scorecard
    }


type alias ScorecardSelection t =
    ( ShotId, t )


type alias ShotSelection =
    ScorecardSelection Shot


type alias RecordSelection =
    ScorecardSelection ScoreRecord


type alias ShotId =
    ( Int, Int )


type alias Score =
    { label : String
    , value : Int
    }


type alias Scorecard =
    Dict Int End


type alias ScoreRecord =
    { score : Score
    , shot : Maybe Shot
    }


type alias End =
    Dict Int ScoreRecord


type alias ArrowSpec =
    Float


type alias Shot =
    { arrow : ArrowSpec
    , pos : ( Float, Float )
    }


type alias EndStyle r =
    { r
        | row : List (Element.Attribute Msg)
        , id : List (Element.Attribute Msg)
        , score : List (Element.Attribute Msg)
    }


arrowSpec : ArrowSpec
arrowSpec =
    0.65


initialModel : Model
initialModel =
    Model
        Nothing
        (Dict.fromList
            [ ( 1
              , endFromScores
                    [ ScoreRecord
                        (Score "10" 10)
                        (Just <| Shot arrowSpec ( 1.0, 2.0 ))
                    , ScoreRecord
                        (Score "9" 9)
                        (Just <| Shot arrowSpec ( 5, 4.8 ))
                    , ScoreRecord
                        (Score "9" 9)
                        (Just <| Shot arrowSpec ( 3, 4 ))
                    ]
              )
            , ( 2
              , endFromScores
                    [ ScoreRecord
                        (Score "X" 10)
                        (Just <| Shot arrowSpec ( 0.3, 0.2 ))
                    , ScoreRecord
                        (Score "9" 9)
                        (Just <| Shot arrowSpec ( -4.0, 3.8 ))
                    , ScoreRecord
                        (Score "9" 9)
                        (Just <| Shot arrowSpec ( -2.1, -4.2 ))
                    ]
              )
            , ( 3
              , endFromScores
                    [ ScoreRecord (Score "X" 10) Nothing
                    , ScoreRecord (Score "10" 10) Nothing
                    , ScoreRecord (Score "9" 9) Nothing
                    ]
              )
            , ( 4
              , endFromScores
                    [ ScoreRecord (Score "10" 10) Nothing
                    , ScoreRecord (Score "10" 10) Nothing
                    , ScoreRecord (Score "8" 8) Nothing
                    ]
              )
            , ( 5
              , endFromScores
                    [ ScoreRecord (Score "9" 10) Nothing
                    , ScoreRecord (Score "9" 10) Nothing
                    , ScoreRecord (Score "9" 8) Nothing
                    ]
              )
            , ( 6
              , endFromScores
                    [ ScoreRecord (Score "X" 10) Nothing
                    , ScoreRecord (Score "10" 10) Nothing
                    , ScoreRecord (Score "9" 8) Nothing
                    ]
              )
            ]
        )
        (Set.fromList [ 1 ])
        (Just ( 1, 2 ))



-- Update


update msg model =
    case msg of
        WindowResize x y ->
            ( model, getViewport )

        ViewportResult viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )

        SelectEnd index ->
            ( { model | selectedEnds = Set.insert index model.selectedEnds }, Cmd.none )

        DeselectEnd index ->
            ( { model | selectedEnds = Set.remove index model.selectedEnds }, Cmd.none )

        SelectShot selection ->
            ( { model | selectedShot = selection }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


getViewport =
    Task.perform ViewportResult Dom.getViewport


endFromScores : List ScoreRecord -> End
endFromScores scores =
    let
        scoreIndices =
            List.range 1 <| List.length scores
    in
    List.map2 Tuple.pair scoreIndices scores
        |> Dict.fromList



-- Subscriptions


subscriptions model =
    Sub.batch [ onResize WindowResize ]



--  View


scorecardDataSelector : Model -> ViewModel
scorecardDataSelector model =
    let
        viewsize =
            viewportSize model.viewport

        ( selectedEnds, unselectedEnds ) =
            Dict.partition
                (\endIndex _ ->
                    List.any
                        (\selectedIndex -> endIndex == selectedIndex)
                        (Set.toList model.selectedEnds)
                )
                model.scorecard

        selectedRecord =
            case model.selectedShot of
                Just selection ->
                    Dict.get (Tuple.first selection) selectedEnds
                        |> Maybe.withDefault Dict.empty
                        |> Dict.get (Tuple.second selection)
                        |> Maybe.andThen (\record -> Just ( selection, record ))

                Nothing ->
                    Nothing
    in
    ViewModel
        viewsize
        selectedRecord
        selectedEnds
        unselectedEnds


view =
    scorecard << scorecardDataSelector


scorecard : ViewModel -> Html Msg
scorecard model =
    Element.layout
        [ Element.width model.viewsize ]
        (Element.column
            [ Element.spacing 1
            ]
            (List.concat
                [ renderSelectedEnds model.selectedEnds
                , [ targetElement
                        { selectedEnds = model.selectedEnds
                        , viewsize = model.viewsize
                        , shotSelection = model.selectedRecord
                        }
                  ]
                , targetScorecard model.unselectedEnds
                ]
            )
        )


viewportSize maybeViewport =
    case maybeViewport of
        Just value ->
            let
                viewport =
                    value.viewport
            in
            min (floor viewport.width) (floor viewport.height) |> px

        Nothing ->
            0 |> px


type alias TargetData r =
    { r
        | viewsize : Element.Length
        , selectedEnds : Scorecard
        , shotSelection : Maybe RecordSelection
    }


excludeSelectedShot selectedShot ends =
    Tuple.second
        (List.partition
            (\( shotId, _ ) -> Just shotId == Maybe.map Tuple.first selectedShot)
            (shotsFromEnds ends)
        )


targetElement : TargetData r -> Element Msg
targetElement { viewsize, selectedEnds, shotSelection } =
    let
        endShots =
            excludeSelectedShot shotSelection selectedEnds
    in
    svg
        [ SvgAttr.version "1.1"
        , SvgAttr.width "100%"
        , SvgAttr.height "100%"
        , SvgAttr.viewBox <|
            Target.viewBoxToAttributeString tenRingTarget.viewBox
        , SvgAttr.id "TargetSvg"
        ]
        [ tenRingTarget.view
        , renderShots endShots shotSelection
        ]
        |> Element.html
        |> Element.el
            [ Element.height viewsize
            , Element.width viewsize
            , Background.color <| rgba255 150 200 200 0.8
            ]


shotsFromEnd : ( Int, End ) -> List ShotSelection
shotsFromEnd ( endIndex, end ) =
    end
        |> Dict.toList
        |> List.filterMap
            (\( shotIndex, record ) ->
                case record.shot of
                    Just shot ->
                        Just ( ( endIndex, shotIndex ), shot )

                    Nothing ->
                        Nothing
            )


shotsFromEnds : Scorecard -> List ShotSelection
shotsFromEnds ends =
    List.map shotsFromEnd (Dict.toList ends) |> List.concat


renderShots : List ShotSelection -> Maybe RecordSelection -> Svg Msg
renderShots shots recordSelection =
    let
        selectionArrow =
            case recordSelection of
                Just ( recordId, record ) ->
                    case record.shot of
                        Just shot ->
                            [ selectedArrow
                                shot
                                [ SvgEvents.onClick <| SelectShot Nothing ]
                            ]

                        Nothing ->
                            []

                Nothing ->
                    []
    in
    Svg.g [] <|
        List.append
            (List.map
                (\shotRecord ->
                    arrow
                        (Tuple.second shotRecord)
                        [ SvgEvents.onClick <| SelectShot <| Just (Tuple.first shotRecord) ]
                )
                shots
            )
            selectionArrow


targetScorecard ends =
    ends
        |> Dict.toList
        |> List.map renderUnselectedEnd



-- Scoring End Views


renderSelectedEnds : Scorecard -> List (Element Msg)
renderSelectedEnds selectedEnds =
    List.map
        renderSelectedEnd
        (Dict.toList selectedEnds)


baseEndStyle =
    { row =
        [ Element.spacing 12
        , Element.width fill
        , Element.spaceEvenly
        ]
    , id =
        [ Element.padding 6 ]
    , score =
        [ Element.width fill
        , Font.center
        ]
    }


selectedEndStyle endIndex =
    { baseEndStyle
        | row =
            List.append
                baseEndStyle.row
                [ Background.color <| rgba255 70 50 230 0.8 ]
        , id =
            List.append
                baseEndStyle.id
                [ Font.color <| rgb255 255 255 255
                , Events.onClick <| DeselectEnd endIndex
                ]
        , score =
            List.append
                baseEndStyle.score
                [ Font.color <| rgb255 255 255 255
                ]
    }


unselectedEndStyle index =
    { baseEndStyle
        | id =
            List.append
                baseEndStyle.id
                [ Events.onClick <| SelectEnd index ]
        , row =
            List.append
                baseEndStyle.row
                [ Background.color <| rgba255 150 200 200 0.8 ]
    }


renderSelectedEnd : ( Int, End ) -> Element Msg
renderSelectedEnd (( index, _ ) as endSelection) =
    renderScoringEnd (selectedEndStyle index) endSelection


renderUnselectedEnd : ( Int, End ) -> Element Msg
renderUnselectedEnd (( index, _ ) as end) =
    renderScoringEnd (unselectedEndStyle index) end


renderScoringEnd : EndStyle r -> ( Int, End ) -> Element Msg
renderScoringEnd endStyle ( index, scores ) =
    endNumber endStyle.id index
        :: renderScores endStyle.score ( index, scores )
        |> Element.row endStyle.row


nestedDictMap : Dict comparable (Dict comparable1 t) -> Dict ( comparable, comparable1 ) t
nestedDictMap dict =
    Dict.foldl
        (\key value result ->
            Dict.foldl
                (\innerKey innerValue _ ->
                    Dict.insert ( key, innerKey ) innerValue result
                )
                Dict.empty
                value
        )
        Dict.empty
        dict


endNumber : List (Element.Attribute Msg) -> Int -> Element Msg
endNumber style num =
    "#"
        ++ String.fromInt num
        |> text
        |> Element.el style


renderScores : List (Element.Attribute Msg) -> ( Int, End ) -> List (Element Msg)
renderScores style ( endIndex, end ) =
    end
        |> Dict.toList
        |> List.map
            (\( recordIndex, record ) ->
                ( ( endIndex, recordIndex ), record )
            )
        |> List.map
            (\( shotSelector, record ) ->
                Element.el
                    ((Events.onClick <| SelectShot (Just shotSelector)) :: style)
                    (text record.score.label)
            )
