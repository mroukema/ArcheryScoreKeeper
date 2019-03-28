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


type Msg
    = NoOp
    | WindowResize Int Int
    | ViewportResult Dom.Viewport
    | SelectEnd Int
    | DeselectEnd Int
    | SelectShot (Maybe RecordId)


type alias Model =
    { viewport : Maybe Dom.Viewport
    , scorecard : Scorecard
    , selectedEnds : Set Int
    , selectedShot : Maybe RecordId
    }


{-| The information used to render the view. Derive from model.
-}
type alias ViewModel =
    { viewsize : Element.Length
    , selectedRecord : RecordSelection
    , selectedEnds : Scorecard
    , unselectedEnds : Scorecard
    }


{-| Tuple where first element identifes a scorecard record and t is some info associated with it
-}
type ScorecardSelection t
    = Nothing
    | Selection ( RecordId, t )


type alias ShotSelection =
    ScorecardSelection Shot


type alias RecordSelection =
    ScorecardSelection EndRecord


type alias RecordId =
    ( Int, Int )


type alias Score =
    { label : String
    , value : Int
    }


{-| Data for one arrow of an End.
Record can be empty or it can contain a score.
Optionally can there can be a record of the shot associated with the score
-}
type EndRecord
    = ScoreRecord Score
    | ShotRecord Score Shot
    | Empty


type alias Scorecard =
    Dict Int End


type alias End =
    Dict Int EndRecord


type alias ArrowSpec =
    Float


{-| Describes where the shot landed on target and what sort of arrow was used
-}
type alias Shot =
    { arrow : ArrowSpec
    , pos : ( Float, Float )
    }



-- Record Specs


type alias EndStyle r =
    { r
        | row : List (Element.Attribute Msg)
        , id : List (Element.Attribute Msg)
        , score : List (Element.Attribute Msg)
    }


type alias TargetData r =
    { r
        | viewsize : Element.Length
        , selectedEnds : Scorecard
        , shotSelection : RecordSelection
    }


arrowSpec : ArrowSpec
arrowSpec =
    0.65


initialModel : Model
initialModel =
    Model
        Maybe.Nothing
        (Dict.fromList
            [ ( 1
              , endFromScores
                    [ ShotRecord (Score "10" 10) (Shot arrowSpec ( 1.0, 2.0 ))
                    , ShotRecord (Score "9" 9) (Shot arrowSpec ( 5, 4.8 ))
                    , ShotRecord (Score "9" 9) (Shot arrowSpec ( 3, 4 ))
                    ]
              )
            , ( 2
              , endFromScores
                    [ ShotRecord (Score "X" 10) (Shot arrowSpec ( 0.3, 0.2 ))
                    , ShotRecord (Score "9" 9) (Shot arrowSpec ( -4.0, 3.8 ))
                    , ShotRecord (Score "9" 9) (Shot arrowSpec ( -2.1, -4.2 ))
                    ]
              )
            , ( 3
              , endFromScores
                    [ ScoreRecord (Score "X" 10)
                    , ScoreRecord (Score "10" 10)
                    , ScoreRecord (Score "9" 9)
                    ]
              )
            , ( 4
              , endFromScores
                    [ ScoreRecord (Score "10" 10)
                    , ScoreRecord (Score "10" 10)
                    , ScoreRecord (Score "8" 8)
                    ]
              )
            , ( 5
              , endFromScores
                    [ ScoreRecord (Score "9" 10)
                    , ScoreRecord (Score "9" 10)
                    , ScoreRecord (Score "9" 8)
                    ]
              )
            , ( 6
              , endFromScores
                    [ ScoreRecord (Score "X" 10)
                    , ScoreRecord (Score "10" 10)
                    , ScoreRecord (Score "9" 8)
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


endFromScores : List EndRecord -> End
endFromScores scores =
    let
        zip =
            List.map2 Tuple.pair

        scoreIndices =
            List.range 1 <| List.length scores
    in
    Dict.fromList (zip scoreIndices scores)



-- Subscriptions


subscriptions model =
    Sub.batch [ onResize WindowResize ]



--  View


view =
    scorecard << scorecardDataSelector


{-| Select and derive the data used to render view from model data
-}
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
                        |> (\result ->
                                case result of
                                    Just record ->
                                        Selection ( selection, record )

                                    Maybe.Nothing ->
                                        Nothing
                           )

                Maybe.Nothing ->
                    Nothing
    in
    ViewModel
        viewsize
        selectedRecord
        selectedEnds
        unselectedEnds


scorecard : ViewModel -> Html Msg
scorecard model =
    Element.layout
        [ Element.width model.viewsize ]
        (Element.column
            [ Element.spacing 1
            ]
            (List.concat
                [ renderSelectedEnds model.selectedEnds model.selectedRecord
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

        Maybe.Nothing ->
            0 |> px


excludeSelectedShot selectedShot ends =
    Tuple.second
        (List.partition
            (\selection ->
                case selection of
                    Selection ( recordId, _ ) ->
                        case selectedShot of
                            Nothing ->
                                False

                            Selection ( shotId, _ ) ->
                                recordId == shotId

                    Nothing ->
                        False
            )
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


{-| Get a list of (shotId, shot) pairs from the score records in the end that contain shot information.

    shotsFromEnd
        (ShotRecord
            |> Score "X" 10
            |> Shot arrowSpec ( 0.3, 0.2 )
        )

-}
shotsFromEnd : ( Int, End ) -> List ShotSelection
shotsFromEnd ( endIndex, end ) =
    let
        var =
            \( shotIndex, record ) ->
                case record of
                    ShotRecord score shot ->
                        Selection ( ( endIndex, shotIndex ), shot )

                    ScoreRecord score ->
                        Nothing

                    _ ->
                        Nothing
    in
    end
        |> Dict.toList
        |> List.map var


{-| Get a list of (shotId, shot) pairs from the score records within a scorecard that contain shot information

    shotsFromEnd scorecard

-}
shotsFromEnds : Scorecard -> List ShotSelection
shotsFromEnds ends =
    List.concat <| List.map shotsFromEnd (Dict.toList ends)


shotFromRecordSelection : RecordSelection -> Maybe ShotSelection
shotFromRecordSelection recordSelection =
    case recordSelection of
        Selection ( recordId, record ) ->
            case record of
                ShotRecord _ shot ->
                    Just <| Selection ( recordId, shot )

                _ ->
                    Maybe.Nothing

        _ ->
            Maybe.Nothing


renderShots : List ShotSelection -> RecordSelection -> Svg Msg
renderShots shots recordSelection =
    let
        selectionArrow =
            case shotFromRecordSelection recordSelection of
                Just selection ->
                    case selection of
                        Selection ( _, shot ) ->
                            [ selectedArrow
                                shot
                                [ SvgEvents.onClick <| SelectShot Maybe.Nothing ]
                            ]

                        Nothing ->
                            []

                _ ->
                    []
    in
    Svg.g [] <|
        List.append
            (List.map
                (\shotRecord ->
                    case shotRecord of
                        Selection ( recordId, record ) ->
                            arrow
                                record
                                [ SvgEvents.onClick <| SelectShot <| Just recordId ]

                        Nothing ->
                            Svg.g [] []
                )
                shots
            )
            selectionArrow


targetScorecard ends =
    ends
        |> Dict.toList
        |> List.map renderUnselectedEnd



-- Scoring End Views


renderSelectedEnds : Scorecard -> RecordSelection -> List (Element Msg)
renderSelectedEnds selectedEnds record =
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
                case record of
                    Empty ->
                        Element.el style <| text "-"

                    ShotRecord score _ ->
                        Element.el
                            ((Events.onClick <| SelectShot (Just shotSelector)) :: style)
                            (text score.label)

                    ScoreRecord score ->
                        Element.el style (text score.label)
            )
