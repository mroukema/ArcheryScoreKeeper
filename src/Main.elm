module Main exposing (main)

import Arrow exposing (ArrowSpec, arrow, selectedArrow)
import Basics exposing (toFloat)
import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Element exposing (Element, fill, height, px, rgb255, rgba255, text, width)
import Element.Background as Element
import Element.Events as Element
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Json.Decode as Decode
import Score exposing (Score)
import Set exposing (Set)
import Shot exposing (FloatPosition, Shot)
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
    ( initialModel, getViewport targetLabel )


targetLabel =
    "TargetSvg"



-- Model


type Msg
    = NoOp
    | WindowResize Int Int
    | ViewportResult (Maybe Dom.Element)
    | SelectEnd Int
    | DeselectEnd Int
    | SelectShot (Maybe RecordId)
    | ArrowDragStart RecordId
    | ArrowDragMove RecordId Int IntPosition
    | ArrowDragEnd RecordId


type alias Model =
    { viewport : Maybe Dom.Element
    , scorecard : Scorecard
    , selectedEnds : Set Int
    , selectedShot : Maybe RecordId
    , dragInProgresss : Bool
    }


{-| The information used to render the view. Derived from model.
-}
type alias ViewModel =
    { viewsize : Element.Length
    , selectedRecord : RecordSelection
    , selectedEnds : RecordCard
    , unselectedEnds : RecordCard
    , dragInProgresss : Bool
    }


type alias IntPosition =
    { x : Int, y : Int }


{-| Tuple where first element identifes a scorecard record and t is some info associated with it
-}
type ScorecardSelection t
    = Nothing
    | Selection ( RecordId, t )


type alias ShotSelection =
    ScorecardSelection Shot


type alias RecordSelection =
    ScorecardSelection EndRecord


type alias RecordCard =
    Dict RecordId EndRecord


type alias RecordId =
    ( EndId, ShotId )


type alias RecordList =
    List ( RecordId, EndRecord )


type alias Score =
    { label : String
    , value : Int
    }


{-| Data for one arrow of an End.

  - Record can be empty or it can contain a score.
  - Optionally can there can be a record of the shot, target and scoring rules
    associated with the score

-}
type EndRecord
    = ScoreRecord Score
    | ShotRecord Score Shot ScoringTarget
    | EmptyRecord


type alias EndId =
    Int


type alias ShotId =
    Int


type alias Scorecard =
    Dict EndId End


type alias End =
    Dict ShotId EndRecord


{-| Record with the various different scoring rules that may be
applied to a shot when calculating it's score
-}
type alias ScoringOptions =
    { lineBreak : Target.LineBreakOption
    , inner10s : Bool
    , innerXs : Bool
    }


{-| TargetSpec with the info needed to associate scores with positions on it.

ScoringTarget can either

  - contain a a target spec + scoring options
  - contain target name (string) + scoring options; target spec will be resolved
    by lookup in builtInTargets

-}
type ScoringTarget
    = BuiltIn TargetName ScoringOptions
    | Custom Target.TargetSpec ScoringOptions


type alias TargetName =
    String


tenRingScoreTarget =
    BuiltIn "Ten Ring" { lineBreak = Target.up, inner10s = False, innerXs = True }


{-| The map of built in targets specifications (as opposed to custom taret)
-}
builtInTargets : Dict TargetName Target.TargetSpec
builtInTargets =
    let
        baseList =
            Dict.fromList
                [ ( "Ten Ring", tenRingTarget.spec )
                , ( "None", [] )
                ]

        default =
            case Dict.get "Ten Ring" baseList of
                Just value ->
                    value

                Maybe.Nothing ->
                    []

        targetListWithDefault =
            Dict.insert "Default" default baseList
    in
    targetListWithDefault



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
        , selectedEnds : RecordCard
        , shotSelection : RecordSelection
        , dragInProgresss : Bool
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
              , Dict.fromList <|
                    List.map2
                        Tuple.pair
                        (List.range 1 3)
                        [ ShotRecord (Score "10" 10) (Shot arrowSpec { x = 0, y = 0 }) tenRingScoreTarget
                        , ShotRecord (Score "9" 9) (Shot arrowSpec { x = 5, y = 4.8 }) tenRingScoreTarget
                        , ShotRecord (Score "9" 9) (Shot arrowSpec { x = 3, y = 4 }) tenRingScoreTarget
                        ]
              )
            , ( 2
              , Dict.fromList <|
                    List.map2
                        Tuple.pair
                        (List.range 1 3)
                        [ ShotRecord (Score "X" 10) (Shot arrowSpec { x = 0.3, y = 0.2 }) tenRingScoreTarget
                        , ShotRecord (Score "9" 9) (Shot arrowSpec { x = -4.0, y = 3.8 }) tenRingScoreTarget
                        , ShotRecord (Score "9" 9) (Shot arrowSpec { x = -2.1, y = -4.2 }) tenRingScoreTarget
                        ]
              )
            , ( 3
              , Dict.fromList <|
                    List.map2
                        Tuple.pair
                        (List.range 1 3)
                        [ ScoreRecord (Score "X" 10)
                        , ScoreRecord (Score "10" 10)
                        , ScoreRecord (Score "9" 9)
                        ]
              )
            , ( 4
              , Dict.fromList <|
                    List.map2
                        Tuple.pair
                        (List.range 1 3)
                        [ ScoreRecord (Score "10" 10)
                        , ScoreRecord (Score "10" 10)
                        , ScoreRecord (Score "8" 8)
                        ]
              )
            , ( 5
              , Dict.fromList <|
                    List.map2
                        Tuple.pair
                        (List.range 1 3)
                        [ ScoreRecord (Score "9" 10)
                        , EmptyRecord
                        , EmptyRecord
                        ]
              )
            , ( 6
              , Dict.fromList <|
                    List.map2
                        Tuple.pair
                        (List.range 1 3)
                        [ EmptyRecord
                        , EmptyRecord
                        , EmptyRecord
                        ]
              )
            ]
        )
        (Set.fromList [ 1 ])
        (Just ( 1, 2 ))
        False



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize x y ->
            ( model, getViewport targetLabel )

        ViewportResult viewport ->
            ( { model | viewport = viewport }, Cmd.none )

        SelectEnd index ->
            ( { model | selectedEnds = Set.insert index model.selectedEnds }, Cmd.none )

        DeselectEnd index ->
            ( { model | selectedEnds = Set.remove index model.selectedEnds }, Cmd.none )

        SelectShot selection ->
            ( { model | selectedShot = selection }, Cmd.none )

        ArrowDragStart recordId ->
            ( { model
                | dragInProgresss = True
                , selectedShot = Just recordId
              }
            , getViewport targetLabel
            )

        ArrowDragMove recordId buttons pos ->
            ( { model
                | dragInProgresss = True
                , scorecard =
                    updateSelectedArrowPos model.scorecard
                        recordId
                        (Target.translateClientToSvgCoordinates
                            Target.tenRingTarget.viewBox
                            model.viewport
                            pos
                        )
              }
            , Cmd.none
            )

        ArrowDragEnd recordId ->
            ( { model
                | dragInProgresss = False
                , selectedShot = Maybe.Nothing
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


scoreShotAtPosition : Target.TargetSpec -> ScoringOptions -> Shot -> Score
scoreShotAtPosition target scoringOptions shot =
    Target.scorePos target scoringOptions shot


updateArrowPos : EndRecord -> FloatPosition -> EndRecord
updateArrowPos record pos =
    case record of
        ShotRecord score shot target ->
            let
                updatedShot =
                    { shot | pos = pos }
            in
            case target of
                BuiltIn targetName options ->
                    case Dict.get targetName builtInTargets of
                        Just targetSpec ->
                            ShotRecord
                                (scoreShotAtPosition targetSpec options updatedShot)
                                updatedShot
                                tenRingScoreTarget

                        Maybe.Nothing ->
                            record

                Custom targetSpec options ->
                    ShotRecord
                        (scoreShotAtPosition targetSpec options updatedShot)
                        updatedShot
                        tenRingScoreTarget

        _ ->
            record


updateSelectedArrowPos : Scorecard -> RecordId -> FloatPosition -> Scorecard
updateSelectedArrowPos scores selectedRecordId newPosition =
    scores
        |> Dict.update (Tuple.first selectedRecordId)
            (\maybeEnd ->
                case maybeEnd of
                    Just end ->
                        Just <|
                            Dict.update
                                (Tuple.second selectedRecordId)
                                (\maybeRecord ->
                                    case maybeRecord of
                                        Just record ->
                                            Just <| updateArrowPos record newPosition

                                        Maybe.Nothing ->
                                            maybeRecord
                                )
                                end

                    Maybe.Nothing ->
                        Maybe.Nothing
            )


getViewport : String -> Cmd Msg
getViewport elementName =
    Task.attempt
        (\result ->
            case result of
                Ok value ->
                    ViewportResult (Just value)

                Err error ->
                    ViewportResult Maybe.Nothing
        )
        (Dom.getElement elementName)


dictToNestedList keyMapper dictionary =
    dictionary
        |> Dict.map (\k v -> v |> Dict.mapKeys (keyMapper k) >> Dict.toList)
        |> Dict.values


mapDictFlatBy : (comparable1 -> comparable2 -> comparable3) -> Dict comparable1 (Dict comparable2 v) -> List ( comparable3, v )
mapDictFlatBy keyMapper =
    dictToNestedList keyMapper >> List.concat


flattenDict : Dict comparable (Dict comparable1 c) -> Dict ( comparable, comparable1 ) c
flattenDict =
    flattenDictToList >> Dict.fromList


flattenDictToList : Dict comparable (Dict comparable1 c) -> List ( ( comparable, comparable1 ), c )
flattenDictToList dictionary =
    mapDictFlatBy Tuple.pair dictionary



-- Subscriptions


subscriptions model =
    Sub.batch [ onResize WindowResize ]



--  View


view : Model -> Html Msg
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
            Tuple.mapBoth flattenDict flattenDict <|
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
                    Dict.get selection selectedEnds
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
        model.dragInProgresss


scorecard : ViewModel -> Html Msg
scorecard model =
    let
        targetView =
            case (not << Dict.isEmpty) model.selectedEnds of
                True ->
                    [ targetElement
                        { selectedEnds = model.selectedEnds
                        , viewsize = model.viewsize
                        , shotSelection = model.selectedRecord
                        , dragInProgresss = model.dragInProgresss
                        }
                    ]

                False ->
                    []
    in
    Element.layout []
        (Element.column
            [ Element.spacing 1
            , Element.width model.viewsize
            ]
            (List.concat
                [ renderSelectedEnds model.selectedEnds model.selectedRecord
                , targetView
                , targetScorecard model.unselectedEnds
                ]
            )
        )


viewportSize : Maybe Dom.Element -> Element.Length
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


excludeSelectedShot : RecordSelection -> RecordCard -> RecordCard
excludeSelectedShot selectedShot ends =
    case selectedShot of
        Nothing ->
            ends

        Selection ( recordId, _ ) ->
            Dict.remove recordId ends


targetElement : TargetData r -> Element Msg
targetElement { viewsize, selectedEnds, shotSelection, dragInProgresss } =
    let
        selectedRecordId =
            case shotSelection of
                Selection id ->
                    Tuple.first id

                Nothing ->
                    ( 0, 0 )

        eventAttr =
            case dragInProgresss of
                True ->
                    [ SvgEvents.on "mousemove"
                        (Decode.map2
                            (ArrowDragMove selectedRecordId)
                            (Decode.field "buttons" Decode.int)
                            (Decode.map2
                                IntPosition
                                (Decode.field "clientX" Decode.int)
                                (Decode.field "clientY" Decode.int)
                            )
                        )
                    , SvgEvents.onMouseUp <| ArrowDragEnd selectedRecordId
                    ]

                False ->
                    []

        endShots =
            excludeSelectedShot shotSelection selectedEnds

        shotList =
            Dict.toList endShots
    in
    svg
        (List.append
            [ SvgAttr.version "1.1"
            , SvgAttr.width "100%"
            , SvgAttr.height "100%"
            , SvgAttr.viewBox <|
                Target.viewBoxToAttributeString tenRingTarget.viewBox
            , SvgAttr.id targetLabel
            ]
            eventAttr
        )
        [ tenRingTarget.view
        , renderShots shotList shotSelection dragInProgresss
        ]
        |> Element.html
        |> Element.el
            [ Element.height viewsize
            , Element.width viewsize
            , Element.color <| rgba255 150 200 200 0.8
            ]


shotFromRecordSelection : RecordSelection -> Maybe ShotSelection
shotFromRecordSelection recordSelection =
    case recordSelection of
        Selection ( recordId, record ) ->
            case record of
                ShotRecord _ shot _ ->
                    Just <| Selection ( recordId, shot )

                _ ->
                    Maybe.Nothing

        _ ->
            Maybe.Nothing


onMouseDownMove : Decode.Decoder msg -> Svg.Attribute msg
onMouseDownMove decoder =
    SvgEvents.on "mousemove"
        (Decode.field "buttons" Decode.int
            |> Decode.andThen
                (\buttons ->
                    case buttons > 0 of
                        True ->
                            decoder

                        False ->
                            Decode.fail "Mouse buttons not pressed"
                )
        )


renderShots : RecordList -> RecordSelection -> Bool -> Svg Msg
renderShots shots recordSelection dragInProgresss =
    let
        selectedRecordId =
            case recordSelection of
                Selection id ->
                    Tuple.first id

                Nothing ->
                    ( 0, 0 )

        selectedArrowEvents =
            case dragInProgresss of
                False ->
                    [ SvgEvents.onClick <| SelectShot Maybe.Nothing
                    , onMouseDownMove <| Decode.succeed (ArrowDragStart selectedRecordId)
                    ]

                True ->
                    []

        selectionArrow =
            case shotFromRecordSelection recordSelection of
                Just selection ->
                    case selection of
                        Selection ( _, shot ) ->
                            [ selectedArrow
                                shot
                                selectedArrowEvents
                            ]

                        Nothing ->
                            []

                _ ->
                    []
    in
    Svg.g [] <|
        List.append
            (List.map
                (\( recordId, record ) ->
                    case record of
                        ShotRecord score shot _ ->
                            arrow
                                shot
                                [ SvgEvents.onMouseDown <| SelectShot <| Just recordId
                                ]

                        _ ->
                            Svg.g [] []
                )
                shots
            )
            selectionArrow


targetScorecard : RecordCard -> List (Element Msg)
targetScorecard ends =
    ends
        |> groupByEnd
        |> List.map renderUnselectedEnd


groupByEnd : RecordCard -> List RecordList
groupByEnd ends =
    ends
        |> Dict.toList
        |> Dict.groupBy (Tuple.first >> Tuple.first)
        |> Dict.toList
        |> List.map Tuple.second



-- Scoring End Views


renderSelectedEnds : RecordCard -> RecordSelection -> List (Element Msg)
renderSelectedEnds selectedEnds record =
    selectedEnds
        |> groupByEnd
        |> List.map renderSelectedEnd


baseEndStyle : EndStyle {}
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


selectedEndStyle : EndId -> EndStyle {}
selectedEndStyle endIndex =
    { baseEndStyle
        | row =
            List.append
                baseEndStyle.row
                [ Element.color <| rgba255 70 50 230 0.8 ]
        , id =
            List.append
                baseEndStyle.id
                [ Font.color <| rgb255 255 255 255
                , Element.onClick <| DeselectEnd endIndex
                ]
        , score =
            List.append
                baseEndStyle.score
                [ Font.color <| rgb255 255 255 255
                ]
    }


unselectedEndStyle : EndId -> EndStyle {}
unselectedEndStyle index =
    { baseEndStyle
        | id =
            List.append
                baseEndStyle.id
                [ Element.onClick <| SelectEnd index ]
        , row =
            List.append
                baseEndStyle.row
                [ Element.color <| rgba255 150 200 200 0.8 ]
    }


renderSelectedEnd : RecordList -> Element Msg
renderSelectedEnd end =
    let
        -- TODO improve
        endId =
            List.head end |> Maybe.andThen (Just << Tuple.first << Tuple.first) |> Maybe.withDefault 0
    in
    renderScoringEnd (selectedEndStyle endId) end


renderUnselectedEnd : RecordList -> Element Msg
renderUnselectedEnd end =
    let
        -- TODO improve
        endId =
            List.head end |> Maybe.andThen (Just << Tuple.first << Tuple.first) |> Maybe.withDefault 0
    in
    renderScoringEnd (unselectedEndStyle endId) end


renderScoringEnd : EndStyle r -> RecordList -> Element Msg
renderScoringEnd endStyle end =
    let
        -- TODO improve
        endId =
            List.head end |> Maybe.andThen (Just << Tuple.first << Tuple.first) |> Maybe.withDefault 0
    in
    endNumber endStyle.id endId
        :: renderScores endStyle.score end
        |> Element.row endStyle.row


endNumber : List (Element.Attribute Msg) -> Int -> Element Msg
endNumber style num =
    "#"
        ++ String.fromInt num
        |> text
        |> Element.el style


renderScores : List (Element.Attribute Msg) -> RecordList -> List (Element Msg)
renderScores style end =
    List.map (renderScore style) end


renderScore : List (Element.Attribute Msg) -> ( RecordId, EndRecord ) -> Element Msg
renderScore style ( recordId, record ) =
    case record of
        EmptyRecord ->
            Element.el style <| text "-"

        ShotRecord score _ _ ->
            Element.el
                ((Element.onClick <| SelectShot (Just recordId)) :: style)
                (text score.label)

        ScoreRecord score ->
            Element.el style (text score.label)
