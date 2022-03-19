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
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as Decode
import Score exposing (Score)
import Set exposing (Set)
import Shot exposing (FloatPosition, Shot)
import String exposing (fromFloat)
import Svg exposing (Svg, svg)
import Svg.Attributes as SvgAttr
import Svg.Events as SvgEvents
import Target exposing (ScoreTarget, tenRingTarget)
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
    | AddShot IntPosition
    | AddEnd


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
    = BuiltIn TargetName
    | Custom ScoreTarget


type alias TargetName =
    String


tenRingBuiltin =
    BuiltIn "Ten Ring"


{-| The map of built in targets specifications (as opposed to custom taret)
-}
builtInTargets : Dict TargetName ScoreTarget
builtInTargets =
    Dict.fromList
        [ ( "Ten Ring", Target.tenRingScoreTarget )
        ]



-- Record Specs


type alias EndStyle r =
    { r
        | row : List (Element.Attribute Msg)
        , id : List (Element.Attribute Msg)
        , score : List (Element.Attribute Msg)
        , total : List (Element.Attribute Msg)
        , selectedRecord : List (Element.Attribute Msg)
    }


type alias TargetData r =
    { r
        | viewsize : Element.Length
        , selectedEnds : RecordCard
        , shotSelection : RecordSelection
        , dragInProgresss : Bool
    }


emptyShotRecord =
    ScoreRecord <| Score "-" 0


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
                        (List.range 1 4)
                        [ ShotRecord (Score "X" 10) (Shot arrowSpec { x = 0, y = 0 }) tenRingBuiltin
                        , ShotRecord (Score "10" 10) (Shot arrowSpec { x = 1, y = 1.4 }) tenRingBuiltin
                        , ShotRecord (Score "9" 9) (Shot arrowSpec { x = 3, y = 4 }) tenRingBuiltin
                        ]
              )
            , ( 2
              , Dict.fromList <|
                    List.map2
                        Tuple.pair
                        (List.range 1 3)
                        [ ShotRecord (Score "X" 10) (Shot arrowSpec { x = 0.3, y = 0.2 }) tenRingBuiltin
                        , ShotRecord (Score "9" 9) (Shot arrowSpec { x = -4.0, y = 3.8 }) tenRingBuiltin
                        , ShotRecord (Score "9" 9) (Shot arrowSpec { x = -2.1, y = -4.2 }) tenRingBuiltin
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
                        [ ScoreRecord (Score "9" 9)
                        , emptyShotRecord
                        , emptyShotRecord
                        ]
              )
            , ( 6
              , Dict.fromList <|
                    List.map2
                        Tuple.pair
                        (List.range 1 3)
                        [ emptyShotRecord
                        , emptyShotRecord
                        , emptyShotRecord
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

        AddShot pos ->
            ( { model
                | scorecard =
                    addShot model.scorecard
                        model.selectedShot
                        (Target.translateClientToSvgCoordinates
                            Target.tenRingTarget.viewBox
                            model.viewport
                            pos
                        )
              }
            , Cmd.none
            )

        AddEnd ->
            ( { model | scorecard = addEnd model.scorecard }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


emptyEnd =
    Dict.fromList <|
        List.map2
            Tuple.pair
            (List.range 1 3)
            [ emptyShotRecord
            , emptyShotRecord
            , emptyShotRecord
            ]



--addEnd : Scorecard -> Scorecard


addEnd scores =
    let
        nextEndNum =
            1 + Dict.size scores
    in
    Dict.insert nextEndNum emptyEnd scores


addShot scores maybeSelection pos =
    case maybeSelection of
        Maybe.Nothing ->
            scores

        Just selection ->
            Dict.insert
                (Tuple.first selection)
                (Dict.insert
                    (Tuple.second
                        selection
                    )
                    (updateArrowPos
                        (ShotRecord
                            (Score "-" 0)
                            (Shot Arrow.defaultArrow pos)
                            tenRingBuiltin
                        )
                        pos
                    )
                    (Dict.get
                        (Tuple.first selection)
                        scores
                        |> Maybe.withDefault Dict.empty
                    )
                )
                scores


scoreShotAtPosition : ScoreTarget -> Shot -> Score
scoreShotAtPosition scoreTarget shot =
    Target.scorePos scoreTarget shot


updateArrowPos : EndRecord -> FloatPosition -> EndRecord
updateArrowPos record pos =
    case record of
        ShotRecord score shot target ->
            let
                updatedShot =
                    { shot | pos = pos }
            in
            case target of
                BuiltIn targetName ->
                    case Dict.get targetName builtInTargets of
                        Just scoreTarget ->
                            ShotRecord
                                (Target.scorePos scoreTarget updatedShot)
                                updatedShot
                                tenRingBuiltin

                        Maybe.Nothing ->
                            record

                Custom scoreTarget ->
                    ShotRecord
                        (scoreShotAtPosition scoreTarget updatedShot)
                        updatedShot
                        tenRingBuiltin

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
                    [ renderEndlistTotal model.selectedEnds model.viewsize
                    , targetElement
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
                , [ scorecardTotal <| sumRecords <| Dict.union model.selectedEnds model.unselectedEnds ]
                , [ addNewEnd ]
                ]
            )
        )


addNewEnd =
    Element.row
        utilityEndStyle.row
        [ Element.el
            [ Element.centerX
            , Element.onClick AddEnd
            , Element.padding 6
            ]
            (Element.text "New End")
        ]


sumRecords records =
    records
        |> Dict.toList
        |> List.map (Tuple.second >> toRecordValue)
        |> List.sum


renderEndlistTotal selectedEnds viewsize =
    let
        total =
            sumRecords selectedEnds
    in
    Element.el
        [ Element.width viewsize
        , Element.below
            (Element.el
                [ Element.alignRight
                , Element.paddingEach { top = 6, right = 12, bottom = 6, left = 0 }
                , Font.color <| rgb255 0 0 0
                ]
                (Element.text <| String.fromInt total)
            )
        ]
        (Element.text <| "")


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

        mouseMoveEventAttr =
            case dragInProgresss of
                -- True ->
                --     [ SvgEvents.on "mousemove"
                --         (Decode.map2
                --             (ArrowDragMove selectedRecordId)
                --             (Decode.field "buttons" Decode.int)
                --             (Decode.map2
                --                 IntPosition
                --                 (Decode.field "clientX" Decode.int)
                --                 (Decode.field "clientY" Decode.int)
                --             )
                --         )
                --     , SvgEvents.onMouseUp <| ArrowDragEnd selectedRecordId
                --     ]
                True ->
                    [ Pointer.onMove
                        (\event ->
                            ArrowDragMove selectedRecordId
                                1
                                (IntPosition
                                    (round <| Tuple.first event.pointer.clientPos)
                                    (round <| Tuple.second event.pointer.clientPos)
                                )
                        )
                    , Pointer.onUp <| always <| ArrowDragEnd selectedRecordId
                    ]

                False ->
                    []

        -- When a record without shot info is selected can click on target to add shot
        eventAttr =
            case shotSelection of
                Nothing ->
                    mouseMoveEventAttr

                Selection ( _, record ) ->
                    case record of
                        ScoreRecord _ ->
                            [ Pointer.onUp <|
                                \event ->
                                    AddShot <|
                                        IntPosition
                                            (round <| Tuple.first event.pointer.clientPos)
                                            (round <| Tuple.second event.pointer.clientPos)
                            ]
                                ++ mouseMoveEventAttr

                        ShotRecord _ _ _ ->
                            mouseMoveEventAttr

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


onPointerDownMove : Msg -> Html.Attribute Msg
onPointerDownMove decoder =
    Pointer.onMove
        (\event ->
            case event.pointerType of
                Pointer.MouseType ->
                    case Debug.log "Button" event.pointer.button of
                        Mouse.MainButton ->
                            decoder

                        _ ->
                            NoOp

                _ ->
                    decoder
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
                    [ Pointer.onDown <| always <| SelectShot Maybe.Nothing
                    , onPointerDownMove <| ArrowDragStart selectedRecordId
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
                                [ Pointer.onDown <| always <| SelectShot <| Just recordId
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
        |> List.map (renderSelectedEnd <| record)


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
        [ Element.width <| px 30
        , Element.centerX
        , Font.center
        ]
    , total =
        [ Element.alignRight
        , Element.paddingEach { top = 0, right = 12, bottom = 0, left = 0 }
        , Font.center
        ]
    , selectedRecord =
        [ Element.spacing 12
        , Element.width <| px 30
        , Element.spaceEvenly
        , Element.centerX
        , Font.color <| rgba255 70 50 230 0.8
        , Element.color <| rgba255 143 224 255 0.5
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
        , total =
            List.append
                baseEndStyle.total
                [ Font.color <| rgb255 255 255 255
                ]
    }


unselectedEndStyle : EndId -> EndStyle {}
unselectedEndStyle index =
    { baseEndStyle
        | id =
            baseEndStyle.id
                ++ [ Element.onClick <| SelectEnd index ]
        , row =
            baseEndStyle.row
                ++ [ Element.color <| rgba255 150 200 200 0.8 ]
    }


utilityEndStyle : EndStyle {}
utilityEndStyle =
    { baseEndStyle
        | row = baseEndStyle.row ++ [ Element.color <| rgba255 150 200 200 0.8 ]
    }


renderSelectedEnd : RecordSelection -> RecordList -> Element Msg
renderSelectedEnd selectedId end =
    let
        -- TODO improve
        endId =
            List.head end
                |> Maybe.andThen (Just << Tuple.first << Tuple.first)
                |> Maybe.withDefault 0
    in
    renderScoringEnd (selectedEndStyle endId) selectedId end


renderUnselectedEnd : RecordList -> Element Msg
renderUnselectedEnd end =
    let
        -- TODO improve
        endId =
            List.head end
                |> Maybe.andThen (Just << Tuple.first << Tuple.first)
                |> Maybe.withDefault 0
    in
    renderScoringEnd (unselectedEndStyle endId) Nothing end


renderScoringEnd : EndStyle r -> RecordSelection -> RecordList -> Element Msg
renderScoringEnd endStyle scoreSelection end =
    let
        -- TODO improve
        endId =
            List.head end
                |> Maybe.andThen (Just << Tuple.first << Tuple.first)
                |> Maybe.withDefault 0
    in
    List.concat
        [ [ endNumber endStyle.id endId ]
        , renderScores endStyle end scoreSelection
        , [ endTotal endStyle.total end ]
        ]
        |> Element.row endStyle.row


endNumber : List (Element.Attribute Msg) -> Int -> Element Msg
endNumber style num =
    "#"
        ++ String.fromInt num
        |> text
        |> Element.el style


toRecordValue record =
    case record of
        ScoreRecord score ->
            score.value

        ShotRecord score _ _ ->
            score.value


scorecardTotal : Int -> Element Msg
scorecardTotal total =
    Element.row utilityEndStyle.row
        [ Element.el (utilityEndStyle.id ++ [ Element.alignLeft ]) (Element.text "Total")
        , Element.el utilityEndStyle.total (Element.text <| String.fromInt total)
        ]


endTotal : List (Element.Attribute msg) -> RecordList -> Element msg
endTotal style end =
    end
        |> List.map (Tuple.second >> toRecordValue)
        |> List.sum
        |> String.fromInt
        |> Element.text
        |> Element.el style


renderScores : EndStyle r -> RecordList -> RecordSelection -> List (Element Msg)
renderScores styles end scoreSelection =
    List.map
        (\(( scoreId, _ ) as score) ->
            case scoreSelection of
                Nothing ->
                    renderScore styles.score score

                Selection ( selectionId, _ ) ->
                    case scoreId == selectionId of
                        True ->
                            renderSelectedScore styles.selectedRecord score

                        False ->
                            renderScore styles.score score
        )
        end


renderPosInfo pos =
    Element.column
        [ height (33 |> px)
        , Font.size 10
        , Font.alignLeft
        , Element.paddingXY 6 0
        , Element.spacing 2
        , Font.color <| rgb255 190 190 190
        ]
        [ Element.text <| "x: " ++ (String.left 4 <| String.fromFloat pos.x)
        , Element.text <| "y: " ++ (String.left 4 <| String.fromFloat pos.y)
        ]


renderSelectedScore : List (Element.Attribute Msg) -> ( RecordId, EndRecord ) -> Element Msg
renderSelectedScore style ( recordId, record ) =
    case record of
        ShotRecord score shot _ ->
            Element.el [ Element.width fill ] <|
                Element.el
                    ((Element.onClick <| SelectShot Maybe.Nothing) :: style)
                    (Element.el
                        [ Element.centerX
                        , Element.onRight (renderPosInfo shot.pos)
                        ]
                        (text score.label)
                    )

        ScoreRecord score ->
            Element.el [ Element.width fill ] <|
                Element.el
                    ((Element.onClick <| SelectShot Maybe.Nothing) :: style)
                    (text score.label)


renderScore : List (Element.Attribute Msg) -> ( RecordId, EndRecord ) -> Element Msg
renderScore style ( recordId, record ) =
    case record of
        ShotRecord score shot _ ->
            Element.el [ Element.width fill ] <|
                Element.el
                    ((Element.onClick <| SelectShot (Just recordId)) :: style)
                    (Element.el
                        [ Element.centerX
                        , Element.onRight (renderPosInfo shot.pos)
                        ]
                        (text score.label)
                    )

        ScoreRecord score ->
            Element.el [ Element.width fill ] <|
                Element.el
                    ((Element.onClick <| SelectShot (Just recordId)) :: style)
                    (text score.label)