module Main exposing (main)

import Arrow exposing (arrow)
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


type alias Model =
    { viewport : Maybe Dom.Viewport
    , scorecard : Scorecard
    , selectedEnds : Set Int
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
    scores
        |> List.map2 Tuple.pair scoreIndices
        |> Dict.fromList



-- Subscriptions


subscriptions model =
    Sub.batch [ onResize WindowResize ]



--  View


view : Model -> Html Msg
view model =
    let
        ( selectedEnds, unselectedEnds ) =
            Dict.partition
                (\endIndex _ ->
                    List.any
                        (\selectedIndex -> endIndex == selectedIndex)
                        (Set.toList model.selectedEnds)
                )
                model.scorecard
    in
    Element.layout
        [ Element.width <| viewportSize model.viewport ]
        (Element.column
            [ Element.spacing 1

            --, Element.padding 6
            ]
            (List.concat <|
                [ renderSelectedEnds (Dict.toList selectedEnds)
                , [ targetElement model.viewport selectedEnds ]
                , targetScorecard unselectedEnds
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


targetElement : Maybe Dom.Viewport -> Scorecard -> Element msg
targetElement viewport selectedEnds =
    let
        size =
            viewportSize viewport
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
        , renderShots (shotsFromEnds <| Dict.values selectedEnds)
        ]
        |> Element.html
        |> Element.el
            [ Element.height size
            , Element.width size
            , Background.color <| rgba255 150 200 200 0.8
            ]


shotsFromEnd : End -> List Shot
shotsFromEnd end =
    end
        |> Dict.toList
        |> List.filterMap (\( _, record ) -> record.shot)


shotsFromEnds : List End -> List Shot
shotsFromEnds ends =
    List.map shotsFromEnd ends |> List.concat


renderShots : List Shot -> Svg msg
renderShots shots =
    Svg.g [] <| List.map arrow shots


targetScorecard ends =
    ends
        |> Dict.toList
        |> List.map renderUnselectedEnd



-- Scoring End Views


renderSelectedEnds selectedEnds =
    List.map
        renderSelectedEnd
        selectedEnds


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


selectedEndStyle index =
    { baseEndStyle
        | row =
            List.append
                baseEndStyle.row
                [ Background.color <| rgba255 70 50 230 0.8 ]
        , id =
            List.append
                baseEndStyle.id
                [ Font.color <| Element.rgb255 255 255 255
                , Events.onClick <| DeselectEnd index
                ]
        , score =
            List.append baseEndStyle.score
                [ Font.color <| Element.rgb255 255 255 255 ]
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
renderSelectedEnd (( index, _ ) as end) =
    renderScoringEnd (selectedEndStyle index) end


renderUnselectedEnd : ( Int, End ) -> Element Msg
renderUnselectedEnd (( index, _ ) as end) =
    renderScoringEnd (unselectedEndStyle index) end


renderScoringEnd : EndStyle r -> ( Int, End ) -> Element Msg
renderScoringEnd endStyle ( index, scores ) =
    endNumber endStyle.id index
        :: renderScores endStyle.score scores
        |> Element.row
            endStyle.row


endNumber : List (Element.Attribute Msg) -> Int -> Element Msg
endNumber style num =
    "#"
        ++ String.fromInt num
        |> text
        |> Element.el style


renderScores : List (Element.Attribute Msg) -> End -> List (Element Msg)
renderScores style scores =
    scores
        |> Dict.toList
        |> List.map
            (\( _, record ) ->
                Element.el
                    style
                    (text record.score.label)
            )
