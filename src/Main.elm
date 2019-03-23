module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Dict exposing (Dict)
import Element exposing (Element, fill, height, px, rgba255, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Svg exposing (svg)
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


type alias Model =
    { viewport : Maybe Dom.Viewport
    , scorecard : Scorecard
    }


initialModel : Model
initialModel =
    Model
        Nothing
        (Dict.fromList
            [ ( 1, endFromScores [ Score "10" 10, Score "9" 9, Score "9" 9 ] )
            , ( 2, endFromScores [ Score "X" 10, Score "9" 9, Score "9" 9 ] )
            , ( 3, endFromScores [ Score "X" 10, Score "10" 10, Score "9" 9 ] )
            , ( 4, endFromScores [ Score "10" 10, Score "10" 10, Score "8" 8 ] )
            , ( 5, endFromScores [ Score "9" 10, Score "9" 10, Score "9" 8 ] )
            , ( 6, endFromScores [ Score "X" 10, Score "10" 10, Score "9" 8 ] )
            ]
        )


type alias Score =
    { label : String
    , value : Int
    }


type alias Scorecard =
    Dict Int End


type alias End =
    Dict Int Score



-- Update


update msg model =
    case msg of
        WindowResize x y ->
            ( model, getViewport )

        ViewportResult viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


getViewport =
    Task.perform ViewportResult Dom.getViewport


endFromScores : List Score -> End
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
    Html.div []
        [ Html.div
            [ HtmlAttr.style "position" "absolute" ]
            [ Element.layout [] <| targetElement model ]
        , Html.div
            [ HtmlAttr.style "position" "absolute" ]
            [ Element.layout [] <| targetScorecard model.scorecard ]
        ]


targetElement : Model -> Element msg
targetElement model =
    let
        size =
            case model.viewport of
                Just value ->
                    let
                        viewport =
                            value.viewport
                    in
                    min (floor viewport.width) (floor viewport.height) |> px

                Nothing ->
                    0 |> px
    in
    svg
        [ SvgAttr.version "1.1"
        , SvgAttr.width "100%"
        , SvgAttr.height "100%"
        , SvgAttr.viewBox <| Target.viewBoxToAttributeString tenRingTarget.viewBox
        , SvgAttr.id "TargetSvg"
        ]
        [ tenRingTarget.view
        ]
        |> Element.html
        |> Element.el [ height size, width size ]


targetScorecard : Scorecard -> Element msg
targetScorecard ends =
    ends
        |> Dict.toList
        |> List.map renderScoringEnd
        |> Element.column [ Element.spacing 6 ]


renderScoringEnd ( index, scores ) =
    let
        endNumber =
            "#"
                ++ String.fromInt index
                |> text
                |> Element.el []

        endScores =
            scores
                |> Dict.toList
                |> List.map renderScores
    in
    endNumber
        :: endScores
        |> Element.row
            [ Background.color <| rgba255 180 212 212 0.7
            , Element.spacing 12
            ]


renderScores ( _, score ) =
    Element.el [] <| text score.label
