module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Element exposing (fill, height, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html
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
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getViewport )



-- Model


type Msg
    = NoOp
    | WindowResize Int Int
    | Viewport Dom.Viewport


type alias Model =
    ( Int, Int )


initialModel : Model
initialModel =
    ( 0, 0 )



-- Update


update msg model =
    case msg of
        WindowResize x y ->
            ( model, getViewport )

        Viewport viewport ->
            let
                width =
                    viewport.viewport.width

                height =
                    viewport.viewport.height
            in
            ( ( floor width, floor height ), Cmd.none )

        NoOp ->
            ( model, Cmd.none )


getViewport =
    Task.perform Viewport Dom.getViewport



-- Subscriptions


subscriptions model =
    Sub.batch [ onResize WindowResize ]



--  View


view model =
    Html.div []
        [ Html.div [ HtmlAttr.style "position" "absolute" ] [ targetView model ]
        , Html.div [ HtmlAttr.style "position" "absolute" ] [ scorecard ]
        ]


targetView model =
    let
        size =
            Element.px <| min (Tuple.first model) (Tuple.second model)
    in
    svg
        [ SvgAttr.version "1.1"
        , SvgAttr.width "100%"
        , SvgAttr.height "100%"
        , SvgAttr.viewBox <| Target.viewBoxToAttributeString tenRingTarget.viewBox
        , SvgAttr.id "TargetSvg"
        ]
        [ tenRingTarget.view ]
        |> Element.html
        |> Element.el [ height size, width size ]
        |> Element.layout []


scorecard =
    Element.column [ Element.spacing 12 ]
        [ scoringEnd "#1 :   10   8   5"
        , scoringEnd "#2 :    9   8   8"
        , scoringEnd "#3 :    9   8   8"
        , scoringEnd "#4 :   10   9   8"
        , scoringEnd "#5 :    X  10   9"
        , scoringEnd "#6 :   10   8   8"
        ]
        |> Element.layout []


scoringEnd end =
    Element.row
        [ Background.color <|
            Element.rgba255 180 212 212 0.7
        ]
        [ Element.el [] (text end) ]
