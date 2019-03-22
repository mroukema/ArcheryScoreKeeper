module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onResize)
import Element exposing (fill, height, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html
import Svg exposing (svg)
import Svg.Attributes as Attr
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
    let
        size =
            Element.px <| min (Tuple.first model) (Tuple.second model)
    in
    svg
        [ Attr.version "1.1"
        , Attr.width "100%"
        , Attr.height "100%"
        , Attr.viewBox (Target.viewBoxToAttributeString tenRingTarget.viewBox)
        , Attr.id "TargetSvg"
        ]
        [ tenRingTarget.view ]
        |> Element.html
        |> Element.el [ height size, width size ]
        |> Element.layout []
