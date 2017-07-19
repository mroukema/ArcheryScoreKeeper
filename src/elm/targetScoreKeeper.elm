module Main exposing (..)

-- elm-lang

import Html exposing (..)
import Html.Attributes
import Mouse


-- user

import Messages exposing (Msg)
import Ports
import CurrentEndInputTarget
    exposing
        ( updateCurrentEnd
        , CurrentEnd
        , selectArrowIndex
        , initialEnd
        , initialControlData
        , CurrentEndControlData
        )
import Types exposing (..)


-- Main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { currentEndControls : CurrentEndControlData
    , end : CurrentEndInputTarget.End
    , stagedMousePosition : Maybe Mouse.Position
    }


initialModel : Model
initialModel =
    { currentEndControls = initialControlData
    , end = initialEnd 3
    , stagedMousePosition = Nothing
    }


init : ( Model, Cmd msg )
init =
    initialModel ! []



-- View


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.class "mdc-layout-grid"
        , Html.Attributes.id "targetScoreKeeper"
        ]
        [ div
            [ Html.Attributes.class "mdc-layout-grid__inner" ]
            [ CurrentEndInputTarget.view (CurrentEnd model.end model.currentEndControls) ]

        --, EndCard.view model.shots
        ]


debugModel : Model -> Html msg
debugModel model =
    Html.text (toString model)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.boundingBoxResult Messages.BoundingBoxResult
        ]



-- Update


placeArrowWithBoundingBoxifStaged : BoundingBox -> Model -> Model
placeArrowWithBoundingBoxifStaged boundingBox model =
    case model.stagedMousePosition of
        Just mousePosition ->
            let
                ( updatedEnd, updatedControls ) =
                    (updateCurrentEnd
                        model.currentEndControls
                        model.end
                        mousePosition
                        boundingBox
                    )
            in
                { model
                    | end = updatedEnd
                    , currentEndControls = updatedControls
                    , stagedMousePosition = Nothing
                }

        Nothing ->
            model


setArrowDragStarted controls =
    { controls | dragInProgress = True }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Messages.NoOp ->
            ( model, Cmd.none )

        Messages.ArrowDragPotentialStart buttons mousePosition ->
            case buttons > 0 of
                True ->
                    ( { model
                        | currentEndControls = setArrowDragStarted model.currentEndControls
                        , stagedMousePosition = Just mousePosition
                      }
                    , (Ports.getClientBoundingBox "TargetSvg")
                    )

                False ->
                    ( model, Cmd.none )

        Messages.ArrowDragStart mousePosition ->
            ( model, Cmd.none )

        Messages.ArrowDrag mousePosition ->
            ( { model
                | stagedMousePosition = Just mousePosition
              }
            , (Ports.getClientBoundingBox "TargetSvg")
            )

        Messages.ArrowDragEnd mousePosition ->
            ( model, Cmd.none )

        Messages.SelectArrow index ->
            ( { model | currentEndControls = selectArrowIndex model.end.endEntries model.currentEndControls (Just index) }, Cmd.none )

        Messages.DeselectArrow ->
            ( { model | currentEndControls = selectArrowIndex model.end.endEntries model.currentEndControls Nothing }, Cmd.none )

        Messages.PlaceMouseCoor mousePosition ->
            ( { model
                | stagedMousePosition = Just mousePosition
              }
            , (Ports.getClientBoundingBox "TargetSvg")
            )

        Messages.BoundingBoxResult boundingBox ->
            ( placeArrowWithBoundingBoxifStaged boundingBox model, Cmd.none )
