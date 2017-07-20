module Main exposing (..)

-- elm-lang

import Html exposing (..)
import Html.Attributes


-- user

import Messages exposing (..)
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
import Debug


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
    , stagedMessageAwaitingBoundingBox : Maybe (BoundingBox -> Msg)
    }


initialModel : Model
initialModel =
    { currentEndControls = initialControlData
    , end = initialEnd 3
    , stagedMessageAwaitingBoundingBox = Nothing
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
    case (Debug.log "Message=" model.stagedMessageAwaitingBoundingBox) of
        Just stagedMessage ->
            Sub.batch
                [ Ports.boundingBoxResult (BoundingBoxResult (Just stagedMessage))
                ]

        Nothing ->
            Sub.batch
                [ Ports.boundingBoxResult (BoundingBoxResult Nothing)
                ]



-- Update


placeArrowOnShotPlacer : Model -> IntPosition -> BoundingBox -> Model
placeArrowOnShotPlacer model mousePosition boundingBox =
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
            , stagedMessageAwaitingBoundingBox = Nothing
        }


setArrowDragStarted : CurrentEndControlData -> CurrentEndControlData
setArrowDragStarted controls =
    { controls | dragInProgress = True }


setArrowDragStartedWithBox : CurrentEndControlData -> BoundingBox -> CurrentEndControlData
setArrowDragStartedWithBox controls boundingBox =
    { controls | dragInProgress = True, boundingBox = boundingBox }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Update=" msg of
        Messages.NoOp ->
            ( model, Cmd.none )

        Messages.ArrowDragPotentialStart buttons mousePosition boundingBox ->
            case buttons > 0 of
                True ->
                    ( placeArrowOnShotPlacer
                        { model
                            | currentEndControls =
                                setArrowDragStartedWithBox
                                    model.currentEndControls
                                    boundingBox
                            , stagedMessageAwaitingBoundingBox = Nothing
                        }
                        mousePosition
                        boundingBox
                    , Cmd.none
                    )

                False ->
                    ( model, Cmd.none )

        Messages.ArrowDrag mousePosition ->
            ( placeArrowOnShotPlacer model mousePosition model.currentEndControls.boundingBox, Cmd.none )

        Messages.ArrowDragEnd mousePosition boundingBox ->
            ( model, Cmd.none )

        Messages.SelectArrow index ->
            ( { model | currentEndControls = selectArrowIndex model.end.endEntries model.currentEndControls (Just index) }, Cmd.none )

        Messages.DeselectArrow ->
            ( { model | currentEndControls = selectArrowIndex model.end.endEntries model.currentEndControls Nothing }, Cmd.none )

        Messages.PlaceArrow mousePosition boundingBox ->
            ( placeArrowOnShotPlacer model mousePosition boundingBox, Cmd.none )

        Messages.StageMsgPartial messagePartial ->
            ( { model
                | stagedMessageAwaitingBoundingBox = Just (messagePartial)
              }
            , (Ports.getClientBoundingBox "TargetSvg")
            )

        Messages.BoundingBoxResult maybeStagedMessage boundingBox ->
            let
                resultMessage =
                    case maybeStagedMessage of
                        Just messagePartial ->
                            messagePartial boundingBox

                        Nothing ->
                            case model.stagedMessageAwaitingBoundingBox of
                                Just messagePartial ->
                                    messagePartial boundingBox

                                Nothing ->
                                    NoOp
            in
                update resultMessage { model | stagedMessageAwaitingBoundingBox = Nothing }
