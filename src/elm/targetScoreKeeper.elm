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
    Sub.batch
        [ Ports.boundingBoxResult Messages.BoundingBoxResult
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
        }


setArrowDragStarted : CurrentEndControlData -> CurrentEndControlData
setArrowDragStarted controls =
    { controls | dragInProgress = True }


setArrowDragStartedWithBox : CurrentEndControlData -> BoundingBox -> CurrentEndControlData
setArrowDragStartedWithBox controls boundingBox =
    { controls | dragInProgress = True, boundingBox = boundingBox }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Messages.NoOp ->
            ( model, Cmd.none )

        Messages.ArrowDragPotentialStart buttons mousePosition ->
            case buttons > 0 of
                True ->
                    ( { model
                        | stagedMessageAwaitingBoundingBox = Just (ArrowDragStart mousePosition)
                        , currentEndControls = setArrowDragStarted model.currentEndControls
                      }
                    , (Ports.getClientBoundingBox "TargetSvg")
                    )

                False ->
                    ( model, Cmd.none )

        Messages.ArrowDragStart mousePosition boundingBox ->
            ( placeArrowOnShotPlacer
                { model
                    | currentEndControls =
                        setArrowDragStartedWithBox
                            model.currentEndControls
                            boundingBox
                }
                mousePosition
                boundingBox
            , Cmd.none
            )

        Messages.ArrowDrag mousePosition ->
            ( placeArrowOnShotPlacer model mousePosition model.currentEndControls.boundingBox, Cmd.none )

        Messages.ArrowDragEnd mousePosition ->
            ( model, Cmd.none )

        Messages.SelectArrow index ->
            ( { model | currentEndControls = selectArrowIndex model.end.endEntries model.currentEndControls (Just index) }, Cmd.none )

        Messages.DeselectArrow ->
            ( { model | currentEndControls = selectArrowIndex model.end.endEntries model.currentEndControls Nothing }, Cmd.none )

        Messages.PlaceMouseCoor mousePosition ->
            ( { model
                | stagedMessageAwaitingBoundingBox = Just (PlaceArrow mousePosition)
              }
            , (Ports.getClientBoundingBox "TargetSvg")
            )

        Messages.PlaceArrow mousePosition boundingBox ->
            ( placeArrowOnShotPlacer model mousePosition boundingBox, Cmd.none )

        Messages.BoundingBoxResult boundingBox ->
            case model.stagedMessageAwaitingBoundingBox of
                Just msgPartial ->
                    update
                        (msgPartial boundingBox)
                        { model | stagedMessageAwaitingBoundingBox = Nothing }

                Nothing ->
                    ( model, Cmd.none )
