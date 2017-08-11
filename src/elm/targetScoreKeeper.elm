module Main exposing (..)

-- elm-lang

import Array exposing (Array)
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
        , End
        )
import Types exposing (..)
import ScoreCard


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
    , ends : Array End
    , stagedMessageAwaitingBoundingBox : Maybe (BoundingBox -> Msg)
    }


initialModel : Model
initialModel =
    { currentEndControls = initialControlData
    , ends = Array.fromList [ initialEnd 3 0 ]
    , stagedMessageAwaitingBoundingBox = Nothing
    }


init : ( Model, Cmd msg )
init =
    initialModel ! []


getCurrentEndOrNewDeafult : CurrentEndControlData -> Array End -> End
getCurrentEndOrNewDeafult currentEndControls ends =
    let
        currentEndIndex =
            currentEndControls.currentEndIndex
    in
        case (Array.get currentEndIndex ends) of
            Just end ->
                end

            Nothing ->
                initialEnd 3 currentEndIndex



-- View


view : Model -> Html Msg
view model =
    let
        currentEndIndex =
            model.currentEndControls.currentEndIndex
    in
        div
            [ Html.Attributes.class "mdc-layout-grid"
            , Html.Attributes.id "targetScoreKeeper"
            ]
            [ div
                [ Html.Attributes.class "mdc-layout-grid__inner" ]
                [ CurrentEndInputTarget.view
                    (CurrentEnd
                        (getCurrentEndOrNewDeafult
                            model.currentEndControls
                            model.ends
                        )
                        model.currentEndControls
                    )
                ]

            --, ScoreCard.view model.ends
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
                (Maybe.withDefault
                    (initialEnd 3 model.currentEndControls.currentEndIndex)
                    (Array.get model.currentEndControls.currentEndIndex model.ends)
                )
                mousePosition
                boundingBox
            )
    in
        { model
            | ends = Array.set model.currentEndControls.currentEndIndex updatedEnd model.ends
            , currentEndControls = updatedControls
        }


setArrowDragStarted : CurrentEndControlData -> CurrentEndControlData
setArrowDragStarted controls =
    { controls | dragInProgress = True }


setArrowDragStartedWithBox : CurrentEndControlData -> BoundingBox -> CurrentEndControlData
setArrowDragStartedWithBox controls boundingBox =
    { controls | dragInProgress = True, boundingBox = boundingBox }


setArrowDragEnded : CurrentEndControlData -> CurrentEndControlData
setArrowDragEnded controls =
    { controls | dragInProgress = False }


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
            ( { model | currentEndControls = setArrowDragEnded model.currentEndControls }, Cmd.none )

        Messages.SelectArrow index ->
            ( { model
                | currentEndControls =
                    selectArrowIndex
                        (.endEntries (getCurrentEndOrNewDeafult model.currentEndControls model.ends))
                        model.currentEndControls
                        (Just index)
              }
            , Cmd.none
            )

        Messages.DeselectArrow ->
            ( { model
                | currentEndControls =
                    selectArrowIndex
                        (.endEntries (getCurrentEndOrNewDeafult model.currentEndControls model.ends))
                        model.currentEndControls
                        Nothing
              }
            , Cmd.none
            )

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
