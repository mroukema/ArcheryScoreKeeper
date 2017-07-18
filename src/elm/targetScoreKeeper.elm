module Main exposing (..)

-- elm-lang

import Html exposing (..)
import Html.Attributes
import Mouse


-- user

import Messages exposing (Msg)
import Ports
import CurrentEndInputTarget exposing (updateCurrentEnd, CurrentEnd, initialEnd, initialControlData, CurrentEndControlData)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Messages.NoOp ->
            ( model, Cmd.none )

        Messages.ArrowDragStart index mousePos ->
            ( model, Cmd.none )

        Messages.ArrowDrag index mousePos ->
            ( model, Cmd.none )

        Messages.ArrowDragEnd index mousePos ->
            ( model, Cmd.none )

        Messages.SelectArrow index ->
            ( { model | currentEndControls = selectArrowIndex model.currentEndControls index }, Cmd.none )

        Messages.PlaceMouseCoor mousePosition ->
            ( { model
                | stagedMousePosition = Just mousePosition
              }
            , (Ports.getClientBoundingBox "TargetSvg")
            )

        Messages.BoundingBoxResult boundingBox ->
            ( placeArrowWithBoundingBoxifStaged boundingBox model, Cmd.none )


selectArrowIndex : CurrentEndControlData -> Int -> CurrentEndControlData
selectArrowIndex controls index =
    { controls | selectedArrowIndex = index }
