module ShotPlacer exposing (isSelectedArrow, offsetToPosition, placeArrowOnClick, selectedArrowLast, shotPlacer)

-- elm-lang
-- user

import Json.Decode as Decode
import Messages exposing (..)
import Shot exposing (Shot)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)



-- Model


type alias Position =
    { x : Int
    , y : Int
    }



-- View


shotPlacer : List ( Int, Shot ) -> Maybe Int -> Bool -> Svg Msg
shotPlacer model selectedArrowIndex arrowDragInProgress =
    let
        selectedArrowIndex_ =
            case selectedArrowIndex of
                Just value ->
                    value

                Nothing ->
                    -1

        ( htmlEvent, messageType ) =
            case arrowDragInProgress of
                True ->
                    ( "mousemove", ArrowDrag )

                False ->
                    ( "mousedown", PlaceMouseCoor )
    in
    Svg.g
        [ id "group" ]
        (List.concat
            [ [ Svg.rect
                    [ x "-50%"
                    , y "-50%"
                    , width "100%"
                    , height "100%"
                    , fillOpacity "0"
                    , on htmlEvent (Decode.map messageType offsetToPosition)

                    --, placeArrowOnClick (Messages.PlaceMouseCoor)
                    , id "ShotPlacer"
                    ]
                    []
              ]
            , List.map
                (\( index, { arrow } ) ->
                    Shot.arrow
                        ( index, arrow )
                        (selectedArrowIndex_ == index)
                        arrowDragInProgress
                )
                (selectedArrowLast
                    model
                    selectedArrowIndex_
                )
            ]
        )


isSelectedArrow : Int -> ( Int, Shot ) -> Bool
isSelectedArrow currentIndex ( index, shot ) =
    currentIndex == index


selectedArrowLast : List ( Int, Shot ) -> Int -> List ( Int, Shot )
selectedArrowLast indexedShots currentIndex =
    let
        ( selectedShotList, notSelected ) =
            List.partition (isSelectedArrow currentIndex) indexedShots
    in
    List.concat [ notSelected, selectedShotList ]



-- Update


placeArrowOnClick : (Position -> value) -> Attribute value
placeArrowOnClick msg =
    on "mousedown"
        (Decode.map
            msg
            offsetToPosition
        )


offsetToPosition : Decode.Decoder Position
offsetToPosition =
    Decode.map2 Position (Decode.field "clientX" Decode.int) (Decode.field "clientY" Decode.int)
