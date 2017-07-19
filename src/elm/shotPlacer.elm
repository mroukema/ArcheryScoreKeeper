module ShotPlacer exposing (..)

-- elm-lang

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Mouse exposing (..)
import Json.Decode as Decode


-- user

import Messages exposing (Msg)
import Shot exposing (Shot)


-- View


shotPlacer : List ( Int, Shot ) -> Maybe Int -> Svg Msg
shotPlacer model selectedArrowIndex =
    let
        selectedArrowIndex_ =
            case selectedArrowIndex of
                Just value ->
                    value

                Nothing ->
                    -1
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
                        , placeArrowOnClick (Messages.PlaceMouseCoor)
                        , id "ShotPlacer"
                        ]
                        []
                  ]
                , (List.map
                    (\( index, { arrow } ) ->
                        Shot.arrow ( index, arrow ) (selectedArrowIndex_ == index)
                    )
                    (selectedArrowLast
                        model
                        selectedArrowIndex_
                    )
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


placeArrowOnClick : (Mouse.Position -> value) -> Attribute value
placeArrowOnClick msg =
    on "mousedown"
        (Decode.map
            msg
            offsetToPosition
        )


offsetToPosition : Decode.Decoder Mouse.Position
offsetToPosition =
    Decode.map2 Mouse.Position (Decode.field "clientX" Decode.int) (Decode.field "clientY" Decode.int)
