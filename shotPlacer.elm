module ShotPlacer exposing (..)

-- elm-lang

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Mouse exposing (..)
import Json.Decode as Decode
import Html.Events


-- user

import Arrow exposing (..)
import Messages exposing (Msg)
import Shot exposing (Shot)


-- Model
--type alias Model =
--    { arrowPosList : List Arrow.Position
--    }
-- View


shotPlacer : List Shot -> Svg Msg
shotPlacer model =
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
            , (List.map Shot.arrow model)
            ]
        )


type alias ViewBox =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }



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
