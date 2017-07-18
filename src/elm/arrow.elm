module Arrow exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Json.Decode as Decode


-- user imports

import Messages
import Types exposing (..)


-- Model


type alias ArrowSpec =
    { radius : Float
    , pos : FloatPosition
    }


defaultArrow : FloatPosition -> ArrowSpec
defaultArrow =
    ArrowSpec 0.65



-- View


arrowSpecToArrowSvg : ( Int, ArrowSpec ) -> Svg Messages.Msg
arrowSpecToArrowSvg ( index, arrowSpec ) =
    Svg.g
        [ transform ("translate(" ++ (toString arrowSpec.pos.x) ++ ", " ++ (toString arrowSpec.pos.y) ++ ")")
        , selectOnMouseUp (Messages.SelectArrow index)
        ]
        [ Svg.circle
            [ cx "0"
            , cy "0"
            , r (toString arrowSpec.radius)
            , fill "white"
            , stroke "black"
            , strokeWidth (toString 0.1)
            , id "arrowCircle"
            ]
            []
        , Svg.path [ d "M -0.3 0 L 0.3 0 M 0 -0.3 L 0 0.3", stroke "black", strokeWidth "0.1", id "Center" ] []
        ]


arrowSpecToSelectedArrowSvg : ( Int, ArrowSpec ) -> Svg Messages.Msg
arrowSpecToSelectedArrowSvg ( index, arrowSpec ) =
    Svg.g
        [ transform ("translate(" ++ (toString arrowSpec.pos.x) ++ ", " ++ (toString arrowSpec.pos.y) ++ ")")
        , selectOnMouseUp (Messages.SelectArrow index)
        ]
        [ Svg.circle
            [ cx "0"
            , cy "0"
            , r (toString arrowSpec.radius)
            , fill "white"
            , stroke "black"
            , strokeWidth (0.1 |> toString)
            , id "arrowCircle"
            ]
            []
        , Svg.circle
            [ cx "0"
            , cy "0"
            , r (arrowSpec.radius * 1.65 |> toString)
            , fill "blue"
            , opacity ".4"
            , id "selectedHighlight"
            ]
            []
        , Svg.path [ d "M -0.3 0 L 0.3 0 M 0 -0.3 L 0 0.3", stroke "black", strokeWidth "0.1", id "Center" ] []
        ]


arrow : FloatPosition -> Svg Messages.Msg
arrow pos =
    arrowSpecToArrowSvg ( 0, (defaultArrow pos) )



-- Update


startDragOnDown msg =
    on "mousedown"
        (Decode.map
            msg
            offsetToPosition
        )


selectOnMouseUp msg =
    on "mousedown"
        (Decode.succeed msg)


offsetToPosition : Decode.Decoder IntPosition
offsetToPosition =
    Decode.map2 IntPosition (Decode.field "clientX" Decode.int) (Decode.field "clientY" Decode.int)
