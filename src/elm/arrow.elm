module Arrow exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Json.Decode as Decode


-- user imports

import Messages exposing (..)
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
        , selectOnClick index
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


selectedArrowEvents isDragInProgress =
    case isDragInProgress of
        True ->
            [ deselectOnClick ]

        False ->
            [ deselectOnClick
            , startDragOnPressedMouseMove
            ]


arrowSpecToSelectedArrowSvg : ( Int, ArrowSpec ) -> Bool -> Svg Messages.Msg
arrowSpecToSelectedArrowSvg ( index, arrowSpec ) isDragInProgress =
    Svg.g
        (List.concat
            [ [ transform ("translate(" ++ (toString arrowSpec.pos.x) ++ ", " ++ (toString arrowSpec.pos.y) ++ ")")
              , deselectOnClick
              , startDragOnPressedMouseMove
              ]
            , selectedArrowEvents isDragInProgress
            ]
        )
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


startDragOnPressedMouseMove =
    on "mousemove"
        (Decode.map3
            (\partial buttons intPos -> StageMsgPartial (partial buttons intPos))
            (Decode.succeed ArrowDragPotentialStart)
            (Decode.field "buttons" Decode.int)
            (Decode.map2 IntPosition (Decode.field "clientX" Decode.int) (Decode.field "clientY" Decode.int))
        )


selectOnClick : Int -> Attribute Msg
selectOnClick index =
    on "mousedown"
        (Decode.succeed <| Messages.SelectArrow index)


deselectOnClick : Attribute Msg
deselectOnClick =
    on "click"
        (Decode.succeed Messages.DeselectArrow)


offsetToPosition : Decode.Decoder IntPosition
offsetToPosition =
    Decode.map2 IntPosition (Decode.field "clientX" Decode.int) (Decode.field "clientY" Decode.int)
