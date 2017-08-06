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


arrowSvgView : Float -> Bool -> List (Svg Messages.Msg)
arrowSvgView radius selected =
    case selected of
        True ->
            List.concat [ (arrowBase radius), (selectionHighlight radius) ]

        False ->
            arrowBase radius


arrowSpecToArrowSvg : ( Int, ArrowSpec ) -> Svg Messages.Msg
arrowSpecToArrowSvg ( index, arrowSpec ) =
    Svg.g
        [ transform ("translate(" ++ (toString arrowSpec.pos.x) ++ ", " ++ (toString arrowSpec.pos.y) ++ ")")
        , selectOnClick index
        ]
        (arrowSvgView arrowSpec.radius False)


arrowSpecToSelectedArrowSvg : ( Int, ArrowSpec ) -> Svg Messages.Msg
arrowSpecToSelectedArrowSvg ( index, arrowSpec ) =
    Svg.g
        [ transform ("translate(" ++ (toString arrowSpec.pos.x) ++ ", " ++ (toString arrowSpec.pos.y) ++ ")")
        , deselectOnClick
        , startDragOnPressedMouseMove
        ]
        (arrowSvgView arrowSpec.radius True)


arrowSpecToDragArrowSvg : ( Int, ArrowSpec ) -> Svg Messages.Msg
arrowSpecToDragArrowSvg ( index, arrowSpec ) =
    Svg.g
        [ transform ("translate(" ++ (toString arrowSpec.pos.x) ++ ", " ++ (toString arrowSpec.pos.y) ++ ")")
        , endArrowDragOnMouseUp
        , deselectOnMouseUp
        , on "mousemove" (Decode.map ArrowDrag offsetToPosition)
        ]
        (arrowSvgView arrowSpec.radius True)


selectionHighlight : Float -> List (Svg Messages.Msg)
selectionHighlight radius =
    [ Svg.circle
        [ cx "0"
        , cy "0"
        , r (radius * 1.65 |> toString)
        , fill "blue"
        , opacity ".4"
        , id "selectedHighlight"
        ]
        []
    ]


arrowBase : Float -> List (Svg Messages.Msg)
arrowBase radius =
    [ Svg.circle
        [ cx "0"
        , cy "0"
        , r (toString radius)
        , fill "white"
        , stroke "black"
        , strokeWidth (toString 0.1)
        , id "arrowCircle"
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
        (Decode.map2
            ArrowDragPotentialStart
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


deselectOnMouseUp : Attribute Msg
deselectOnMouseUp =
    on "click"
        (Decode.succeed Messages.DeselectArrow)


endArrowDragOnMouseUp : Attribute Msg
endArrowDragOnMouseUp =
    on "mouseup"
        (Decode.map
            Messages.ArrowDragEnd
            offsetToPosition
        )


offsetToPosition : Decode.Decoder IntPosition
offsetToPosition =
    Decode.map2 IntPosition (Decode.field "clientX" Decode.int) (Decode.field "clientY" Decode.int)
