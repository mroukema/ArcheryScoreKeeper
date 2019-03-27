module Arrow exposing (ArrowSpec, FloatPosition, Msg, arrow, arrowBase, defaultArrow, selectedArrow, update)

import Json.Decode as Decode
import String exposing (fromFloat)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)



-- Model


type Msg
    = ArrowSelection ( Int, Int )
    | Test


update msg model =
    case msg of
        ArrowSelection selection ->
            ( { model | selectedShot = Just selection }, Cmd.none )

        Test ->
            ( model, Cmd.none )


type alias FloatPosition =
    { x : Float, y : Float }


type alias Shot =
    { arrow : ArrowSpec
    , pos : ( Float, Float )
    }


type alias ArrowSpec =
    Float


defaultArrow : ArrowSpec
defaultArrow =
    0.65


selectedArrow shot attr =
    let
        ( x, y ) =
            shot.pos
    in
    Svg.g
        (List.append
            [ transform
                ("translate("
                    ++ fromFloat x
                    ++ ", "
                    ++ fromFloat y
                    ++ ")"
                )
            ]
            attr
        )
        (List.append
            (arrowBase defaultArrow)
            (selectionHighlight defaultArrow)
        )


arrow : Shot -> List (Attribute msg) -> Svg msg
arrow shot attr =
    let
        ( x, y ) =
            shot.pos
    in
    Svg.g
        (List.append
            [ transform
                ("translate("
                    ++ fromFloat x
                    ++ ", "
                    ++ fromFloat y
                    ++ ")"
                )
            ]
            attr
        )
        (arrowBase defaultArrow)


selectionHighlight : Float -> List (Svg msg)
selectionHighlight radius =
    [ Svg.circle
        [ cx "0"
        , cy "0"
        , r (radius * 1.65 |> fromFloat)
        , fill "blue"
        , opacity ".4"
        , id "selectedHighlight"
        ]
        []
    ]


arrowBase : Float -> List (Svg msg)
arrowBase radius =
    [ Svg.circle
        [ cx "0"
        , cy "0"
        , r (fromFloat radius)
        , fill "white"
        , stroke "black"
        , strokeWidth (fromFloat 0.1)
        , id "arrowCircle"
        ]
        []
    , Svg.path [ d "M -0.3 0 L 0.3 0 M 0 -0.3 L 0 0.3", stroke "black", strokeWidth "0.1", id "Center" ] []
    ]
