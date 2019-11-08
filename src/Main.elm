module Main exposing (main)

import Browser
import Element



-- init


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( "Initial", Cmd.none )



-- Model


type alias Model =
    String


type Msg
    = NoOp



-- Update


update msg model =
    ( "Updated", Cmd.none )


subscriptions model =
    Sub.none



-- view


view model =
    Element.text model
        |> Element.el []
        |> Element.layout []
