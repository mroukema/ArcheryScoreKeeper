module Score exposing (Label, Score, Value)


type alias Label =
    String


type alias Value =
    Int


type Score
    = Score Label Value


value score =
    case score of
        Score _ value_ ->
            value_


label score =
    case score of
        Score label_ _ ->
            label_
