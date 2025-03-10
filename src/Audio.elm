port module Audio exposing
    ( Mapping
    , playAudio
    )


type alias Mapping =
    { effects : Float
    }


port playAudio : { label : String, volume : Float, delay : Float } -> Cmd msg
