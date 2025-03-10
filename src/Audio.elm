port module Audio exposing
    ( Audio
    , Mapping
    , effect
    , play
    , withDelay
    )


type alias Mapping =
    { effects : Float
    }


type Audio
    = Audio { label : String, volume : Float, delay : Float }


effect : { label : String, volume : Float } -> Audio
effect { label, volume } =
    Audio { label = "effect_" ++ label, volume = volume, delay = 0 }


withDelay : Float -> Audio -> Audio
withDelay delay (Audio audio) =
    Audio { audio | delay = delay }


play : Audio -> Cmd msg
play (Audio audio) =
    playAudio audio


port playAudio : { label : String, volume : Float, delay : Float } -> Cmd msg
