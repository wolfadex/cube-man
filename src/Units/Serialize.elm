module Units.Serialize exposing (durationCodec)

import Duration exposing (Duration)
import Serialize


durationCodec : Serialize.Codec e Duration
durationCodec =
    Serialize.customType
        (\encoder value ->
            encoder (Duration.inSeconds value)
        )
        |> Serialize.variant1 Duration.seconds Serialize.float
        |> Serialize.finishCustomType
