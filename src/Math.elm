module Math exposing (betterRound)


betterRound : Float -> Int
betterRound n =
    if n > 0 then
        round n

    else
        -(round (abs n))
