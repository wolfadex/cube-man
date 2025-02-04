module Animation exposing
    ( Animation
    , KeyFrame
    , init
    , step
    , withLoop
    )


type Animation a
    = Animation
        { state : State a
        , frames : List (KeyFrame a)
        , loops : Bool
        , default : a
        , totalTime : Float
        }


type alias KeyFrame a =
    { value : a
    , offset : Float
    }


type State a
    = NoAnimation
    | Animating Float
    | Complete a


init : a -> List (KeyFrame a) -> Animation a
init default frames =
    Animation
        { state =
            case frames of
                [] ->
                    NoAnimation

                frame :: [] ->
                    Complete frame.value

                _ ->
                    Animating 0
        , frames = frames
        , loops = False
        , default = default
        , totalTime = List.foldl (\frame total -> frame.offset + total) 0 frames
        }


withLoop : Animation a -> Animation a
withLoop (Animation anim) =
    Animation { anim | loops = True }


step :
    { interpolate : a -> a -> Float -> a
    , easing : Float -> Float
    }
    -> Float
    -> Animation a
    -> ( a, Animation a )
step { interpolate, easing } deltaTime (Animation anim) =
    case anim.state of
        NoAnimation ->
            ( anim.default, Animation anim )

        Complete frame ->
            ( frame, Animation anim )

        Animating elapsedTime ->
            if anim.loops then
                let
                    newElapsedTime =
                        elapsedTime + deltaTime

                    ( frame, isComplete ) =
                        stepHelper anim.default interpolate anim.frames newElapsedTime
                in
                if isComplete then
                    step { interpolate = interpolate, easing = easing }
                        deltaTime
                        (Animation { anim | state = Animating (newElapsedTime - anim.totalTime) })

                else
                    ( frame
                    , Animation
                        { anim
                            | state = Animating newElapsedTime
                        }
                    )

            else
                let
                    newElapsedTime =
                        elapsedTime + deltaTime

                    ( frame, isComplete ) =
                        stepHelper anim.default interpolate anim.frames newElapsedTime
                in
                ( frame
                , Animation
                    { anim
                        | state =
                            if isComplete then
                                Complete frame

                            else
                                Animating newElapsedTime
                    }
                )


stepHelper : a -> (a -> a -> Float -> a) -> List (KeyFrame a) -> Float -> ( a, Bool )
stepHelper default interpolate frames elapsedTime =
    case frames of
        [] ->
            ( default, True )

        frame :: [] ->
            ( frame.value, True )

        frameA :: frameB :: rest ->
            if elapsedTime <= 0 then
                ( frameA.value, False )

            else if elapsedTime == frameB.offset then
                ( frameB.value, False )

            else if elapsedTime <= frameB.offset then
                let
                    t =
                        elapsedTime / frameB.offset
                in
                ( interpolate frameA.value frameB.value t
                , False
                )

            else
                stepHelper default interpolate (frameB :: rest) (elapsedTime - frameB.offset)
