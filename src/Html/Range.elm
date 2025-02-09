module Html.Range exposing (view)

import DOM
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Round


type alias Config msg =
    { max : Float
    , min : Float
    , lowValue : Float
    , highValue : Float
    , onLowChange : Float -> msg
    , onHighChange : Float -> msg
    }


view :
    { max : Float
    , min : Float
    , lowValue : Float
    , highValue : Float
    , onLowChange : Float -> msg
    , onHighChange : Float -> msg
    }
    -> Html msg
view config =
    Html.div []
        [ Html.div [ Html.Attributes.class "input-range-container" ]
            [ sliderInputView config.onLowChange config.lowValue config (inputDecoder config Low) [ "input-range--first" ]
            , sliderInputView config.onHighChange config.highValue config (inputDecoder config High) [ "input-range--second" ]
            , sliderTrackView (onOutsideRangeClick config)
            , progressView config
            ]
        , Html.div [ Html.Attributes.class "input-range-labels-container" ]
            [ Html.div
                [ Html.Attributes.class "input-range-label" ]
                [ Html.text (String.fromFloat config.min) ]
            , Html.div
                [ Html.Attributes.class "input-range-label input-range-label--current-value" ]
                [ Html.text (formatCurrentRange config) ]
            , Html.div
                [ Html.Attributes.class "input-range-label" ]
                [ Html.text (String.fromFloat config.max) ]
            ]
        ]


progressView : Config msg -> Html msg
progressView config =
    let
        lowValue =
            config.lowValue

        highValue =
            config.highValue

        progressRatio =
            100 / (config.max - config.min)

        progressLow =
            String.fromFloat ((lowValue - config.min) * progressRatio) ++ "%"

        progressHigh =
            String.fromFloat ((config.max - highValue) * progressRatio) ++ "%"
    in
    Html.div
        [ Html.Attributes.class "input-range__progress"
        , Html.Attributes.style "left" progressLow
        , Html.Attributes.style "right" progressHigh
        , Html.Events.on "click" (onInsideRangeClick config)
        ]
        []


onInsideRangeClick : Config msg -> Json.Decode.Decoder msg
onInsideRangeClick config =
    let
        valueTypeDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        centerThreshold =
                            rectangle.width / 2
                    in
                    if mouseX < centerThreshold then
                        Low

                    else
                        High
                )
                (Json.Decode.at [ "target" ] DOM.boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)

        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    snapValue ((((config.highValue - config.lowValue) / rectangle.width) * mouseX) + config.lowValue)
                )
                (Json.Decode.at [ "target" ] DOM.boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map2 (changeMsg config) valueTypeDecoder valueDecoder


formatCurrentRange : Config msg -> String
formatCurrentRange config =
    if config.lowValue == config.min && config.highValue == config.max then
        ""

    else
        String.join " " [ String.fromFloat config.lowValue, "-", String.fromFloat config.highValue ]


onOutsideRangeClick : Config msg -> Json.Decode.Decoder msg
onOutsideRangeClick config =
    let
        valueTypeDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        newValue =
                            snapValue ((config.max / rectangle.width) * mouseX)
                    in
                    if newValue < config.lowValue then
                        Low

                    else
                        High
                )
                (Json.Decode.at [ "target" ] DOM.boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)

        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    snapValue ((((config.max - config.min) / rectangle.width) * mouseX) + config.min)
                )
                (Json.Decode.at [ "target" ] DOM.boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map2 (changeMsg config) valueTypeDecoder valueDecoder


changeMsg : Config msg -> Thumb -> (Float -> msg)
changeMsg config thumb =
    case thumb of
        Low ->
            config.onLowChange

        High ->
            config.onHighChange


snapValue : Float -> Float
snapValue value =
    let
        stepDecimals =
            1
                |> String.fromFloat
                |> String.split "."
                |> List.drop 1
                |> List.head

        precision =
            case stepDecimals of
                Just s ->
                    String.length s

                Nothing ->
                    0
    in
    toFloat (round value)
        |> Round.roundNum precision


type Thumb
    = High
    | Low


inputDecoder : Config msg -> Thumb -> Json.Decode.Decoder Float
inputDecoder config thumb =
    Json.Decode.map (\value -> String.toFloat value |> Maybe.withDefault 0 |> convertValue config thumb)
        Html.Events.targetValue


convertValue : Config msg -> Thumb -> Float -> Float
convertValue config thumb value =
    case thumb of
        Low ->
            Basics.min value (config.highValue - 1)

        High ->
            Basics.max value (config.lowValue + 1)


sliderInputView : (Float -> msg) -> Float -> Config msg -> Json.Decode.Decoder Float -> List String -> Html msg
sliderInputView onChange value config input extraClasses =
    Html.input
        [ Html.Attributes.type_ "range"
        , Html.Attributes.min <| String.fromFloat config.min
        , Html.Attributes.max <| String.fromFloat config.max
        , Html.Attributes.step <| String.fromFloat 1
        , Html.Attributes.value <| String.fromFloat value
        , Html.Attributes.class "input-range"
        , Html.Attributes.classList <| List.map (\c -> ( c, True )) extraClasses
        , Html.Events.on "change" (Json.Decode.map onChange input)
        , Html.Events.on "input" (Json.Decode.map onChange input)
        ]
        []


sliderTrackView : Json.Decode.Decoder msg -> Html msg
sliderTrackView decoder =
    Html.div
        [ Html.Attributes.class "input-range__track"
        , onClick decoder
        ]
        []


onClick : Json.Decode.Decoder msg -> Html.Attribute msg
onClick decoder =
    Html.Events.on "click" decoder
