module Html.Extra exposing
    ( dualRange
    , modal
    , select
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode


select :
    List (Html.Attribute msg)
    ->
        { value : Maybe value
        , options : List value
        , toLabel : value -> String
        , toKey : value -> String
        , onSelect : Maybe value -> msg
        }
    -> Html msg
select attributes config =
    Html.select
        (attributes
            ++ [ Html.Events.onInput
                    (\key ->
                        config.onSelect
                            (selectChangeHelper config.toKey config.options key)
                    )
               , Html.Attributes.value <|
                    case config.value of
                        Nothing ->
                            ""

                        Just value ->
                            config.toKey value
               ]
        )
        (List.map
            (\option ->
                Html.option
                    [ Html.Attributes.value <| config.toKey option ]
                    [ Html.text <| config.toLabel option ]
            )
            config.options
        )


selectChangeHelper : (value -> String) -> List value -> String -> Maybe value
selectChangeHelper toKey options key =
    case options of
        [] ->
            Nothing

        value :: rest ->
            if toKey value == key then
                Just value

            else
                selectChangeHelper toKey rest key



-- nonBreakingSpace : String
-- nonBreakingSpace =
--     "\u{00A0}"


dualRange :
    List (Html.Attribute msg)
    ->
        { valueLow : Float
        , valueHigh : Float
        , onInputLow : Float -> msg
        , onInputHigh : Float -> msg
        , min : Float
        , max : Float
        , step : Float
        , colorMin : Maybe String
        , colorMid : Maybe String
        , colorMax : Maybe String
        , thumb1Image : Maybe String
        , thumb2Image : Maybe String
        }
    -> Html msg
dualRange attributes config =
    let
        percent1 =
            config.valueLow / config.max * 100

        percent2 =
            config.valueHigh / config.max * 100

        colorMin =
            Maybe.withDefault "gray" config.colorMin

        colorMid =
            Maybe.withDefault "cornflowerblue" config.colorMid

        colorMax =
            Maybe.withDefault "gray" config.colorMax

        backgroundColorLeft =
            "linear-gradient(to right, "
                ++ colorMin
                ++ " 0%, "
                ++ colorMin
                ++ " "
                ++ String.fromFloat percent1
                ++ "%, "
                ++ colorMid
                ++ " "
                ++ String.fromFloat percent1
                ++ "%, rgba(0, 0, 0, 0) "
                ++ String.fromFloat percent2
                ++ "%, rgba(0, 0, 0, 0) "
                ++ String.fromFloat percent2
                ++ "%, rgba(0, 0, 0, 0) 100%)"

        backgroundColorRight =
            "linear-gradient(to right, "
                ++ colorMin
                ++ " 0%, "
                ++ colorMin
                ++ " "
                ++ String.fromFloat percent1
                ++ "%, "
                ++ colorMid
                ++ " "
                ++ String.fromFloat percent1
                ++ "%, "
                ++ colorMid
                ++ " "
                ++ String.fromFloat percent2
                ++ "%, "
                ++ colorMax
                ++ " "
                ++ String.fromFloat percent2
                ++ "%, "
                ++ colorMax
                ++ " 100%)"
    in
    Html.div
        (attributes
            ++ [ Html.Attributes.class "dual-range__container"
               ]
        )
        [ Html.div
            [ Html.Attributes.class "dual-range__controls"
            ]
            [ Html.input
                [ Html.Attributes.class "dual-range__control-1"
                , Html.Attributes.type_ "range"
                , Html.Attributes.value (String.fromFloat config.valueLow)
                , Html.Events.onInput
                    (String.toFloat
                        >> Maybe.withDefault config.valueLow
                        >> config.onInputLow
                    )
                , Html.Attributes.min (String.fromFloat config.min)
                , Html.Attributes.max (String.fromFloat config.max)
                , Html.Attributes.step (String.fromFloat config.step)
                , case config.thumb1Image of
                    Nothing ->
                        Html.Attributes.attribute "style" ("background: " ++ backgroundColorLeft ++ "; --thumb-image: white;")

                    Just thumb1Image ->
                        Html.Attributes.attribute "style" ("background: " ++ backgroundColorLeft ++ "; --thumb-image: white url(" ++ thumb1Image ++ ") center center/contain no-repeat;")
                ]
                []
            , Html.input
                [ Html.Attributes.class "dual-range__control-2"
                , Html.Attributes.type_ "range"
                , Html.Attributes.value (String.fromFloat config.valueHigh)
                , Html.Events.onInput
                    (String.toFloat
                        >> Maybe.withDefault config.valueHigh
                        >> config.onInputHigh
                    )
                , Html.Attributes.min (String.fromFloat config.min)
                , Html.Attributes.max (String.fromFloat config.max)
                , Html.Attributes.step (String.fromFloat config.step)
                , case config.thumb2Image of
                    Nothing ->
                        Html.Attributes.attribute "style" ("background: " ++ backgroundColorRight ++ "; --thumb-image: white;")

                    Just thumb2Image ->
                        Html.Attributes.attribute "style" ("background: " ++ backgroundColorRight ++ "; --thumb-image: white url(" ++ thumb2Image ++ ") center center/contain no-repeat;")
                ]
                []
            ]
        ]


modal : { open : Bool, onClose : msg } -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
modal config attributes children =
    Html.node "dialog"
        ([ Html.Attributes.property "___modal-open" (Json.Encode.bool config.open)

         -- , Html.Attributes.attribute "data-state" <|
         --    if config.open then
         --        "open"
         --    else
         --        "closed"
         , Html.Events.on "close" (Json.Decode.succeed config.onClose)

         -- , Html.Attributes.classList
         --    ([ ( "rt-BaseDialogContent", True )
         --     , ( "rt-BaseDialogContent-overrides", True )
         --     , ( "rt-DialogContent", config.style == PlainModal || config.style == DialogModal )
         --     , ( "rt-AlertDialogContent", config.style == AlertDialogModal )
         --     , ( "rt-r-size-" ++ String.fromInt config.size, True )
         --     , ( "rt-r-w", config.width /= Nothing )
         --     , ( "rt-r-min-w", config.minWidth /= Nothing )
         --     , ( "rt-r-max-w", True )
         --     ]
         --        ++ config.customClassList
         --    )
         -- , Radix.styles
         --    (List.filterMap identity
         --        [ Maybe.map
         --            (\width -> ( "--width", width ))
         --            config.width
         --        , Maybe.map
         --            (\minWidth -> ( "--min-width", minWidth ))
         --            config.minWidth
         --        , Just ( "--max-width", config.maxWidth )
         --        ]
         --        ++ config.customStyles
         --    )
         ]
            ++ attributes
        )
        children
