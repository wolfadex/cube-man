module Html.Extra exposing (select)

import Html exposing (Html)
import Html.Attributes
import Html.Events


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
