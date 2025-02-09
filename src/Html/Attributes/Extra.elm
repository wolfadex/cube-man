module Html.Attributes.Extra exposing
    ( aria
    , bool
    )

import Html
import Html.Attributes


aria : String -> String -> Html.Attribute msg
aria attrib value =
    Html.Attributes.attribute ("aria-" ++ attrib) value


bool : Bool -> String
bool b =
    if b then
        "true"

    else
        "false"
