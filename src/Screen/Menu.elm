module Screen.Menu exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Shared


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = NoOp


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ model =
    ( model, Cmd.none )


view : (Shared.Msg -> msg) -> Shared.Model -> (Msg -> msg) -> Model -> List (Html msg)
view toSharedMsg _ _ _ =
    [ Html.div
        [ Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "display" "grid"
        , Html.Attributes.style "grid-template-columns" "auto auto auto"
        ]
        [ Html.div
            [ Html.Attributes.style "grid-column" "2"
            , Html.Attributes.style "margin-top" "5rem"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "gap" "1rem"
            , Html.Attributes.style "align-items" "center"
            ]
            [ Html.h1
                [ Html.Attributes.style "text-align" "center"
                ]
                [ Html.text "Cube-Man" ]
            , Html.div
                [ Html.Attributes.style "margin-top" "5rem"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-direction" "column"
                , Html.Attributes.style "gap" "1rem"
                ]
                [ Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "padding" "0.5rem 2rem"
                    , Html.Events.onClick (toSharedMsg (Shared.SetScreen Shared.Game))
                    ]
                    [ Html.span [ Html.Attributes.style "text-decoration" "line-through" ] [ Html.text "Play" ]
                    , Html.br [] []
                    , Html.small [] [ Html.text " Coming soon..." ]
                    ]
                , Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "padding" "0.5rem 2rem"
                    , Html.Events.onClick (toSharedMsg (Shared.SetScreen Shared.FreePlay))
                    ]
                    [ Html.text "Free Play" ]
                , Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "padding" "0.5rem 2rem"
                    , Html.Events.onClick (toSharedMsg (Shared.SetScreen Shared.Editor))
                    ]
                    [ Html.text "Level Editor" ]
                ]
            ]
        ]
    ]
