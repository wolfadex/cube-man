module Input exposing
    ( Mapping
    , defaultMapping
    , isInputKey
    , viewInputKeyHoverText
    , viewMapping
    )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode


type alias Mapping =
    { moveUp : ( String, String )
    , moveDown : ( String, String )
    , moveLeft : ( String, String )
    , moveRight : ( String, String )

    --
    --
    , cameraOrbit : ( String, String )
    , cameraPan : ( String, String )
    , cameraZoom : ( String, String )
    , cameraReset : ( String, String )

    --
    , blockSelect : ( String, String )
    , blockAdd : ( String, String )
    , blockRemove : ( String, String )

    --
    , blockTypeWall : ( String, String )
    , blockTypeEdge : ( String, String )
    , blockTypePointPickup : ( String, String )
    , blockTypePlayerSpawn : ( String, String )
    , blockTypeEnemySpawner : ( String, String )

    --
    , undo : ( String, String )
    , redo : ( String, String )

    --
    , toggleSettings : ( String, String )
    }


viewInputKeyHoverText : ( String, String ) -> String
viewInputKeyHoverText ( primary, secondary ) =
    if secondary == "" then
        primary

    else
        primary ++ " | " ++ secondary


isInputKey : ( String, String ) -> String -> Bool
isInputKey ( primary, secondary ) key =
    key == primary || key == secondary


defaultMapping : Mapping
defaultMapping =
    { moveUp = ( "w", "ArrowUp" )
    , moveDown = ( "s", "ArrowDown" )
    , moveLeft = ( "a", "ArrowLeft" )
    , moveRight = ( "d", "ArrowRight" )

    --
    --
    , cameraOrbit = ( "1", "" )
    , cameraPan = ( "2", "" )
    , cameraZoom = ( "3", "" )
    , cameraReset = ( "4", "" )

    --
    , blockSelect = ( "q", "" )
    , blockAdd = ( "w", "" )
    , blockRemove = ( "e", "" )

    --
    , blockTypeWall = ( "a", "" )
    , blockTypeEdge = ( "s", "" )
    , blockTypePointPickup = ( "d", "" )
    , blockTypeEnemySpawner = ( "f", "" )
    , blockTypePlayerSpawn = ( "g", "" )

    --
    , undo = ( "z", "" )
    , redo = ( "x", "" )

    --
    , toggleSettings = ( ",", "" )
    }


viewMapping :
    ((Mapping -> Mapping) -> msg)
    ->
        { keys : ( String, String )
        , label : String
        , setPrimary : String -> Mapping -> Mapping
        , setSecondary : String -> Mapping -> Mapping
        }
    -> Html msg
viewMapping toMsg mapping =
    let
        ( primary, secondary ) =
            mapping.keys
    in
    Html.tr []
        [ Html.th [ Html.Attributes.attribute "align" "left" ] [ Html.text mapping.label ]
        , Html.td [ Html.Attributes.attribute "align" "center" ]
            [ Html.input
                [ Html.Attributes.value primary
                , Html.Events.custom "keydown" (decodeMappingChange toMsg mapping.setPrimary)
                , Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.placeholder "must be set"
                ]
                []
            ]
        , Html.td [ Html.Attributes.attribute "align" "center" ]
            [ Html.input
                [ Html.Attributes.value secondary
                , Html.Events.custom "keydown" (decodeMappingChange toMsg mapping.setSecondary)
                , Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.placeholder "not set"
                ]
                []
            ]
        ]


decodeMappingChange :
    ((Mapping -> Mapping) -> msg)
    -> (String -> Mapping -> Mapping)
    ->
        Json.Decode.Decoder
            { message : msg
            , preventDefault : Bool
            , stopPropagation : Bool
            }
decodeMappingChange toMsg fn =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\key ->
                case key of
                    "Tab" ->
                        Json.Decode.fail "Don't capture"

                    "Shift" ->
                        Json.Decode.fail "Don't capture"

                    "Meta" ->
                        Json.Decode.fail "Don't capture"

                    "Alt" ->
                        Json.Decode.fail "Don't capture"

                    "Control" ->
                        Json.Decode.fail "Don't capture"

                    "Escape" ->
                        Json.Decode.fail "Don't capture"

                    "Enter" ->
                        Json.Decode.fail "Don't capture"

                    "Backspace" ->
                        Json.Decode.succeed
                            { message = toMsg (fn "")
                            , preventDefault = True
                            , stopPropagation = True
                            }

                    _ ->
                        Json.Decode.succeed
                            { message = toMsg (fn key)
                            , preventDefault = True
                            , stopPropagation = True
                            }
            )
