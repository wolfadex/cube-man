module Input exposing (Mapping, decodeMappingChange, defaultMapping, isInputKey, viewInputKeyHoverText)

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
    Json.Decode.field "data" Json.Decode.string
        |> Json.Decode.andThen
            (\data ->
                let
                    trimmed =
                        data
                            |> String.trim
                            |> String.right 1
                            |> String.trim
                in
                if String.isEmpty trimmed then
                    Json.Decode.fail "Not a valid input mapping"

                else
                    Json.Decode.succeed
                        { message = toMsg (fn trimmed)
                        , preventDefault = True
                        , stopPropagation = True
                        }
            )


isInputKey : ( String, String ) -> String -> Bool
isInputKey ( primary, secondary ) key =
    key == primary || key == secondary


defaultMapping : Mapping
defaultMapping =
    { moveUp = ( "w", "" )
    , moveDown = ( "s", "" )
    , moveLeft = ( "a", "" )
    , moveRight = ( "d", "" )

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
