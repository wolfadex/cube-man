module Shared exposing
    ( Error
    , LoadedModel
    , Model(..)
    , Msg(..)
    , Screen(..)
    , TexturedMesh
    , init
    , update
    )

import Board
import Color exposing (Color)
import Frame3d
import Http
import Input
import Length
import Obj.Decode
import Scene3d.Material
import Scene3d.Mesh
import Task exposing (Task)
import Task.Parallel


type Model
    = Loading (Task.Parallel.State2 Msg TexturedMesh TexturedMesh)
    | Failed Error
    | Loaded LoadedModel


type alias LoadedModel =
    { screenSize : { width : Int, height : Int }
    , blockPalette : Board.BlockPalette
    , inputMapping : Input.Mapping
    , screen : Screen

    --
    , playerMesh : TexturedMesh
    }


type Screen
    = Editor
    | Game
    | FreePlay
    | Menu


init : ( Model, Cmd Msg )
init =
    let
        ( loadingState, loadingCmd ) =
            loadMeshes
    in
    ( Loading loadingState
    , loadingCmd
    )


loadMeshes =
    Task.Parallel.attempt2
        { task1 = loadMesh "player_pawn"
        , task2 = loadMesh "player_pawn"
        , onUpdates = TaskStateUpdated
        , onSuccess = AllTasksCompleted
        , onFailure = OneTaskFailed
        }


type alias TexturedMesh =
    ( Scene3d.Mesh.Unlit Board.WorldCoordinates, Scene3d.Material.Texture Color )


type Error
    = MeshLoadError String
    | TextureLoadError String


loadMesh : String -> Task Error TexturedMesh
loadMesh name =
    Task.map2 (\mesh texture -> ( Scene3d.Mesh.texturedTriangles mesh, texture ))
        (Http.task
            { method = "GET"
            , headers = []
            , url = "assets/" ++ name ++ ".obj"
            , body = Http.emptyBody
            , resolver =
                Http.stringResolver
                    (\response ->
                        case response of
                            Http.BadUrl_ url ->
                                Err (Http.BadUrl url)

                            Http.Timeout_ ->
                                Err Http.Timeout

                            Http.NetworkError_ ->
                                Err Http.NetworkError

                            Http.BadStatus_ metadata _ ->
                                Err (Http.BadStatus metadata.statusCode)

                            Http.GoodStatus_ _ body ->
                                case
                                    Obj.Decode.decodeString
                                        Length.centimeters
                                        (Obj.Decode.texturedTrianglesIn Frame3d.atOrigin)
                                        body
                                of
                                    Err err ->
                                        Err (Http.BadBody err)

                                    Ok mesh ->
                                        Ok mesh
                    )
            , timeout = Nothing
            }
            |> Task.mapError (\_ -> MeshLoadError name)
        )
        (Scene3d.Material.loadWith Scene3d.Material.nearestNeighborFiltering ("assets/" ++ name ++ ".png")
            |> Task.mapError (\_ -> TextureLoadError name)
        )


type Msg
    = SetMapping (Input.Mapping -> Input.Mapping)
    | SetScreen Screen
    | SetBlockPalette Board.BlockPalette
    | TaskStateUpdated (Task.Parallel.Msg2 TexturedMesh TexturedMesh)
    | OneTaskFailed Error
    | AllTasksCompleted TexturedMesh TexturedMesh


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Failed _, _ ) ->
            ( model, Cmd.none )

        ( Loading state, TaskStateUpdated message ) ->
            let
                ( nextState, nextCmd ) =
                    Task.Parallel.update2 state message
            in
            ( Loading nextState, nextCmd )

        ( Loading _, OneTaskFailed error ) ->
            ( Failed error, Cmd.none )

        ( Loading _, AllTasksCompleted playerMesh _ ) ->
            ( Loaded
                { screenSize = { width = 800, height = 600 }
                , blockPalette = Board.SimpleBlocks
                , inputMapping = Input.defaultMapping
                , screen = Menu
                , playerMesh = playerMesh
                }
            , Cmd.none
            )

        ( Loading _, _ ) ->
            ( model, Cmd.none )

        ( Loaded _, TaskStateUpdated _ ) ->
            ( model, Cmd.none )

        ( Loaded _, OneTaskFailed _ ) ->
            ( model, Cmd.none )

        ( Loaded _, AllTasksCompleted _ _ ) ->
            ( model, Cmd.none )

        ( Loaded mod, SetMapping fn ) ->
            ( Loaded { mod | inputMapping = fn mod.inputMapping }, Cmd.none )

        ( Loaded mod, SetScreen screen ) ->
            ( Loaded { mod | screen = screen }, Cmd.none )

        ( Loaded mod, SetBlockPalette blockPalette ) ->
            ( Loaded { mod | blockPalette = blockPalette }, Cmd.none )
