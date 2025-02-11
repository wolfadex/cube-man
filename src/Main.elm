module Main exposing (main)

import Browser
import Screen.Editor
import Screen.FreePlay
import Screen.Game
import Screen.Menu
import Shared


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { sharedModel : Shared.Model
    , menuModel : Screen.Menu.Model
    , gameModel : Screen.Game.Model
    , freePlayModel : Screen.FreePlay.Model
    , editorModel : Screen.Editor.Model
    }


init : () -> ( Model, Cmd Msg )
init () =
    -- let
    --     maxX =
    --         9
    --     maxY =
    --         9
    --     maxZ =
    --         9
    --     sizeHelper =
    --         { maxX = maxX, maxY = maxY, maxZ = maxZ }
    --     board =
    --         { maxX = maxX
    --         , maxY = maxY
    --         , maxZ = maxZ
    --         , blocks =
    --             List.repeat (maxX * maxY * maxZ) Wall
    --                 |> List.foldl
    --                     (\block ( index, blocks ) ->
    --                         ( index + 1
    --                         , Dict.insert (indexToPoint sizeHelper index) block blocks
    --                         )
    --                     )
    --                     ( 0, Dict.empty )
    --                 |> Tuple.second
    --         }
    -- in
    -- ( { xLowerVisible = 0
    --   , xUpperVisible = maxX - 1
    --   , yLowerVisible = 0
    --   , yUpperVisible = maxY - 1
    --   , zLowerVisible = 0
    --   , zUpperVisible = maxZ - 1
    --   , selectedBlockType = Wall
    --   , board = board
    --   , score = 0
    --   , editorBoard = Undo.init board
    --   , boardLoadError = Nothing
    --   , boardPlayError = Nothing
    --   , screenSize = { width = 800, height = 600 }
    --   , editorCursor = ( 0, 0, 0 )
    --   , editorKeysDown = Set.empty
    --   , cameraRotation = Angle.degrees 225
    --   , cameraElevation = Angle.degrees 25
    --   , cameraDistance = Length.meters 30
    --   , cameraFocalPoint =
    --         Point3d.meters
    --             ((toFloat maxX - 1) / 2)
    --             ((toFloat maxY - 1) / 2)
    --             ((toFloat maxZ - 1) / 2)
    --   , mouseDragging = NoInteraction
    --   , cursorBounce =
    --         Animation.init 0
    --             [ { value = 0
    --               , offset = 0
    --               }
    --             , { value = 1
    --               , offset = 500
    --               }
    --             , { value = 0
    --               , offset = 500
    --               }
    --             ]
    --             |> Animation.withLoop
    --   , editorMode = EditBoard
    --   , blockEditMode = Select
    --   , cameraMode = Orbit
    --   , showBoardBounds = True
    --   , selectedBlock = Nothing
    --   , boardEncoding =
    --         board
    --             |> Serialize.encodeToJson boardCodec
    --             |> Json.Encode.encode 0
    --   , playerFrame = Frame3d.atPoint (Point3d.meters 1 4 7)
    --   , playerFacing = Forward
    --   , playerWantFacing = Forward
    --   , playerMovingAcrossEdge = Nothing
    --   , editorMaxXRaw = String.fromInt maxX
    --   , editorMaxYRaw = String.fromInt maxY
    --   , editorMaxZRaw = String.fromInt maxZ
    --   , blockPalette = SimpleBlocks
    --   , inputMapping =
    --         { moveUp = ( "w", "" )
    --         , moveDown = ( "s", "" )
    --         , moveLeft = ( "a", "" )
    --         , moveRight = ( "d", "" )
    --         --
    --         --
    --         , cameraOrbit = ( "1", "" )
    --         , cameraPan = ( "2", "" )
    --         , cameraZoom = ( "3", "" )
    --         , cameraReset = ( "4", "" )
    --         --
    --         , blockSelect = ( "q", "" )
    --         , blockAdd = ( "w", "" )
    --         , blockRemove = ( "e", "" )
    --         --
    --         , blockTypeWall = ( "a", "" )
    --         , blockTypeEdge = ( "s", "" )
    --         , blockTypePointPickup = ( "d", "" )
    --         , blockTypePlayerSpawn = ( "f", "" )
    --         --
    --         , undo = ( "z", "" )
    --         , redo = ( "x", "" )
    --         --
    --         , toggleSettings = ( ",", "" )
    --         }
    --   , showSettings = False
    --   , screen = Menu
    --   , freePlayMode = FreePlayBoardSelection
    --   , showFreePlayMenu = False
    --   }
    -- , Cmd.none
    -- )
    let
        ( sharedModel, sharedCmd ) =
            Shared.init

        ( menuModel, menuCmd ) =
            Screen.Menu.init

        ( gameModel, gameCmd ) =
            Screen.Game.init

        ( freePlayModel, freePlayCmd ) =
            Screen.FreePlay.init

        ( editorModel, editorCmd ) =
            Screen.Editor.init
    in
    ( { sharedModel = sharedModel
      , menuModel = menuModel
      , gameModel = gameModel
      , freePlayModel = freePlayModel
      , editorModel = editorModel
      }
    , Cmd.batch
        [ Cmd.map SharedMsg sharedCmd
        , Cmd.map MenuMsg menuCmd
        , Cmd.map GameMsg gameCmd
        , Cmd.map FreePlayMsg freePlayCmd
        , Cmd.map EditorMsg editorCmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.sharedModel.screen of
        Shared.Menu ->
            Screen.Menu.subscriptions model.menuModel
                |> Sub.map MenuMsg

        Shared.Game ->
            Screen.Game.subscriptions model.gameModel
                |> Sub.map GameMsg

        Shared.FreePlay ->
            Screen.FreePlay.subscriptions model.freePlayModel
                |> Sub.map FreePlayMsg

        Shared.Editor ->
            Screen.Editor.subscriptions model.editorModel
                |> Sub.map EditorMsg


type Msg
    = SharedMsg Shared.Msg
    | MenuMsg Screen.Menu.Msg
    | GameMsg Screen.Game.Msg
    | FreePlayMsg Screen.FreePlay.Msg
    | EditorMsg Screen.Editor.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SharedMsg message ->
            let
                ( sharedModel, sharedCmd ) =
                    Shared.update message model.sharedModel
            in
            ( { model | sharedModel = sharedModel }, Cmd.map SharedMsg sharedCmd )

        MenuMsg message ->
            let
                ( menuModel, menuCmd ) =
                    Screen.Menu.update model.sharedModel message model.menuModel
            in
            ( { model | menuModel = menuModel }, Cmd.map MenuMsg menuCmd )

        GameMsg message ->
            let
                ( gameModel, gameCmd ) =
                    Screen.Game.update model.sharedModel message model.gameModel
            in
            ( { model | gameModel = gameModel }, Cmd.map GameMsg gameCmd )

        FreePlayMsg message ->
            let
                ( freePlayModel, freePlayCmd ) =
                    Screen.FreePlay.update model.sharedModel message model.freePlayModel
            in
            ( { model | freePlayModel = freePlayModel }, Cmd.map FreePlayMsg freePlayCmd )

        EditorMsg message ->
            let
                ( editorModel, editorCmd ) =
                    Screen.Editor.update
                        SharedMsg
                        model.sharedModel
                        EditorMsg
                        message
                        model.editorModel
            in
            ( { model | editorModel = editorModel }, editorCmd )


view : Model -> Browser.Document Msg
view model =
    { title = "Cube-Man"
    , body =
        case model.sharedModel.screen of
            Shared.Menu ->
                Screen.Menu.view SharedMsg model.sharedModel MenuMsg model.menuModel

            Shared.Game ->
                Screen.Game.view SharedMsg model.sharedModel GameMsg model.gameModel

            Shared.FreePlay ->
                Screen.FreePlay.view SharedMsg model.sharedModel FreePlayMsg model.freePlayModel

            Shared.Editor ->
                Screen.Editor.view SharedMsg model.sharedModel EditorMsg model.editorModel
    }
