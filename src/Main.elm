module Main exposing (main)

import Angle exposing (Angle)
import Animation exposing (Animation)
import Axis3d exposing (Axis3d)
import Axis3d.Extra
import Block3d
import BoundingBox3d
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cone3d
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes
import Html.Attributes.Extra
import Html.Events
import Html.Extra
import Illuminance
import Json.Decode
import Json.Encode
import Length exposing (Length)
import LineSegment3d
import List.Cartesian
import Luminance
import Phosphor
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Extra
import Quantity
import Rectangle2d
import Scene3d
import Scene3d.Light
import Scene3d.Material
import Serialize
import Set exposing (Set)
import SketchPlane3d
import Sphere3d
import Undo
import Vector3d
import Viewpoint3d exposing (Viewpoint3d)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { -- Game play
      board : Board
    , score : Int
    , playerFrame : Frame3d Length.Meters WorldCoordinates { defines : {} }
    , playerFacing : Facing
    , playerWantFacing : Facing
    , playerMovingAcrossEdge : Maybe Angle

    -- EditBoard
    , editorBoard : Undo.Stack Board
    , boardLoadError : Maybe BoardLoadError
    , boardPlayError : Maybe BoardPlayError
    , xLowerVisible : Int
    , xUpperVisible : Int
    , yLowerVisible : Int
    , yUpperVisible : Int
    , zLowerVisible : Int
    , zUpperVisible : Int
    , selectedBlockType : Block
    , editorCursor : Point
    , editorKeysDown : Set String
    , cameraRotation : Angle
    , cameraElevation : Angle
    , cameraDistance : Length
    , cameraFocalPoint : Point3d Length.Meters WorldCoordinates
    , mouseDragging : EditorMouseInteraction
    , cursorBounce : Animation Float
    , boardEncoding : String
    , editorMode : EditorMode
    , blockEditMode : BlockEditMode
    , cameraMode : CameraMode
    , showBoardBounds : Bool
    , selectedBlock : Maybe ( Point, Block )
    , editorMaxXRaw : String
    , editorMaxYRaw : String
    , editorMaxZRaw : String

    -- Common
    , screenSize : { width : Int, height : Int }
    , blockPalette : BlockPalette
    , inputMapping : InputMapping
    , showSettings : Bool
    , screen : Screen
    }


type alias InputMapping =
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

    --
    , undo : ( String, String )
    , redo : ( String, String )

    --
    , toggleSettings : ( String, String )
    }


type Screen
    = Editor
    | Game
    | FreePlay
    | Menu


type BlockPalette
    = SimpleBlocks
    | RainbowBlocks


type EditorMouseInteraction
    = NoInteraction
    | InteractionStart Json.Decode.Value
    | InteractionMoving Json.Decode.Value


type EditorMode
    = EditBoard
    | TestGame


type BlockEditMode
    = Add
    | Remove
    | Select


type CameraMode
    = Orbit
    | Pan
    | Zoom


type Facing
    = Forward
    | Backward
    | Left
    | Right


type WorldCoordinates
    = WorldCoordinates Never


type alias Point =
    ( Int, Int, Int )


pointCodec : Serialize.Codec e Point
pointCodec =
    Serialize.triple Serialize.int Serialize.int Serialize.int


type alias Board =
    { maxX : Int
    , maxY : Int
    , maxZ : Int
    , blocks : Dict Point Block
    }


type Block
    = Empty
    | Wall
    | Edge
    | PointPickup Bool
    | PlayerSpawn { forward : Axis, left : Axis }


boardCodec : Serialize.Codec e Board
boardCodec =
    Serialize.record Board
        |> Serialize.field .maxX Serialize.int
        |> Serialize.field .maxY Serialize.int
        |> Serialize.field .maxZ Serialize.int
        |> Serialize.field .blocks (Serialize.dict pointCodec blockCodec)
        |> Serialize.finishRecord


playerSpawnDetailsCodec : Serialize.Codec e { forward : Axis, left : Axis }
playerSpawnDetailsCodec =
    Serialize.record (\forward left -> { forward = forward, left = left })
        |> Serialize.field .forward axisCodex
        |> Serialize.field .left axisCodex
        |> Serialize.finishRecord


axisCodex : Serialize.Codec e Axis
axisCodex =
    Serialize.customType
        (\positiveXEncoder negativeXEncoder positiveYEncoder negativeYEncoder positiveZEncoder negativeZEncoder value ->
            case value of
                PositiveX ->
                    positiveXEncoder

                NegativeX ->
                    negativeXEncoder

                PositiveY ->
                    positiveYEncoder

                NegativeY ->
                    negativeYEncoder

                PositiveZ ->
                    positiveZEncoder

                NegativeZ ->
                    negativeZEncoder
        )
        |> Serialize.variant0 PositiveX
        |> Serialize.variant0 NegativeX
        |> Serialize.variant0 PositiveY
        |> Serialize.variant0 NegativeY
        |> Serialize.variant0 PositiveZ
        |> Serialize.variant0 NegativeZ
        |> Serialize.finishCustomType


blockCodec : Serialize.Codec e Block
blockCodec =
    Serialize.customType
        (\emptyEncoder wallEncoder edgeEncoder pointPickupEncoder playerSpawnEncoder value ->
            case value of
                Empty ->
                    emptyEncoder

                Wall ->
                    wallEncoder

                Edge ->
                    edgeEncoder

                PointPickup collected ->
                    pointPickupEncoder collected

                PlayerSpawn details ->
                    playerSpawnEncoder details
        )
        |> Serialize.variant0 Empty
        |> Serialize.variant0 Wall
        |> Serialize.variant0 Edge
        |> Serialize.variant1 PointPickup Serialize.bool
        |> Serialize.variant1 PlayerSpawn playerSpawnDetailsCodec
        |> Serialize.finishCustomType



-- pointToIndex : { m | maxY : Int, maxZ : Int } -> Point -> Int
-- pointToIndex { maxY, maxZ } ( x, y, z ) =
--     x * maxY * maxZ + y * maxZ + z


indexToPoint : { m | maxX : Int, maxY : Int, maxZ : Int } -> Int -> Point
indexToPoint { maxY, maxZ } index =
    let
        x =
            index // (maxY * maxZ)

        y =
            (index - x * maxY * maxZ) // maxZ

        z =
            index - x * maxY * maxZ - y * maxZ
    in
    ( x, y, z )


pointToPoint3d : Point -> Point3d Length.Meters WorldCoordinates
pointToPoint3d ( x, y, z ) =
    Point3d.meters (toFloat x) (toFloat y) (toFloat z)


point3dToPoint : Point3d Length.Meters WorldCoordinates -> Point
point3dToPoint point =
    let
        parts =
            Point3d.unwrap point
    in
    ( round parts.x, round parts.y, round parts.z )


init : () -> ( Model, Cmd Msg )
init () =
    let
        maxX =
            9

        maxY =
            9

        maxZ =
            9

        sizeHelper =
            { maxX = maxX, maxY = maxY, maxZ = maxZ }

        board =
            { maxX = maxX
            , maxY = maxY
            , maxZ = maxZ
            , blocks =
                List.repeat (maxX * maxY * maxZ) Wall
                    |> List.foldl
                        (\block ( index, blocks ) ->
                            ( index + 1
                            , Dict.insert (indexToPoint sizeHelper index) block blocks
                            )
                        )
                        ( 0, Dict.empty )
                    |> Tuple.second
            }
    in
    ( { xLowerVisible = 0
      , xUpperVisible = maxX - 1
      , yLowerVisible = 0
      , yUpperVisible = maxY - 1
      , zLowerVisible = 0
      , zUpperVisible = maxZ - 1
      , selectedBlockType = Wall
      , board = board
      , score = 0
      , editorBoard = Undo.init board
      , boardLoadError = Nothing
      , boardPlayError = Nothing
      , screenSize = { width = 800, height = 600 }
      , editorCursor = ( 0, 0, 0 )
      , editorKeysDown = Set.empty
      , cameraRotation = Angle.degrees 225
      , cameraElevation = Angle.degrees 25
      , cameraDistance = Length.meters 30
      , cameraFocalPoint =
            Point3d.meters
                ((toFloat maxX - 1) / 2)
                ((toFloat maxY - 1) / 2)
                ((toFloat maxZ - 1) / 2)
      , mouseDragging = NoInteraction
      , cursorBounce =
            Animation.init 0
                [ { value = 0
                  , offset = 0
                  }
                , { value = 1
                  , offset = 500
                  }
                , { value = 0
                  , offset = 500
                  }
                ]
                |> Animation.withLoop
      , editorMode = EditBoard
      , blockEditMode = Select
      , cameraMode = Orbit
      , showBoardBounds = True
      , selectedBlock = Nothing
      , boardEncoding =
            board
                |> Serialize.encodeToJson boardCodec
                |> Json.Encode.encode 0
      , playerFrame = Frame3d.atPoint (Point3d.meters 1 4 7)
      , playerFacing = Forward
      , playerWantFacing = Forward
      , playerMovingAcrossEdge = Nothing
      , editorMaxXRaw = String.fromInt maxX
      , editorMaxYRaw = String.fromInt maxY
      , editorMaxZRaw = String.fromInt maxZ
      , blockPalette = SimpleBlocks
      , inputMapping =
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
            , blockTypePlayerSpawn = ( "f", "" )

            --
            , undo = ( "z", "" )
            , redo = ( "x", "" )

            --
            , toggleSettings = ( ",", "" )
            }
      , showSettings = False
      , screen = Menu
      }
    , Cmd.none
    )


type BoardPlayError
    = MissingPlayerSpawn


type BoardLoadError
    = DataCorrupted
    | SerializerOutOfDate
    | OtherError


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screen of
        Menu ->
            Sub.none

        Game ->
            Sub.none

        FreePlay ->
            Sub.none

        Editor ->
            if model.showSettings then
                Sub.none

            else
                Sub.batch
                    [ Browser.Events.onAnimationFrameDelta Tick
                    , Browser.Events.onKeyPress decodeKeyPressed
                    , Browser.Events.onKeyDown decodeKeyDown
                    , Browser.Events.onKeyUp decodeKeyUp
                    ]


decodeKeyPressed : Json.Decode.Decoder Msg
decodeKeyPressed =
    Json.Decode.map KeyPressed
        (Json.Decode.field "key" Json.Decode.string)


decodeKeyDown : Json.Decode.Decoder Msg
decodeKeyDown =
    Json.Decode.map KeyDown
        (Json.Decode.field "key" Json.Decode.string)


decodeKeyUp : Json.Decode.Decoder Msg
decodeKeyUp =
    Json.Decode.map KeyUp
        (Json.Decode.field "key" Json.Decode.string)


type ScreenCoordinates
    = ScreenCoordinates Never


type Msg
    = NoOp
    | Tick Float
    | KeyPressed String
    | KeyDown String
    | KeyUp String
    | MouseDown Json.Decode.Value
    | MouseUp
    | MouseMove Json.Decode.Value (Point2d Pixels ScreenCoordinates) (Point2d Pixels ScreenCoordinates)
    | EncodingChanged String
    | LoadBoard String
    | ChangeMode
    | SetBlockEditMode BlockEditMode
    | SetCameraMode CameraMode
    | ResetCamera
    | Undo
    | Redo
    | XLowerVisibleChanged Int
    | XUpperVisibleChanged Int
    | YLowerVisibleChanged Int
    | YUpperVisibleChanged Int
    | ZLowerVisibleChanged Int
    | ZUpperVisibleChanged Int
    | BlockTypeSelected Block
    | SetBlock Point Block
    | MaxXChanged String
    | MaxYChanged String
    | MaxZChanged String
    | ShowBoardBounds Bool
    | ShowSettings Bool
    | SetMapping (InputMapping -> InputMapping)
    | SetScreen Screen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick deltaMs ->
            ( model
                |> tickPlayer deltaMs
            , Cmd.none
            )

        EncodingChanged boardEncoding ->
            ( { model | boardEncoding = boardEncoding }, Cmd.none )

        LoadBoard encoding ->
            let
                loadedBoard =
                    encoding
                        |> Json.Decode.decodeString Json.Decode.value
                        |> Result.withDefault Json.Encode.null
                        |> Serialize.decodeFromJson boardCodec
                        |> Result.mapError
                            (\error ->
                                case error of
                                    Serialize.CustomError _ ->
                                        OtherError

                                    Serialize.DataCorrupted ->
                                        DataCorrupted

                                    Serialize.SerializerOutOfDate ->
                                        SerializerOutOfDate
                            )
            in
            case loadedBoard of
                Ok editorBoard ->
                    resetCamera
                        { model
                            | editorBoard = Undo.init editorBoard
                            , boardLoadError = Nothing
                            , selectedBlock = Nothing
                            , xLowerVisible = 0
                            , xUpperVisible = editorBoard.maxX - 1
                            , yLowerVisible = 0
                            , yUpperVisible = editorBoard.maxY - 1
                            , zLowerVisible = 0
                            , zUpperVisible = editorBoard.maxZ - 1
                        }

                Err error ->
                    ( { model
                        | boardLoadError = Just error
                      }
                    , Cmd.none
                    )

        ChangeMode ->
            case model.editorMode of
                EditBoard ->
                    let
                        board =
                            Undo.value model.editorBoard
                    in
                    case findSpawn board of
                        Nothing ->
                            ( { model | boardPlayError = Just MissingPlayerSpawn }, Cmd.none )

                        Just spawnFrame ->
                            ( { model
                                | editorMode = TestGame
                                , board = board
                                , playerFrame = spawnFrame
                                , score = 0
                                , playerMovingAcrossEdge = Nothing
                                , playerFacing = Forward
                                , playerWantFacing = Forward
                                , boardPlayError = Nothing
                              }
                            , Cmd.none
                            )

                TestGame ->
                    ( { model
                        | editorMode = EditBoard
                      }
                    , Cmd.none
                    )

        SetBlockEditMode blockEditMode ->
            ( { model | blockEditMode = blockEditMode }, Cmd.none )

        SetCameraMode cameraMode ->
            ( { model | cameraMode = cameraMode }, Cmd.none )

        ResetCamera ->
            resetCamera model

        Undo ->
            undo model

        Redo ->
            redo model

        MouseDown pointerId ->
            ( { model | mouseDragging = InteractionStart pointerId }, Cmd.none )

        KeyDown key ->
            ( { model | editorKeysDown = Set.insert key model.editorKeysDown }, Cmd.none )

        KeyUp key ->
            ( { model | editorKeysDown = Set.remove key model.editorKeysDown }, Cmd.none )

        MouseUp ->
            if Set.member "Shift" model.editorKeysDown then
                ( { model | mouseDragging = NoInteraction }, Cmd.none )

            else
                case model.blockEditMode of
                    Remove ->
                        let
                            editorBoard =
                                Undo.insertWith
                                    (\board ->
                                        { board
                                            | blocks =
                                                Dict.insert model.editorCursor
                                                    Empty
                                                    board.blocks
                                        }
                                    )
                                    model.editorBoard
                        in
                        ( { model
                            | mouseDragging = NoInteraction
                            , editorBoard = editorBoard
                            , boardEncoding =
                                editorBoard
                                    |> Undo.value
                                    |> Serialize.encodeToJson boardCodec
                                    |> Json.Encode.encode 0
                            , selectedBlock = Nothing
                          }
                        , Cmd.none
                        )

                    Add ->
                        let
                            editorBoard =
                                Undo.insertWith
                                    (\board ->
                                        { board
                                            | blocks =
                                                case model.selectedBlockType of
                                                    PlayerSpawn _ ->
                                                        board.blocks
                                                            |> Dict.map
                                                                (\_ block ->
                                                                    case block of
                                                                        PlayerSpawn _ ->
                                                                            Empty

                                                                        _ ->
                                                                            block
                                                                )
                                                            |> Dict.insert model.editorCursor
                                                                model.selectedBlockType

                                                    _ ->
                                                        Dict.insert model.editorCursor
                                                            model.selectedBlockType
                                                            board.blocks
                                        }
                                    )
                                    model.editorBoard
                        in
                        ( { model
                            | mouseDragging = NoInteraction
                            , editorBoard = editorBoard
                            , boardEncoding =
                                editorBoard
                                    |> Undo.value
                                    |> Serialize.encodeToJson boardCodec
                                    |> Json.Encode.encode 0
                            , selectedBlock =
                                let
                                    board =
                                        editorBoard
                                            |> Undo.value
                                in
                                board.blocks
                                    |> Dict.get model.editorCursor
                                    |> Maybe.map (\block -> ( model.editorCursor, block ))
                            , boardPlayError =
                                case model.boardPlayError of
                                    Nothing ->
                                        model.boardPlayError

                                    Just MissingPlayerSpawn ->
                                        case findSpawn (Undo.value editorBoard) of
                                            Nothing ->
                                                model.boardPlayError

                                            Just _ ->
                                                Nothing
                          }
                        , Cmd.none
                        )

                    Select ->
                        ( { model
                            | mouseDragging = NoInteraction
                            , selectedBlock =
                                let
                                    editorBoard =
                                        model.editorBoard
                                            |> Undo.value
                                in
                                editorBoard.blocks
                                    |> Dict.get model.editorCursor
                                    |> Maybe.map (\block -> ( model.editorCursor, block ))
                          }
                        , Cmd.none
                        )

        MouseMove pointerId offset movement ->
            if Set.member "Shift" model.editorKeysDown then
                moveCameraByMouse pointerId movement model

            else
                moveCursorByMouse offset model

        XLowerVisibleChanged value ->
            ( { model
                | xLowerVisible = value
                , xUpperVisible = max value model.xUpperVisible
              }
            , Cmd.none
            )

        XUpperVisibleChanged value ->
            ( { model
                | xUpperVisible = value
                , xLowerVisible = min value model.xLowerVisible
              }
            , Cmd.none
            )

        YLowerVisibleChanged value ->
            ( { model
                | yLowerVisible = value
                , yUpperVisible = max value model.yUpperVisible
              }
            , Cmd.none
            )

        YUpperVisibleChanged value ->
            ( { model
                | yUpperVisible = value
                , yLowerVisible = min value model.yLowerVisible
              }
            , Cmd.none
            )

        ZLowerVisibleChanged value ->
            ( { model
                | zLowerVisible = value
                , zUpperVisible = max value model.zUpperVisible
              }
            , Cmd.none
            )

        ZUpperVisibleChanged value ->
            ( { model
                | zUpperVisible = value
                , zLowerVisible = min value model.zLowerVisible
              }
            , Cmd.none
            )

        BlockTypeSelected blockType ->
            ( { model | selectedBlockType = blockType }, Cmd.none )

        SetBlock point block ->
            ( { model
                | editorBoard =
                    Undo.insertWith
                        (\board -> { board | blocks = Dict.insert point block board.blocks })
                        model.editorBoard
                , selectedBlock = Just ( point, block )
              }
            , Cmd.none
            )

        MaxXChanged maxXStr ->
            ( case String.toInt maxXStr of
                Nothing ->
                    { model
                        | editorMaxXRaw = maxXStr
                    }

                Just maxX ->
                    let
                        editorBoard =
                            Undo.insertWith
                                (\eb ->
                                    { eb
                                        | maxX = maxX
                                        , blocks =
                                            if maxX < (eb.maxX + 1) then
                                                List.Cartesian.map3 (\x y z -> ( x, y, z ))
                                                    (List.range maxX (eb.maxX + 1))
                                                    (List.range 0 eb.maxY)
                                                    (List.range 0 eb.maxZ)
                                                    |> List.foldl Dict.remove eb.blocks

                                            else
                                                eb.blocks
                                    }
                                )
                                model.editorBoard

                        editorBoardVal =
                            Undo.value editorBoard
                    in
                    { model
                        | editorMaxXRaw = maxXStr
                        , editorBoard = editorBoard
                        , xLowerVisible = min model.xLowerVisible maxX
                        , xUpperVisible =
                            if model.xUpperVisible + 1 == (Undo.value model.editorBoard).maxX then
                                maxX - 1

                            else
                                min model.xUpperVisible (maxX - 1)
                        , cameraFocalPoint =
                            model.cameraFocalPoint
                                |> Point3d.Extra.constrain
                                    { min = Point3d.origin
                                    , max =
                                        Point3d.meters
                                            (toFloat editorBoardVal.maxX - 1)
                                            (toFloat editorBoardVal.maxY - 1)
                                            (toFloat editorBoardVal.maxZ - 1)
                                    }
                    }
            , Cmd.none
            )

        MaxYChanged maxYStr ->
            ( case String.toInt maxYStr of
                Nothing ->
                    { model
                        | editorMaxYRaw = maxYStr
                    }

                Just maxY ->
                    let
                        editorBoard =
                            Undo.insertWith
                                (\eb ->
                                    { eb
                                        | maxY = maxY
                                        , blocks =
                                            if maxY < (eb.maxY + 1) then
                                                List.Cartesian.map3 (\x y z -> ( x, y, z ))
                                                    (List.range 0 eb.maxX)
                                                    (List.range maxY (eb.maxY + 1))
                                                    (List.range 0 eb.maxZ)
                                                    |> List.foldl Dict.remove eb.blocks

                                            else
                                                eb.blocks
                                    }
                                )
                                model.editorBoard

                        editorBoardVal =
                            Undo.value editorBoard
                    in
                    { model
                        | editorMaxYRaw = maxYStr
                        , editorBoard = editorBoard
                        , yLowerVisible = min model.yLowerVisible maxY
                        , yUpperVisible =
                            if model.yUpperVisible + 1 == (Undo.value model.editorBoard).maxY then
                                maxY - 1

                            else
                                min model.yUpperVisible (maxY - 1)
                        , cameraFocalPoint =
                            model.cameraFocalPoint
                                |> Point3d.Extra.constrain
                                    { min = Point3d.origin
                                    , max =
                                        Point3d.meters
                                            (toFloat editorBoardVal.maxX - 1)
                                            (toFloat editorBoardVal.maxY - 1)
                                            (toFloat editorBoardVal.maxZ - 1)
                                    }
                    }
            , Cmd.none
            )

        MaxZChanged maxZStr ->
            ( case String.toInt maxZStr of
                Nothing ->
                    { model
                        | editorMaxZRaw = maxZStr
                    }

                Just maxZ ->
                    let
                        editorBoard =
                            Undo.insertWith
                                (\eb ->
                                    { eb
                                        | maxZ = maxZ
                                        , blocks =
                                            if maxZ < (eb.maxZ + 1) then
                                                List.Cartesian.map3 (\x y z -> ( x, y, z ))
                                                    (List.range 0 eb.maxX)
                                                    (List.range 0 eb.maxY)
                                                    (List.range maxZ (eb.maxZ + 1))
                                                    |> List.foldl Dict.remove eb.blocks

                                            else
                                                eb.blocks
                                    }
                                )
                                model.editorBoard

                        editorBoardVal =
                            Undo.value editorBoard
                    in
                    { model
                        | editorMaxZRaw = maxZStr
                        , editorBoard = editorBoard
                        , zLowerVisible = min model.zLowerVisible maxZ
                        , zUpperVisible =
                            if model.zUpperVisible + 1 == (Undo.value model.editorBoard).maxZ then
                                maxZ - 1

                            else
                                min model.zUpperVisible (maxZ - 1)
                        , cameraFocalPoint =
                            model.cameraFocalPoint
                                |> Point3d.Extra.constrain
                                    { min = Point3d.origin
                                    , max =
                                        Point3d.meters
                                            (toFloat editorBoardVal.maxX - 1)
                                            (toFloat editorBoardVal.maxY - 1)
                                            (toFloat editorBoardVal.maxZ - 1)
                                    }
                    }
            , Cmd.none
            )

        ShowBoardBounds show ->
            ( { model | showBoardBounds = show }, Cmd.none )

        ShowSettings show ->
            showSettings show model

        SetMapping fn ->
            ( { model | inputMapping = fn model.inputMapping }, Cmd.none )

        SetScreen screen ->
            ( { model | screen = screen }, Cmd.none )

        KeyPressed key ->
            case model.editorMode of
                EditBoard ->
                    handleEditorKeyPressed key model

                TestGame ->
                    handleGameKeyPressed key model


showSettings : Bool -> Model -> ( Model, Cmd Msg )
showSettings show model =
    ( { model | showSettings = show }, Cmd.none )


findSpawn : Board -> Maybe (Frame3d Length.Meters WorldCoordinates { defines : {} })
findSpawn board =
    findSpawnHelper (Dict.toList board.blocks)


findSpawnHelper : List ( Point, Block ) -> Maybe (Frame3d Length.Meters WorldCoordinates { defines : {} })
findSpawnHelper blocks =
    case blocks of
        [] ->
            Nothing

        ( point, block ) :: rest ->
            case block of
                PlayerSpawn details ->
                    SketchPlane3d.unsafe
                        { originPoint = pointToPoint3d point
                        , xDirection = axisToDirection3d details.forward
                        , yDirection = axisToDirection3d details.left
                        }
                        |> SketchPlane3d.toFrame
                        |> Just

                _ ->
                    findSpawnHelper rest


moveCameraByMouse : Json.Encode.Value -> Point2d Pixels ScreenCoordinates -> Model -> ( Model, Cmd Msg )
moveCameraByMouse pointerId movement model =
    case model.cameraMode of
        Orbit ->
            ( { model
                | mouseDragging = InteractionMoving pointerId
                , cameraRotation =
                    model.cameraRotation
                        |> Quantity.minus
                            (Point2d.xCoordinate movement
                                |> Pixels.toFloat
                                |> Angle.degrees
                            )
                , cameraElevation =
                    model.cameraElevation
                        |> Quantity.plus
                            (Point2d.yCoordinate movement
                                |> Pixels.toFloat
                                |> Angle.degrees
                            )
              }
            , Cmd.none
            )

        Pan ->
            let
                viewpointFrame =
                    editorViewpoint model
                        |> Viewpoint3d.viewPlane
                        |> SketchPlane3d.toFrame

                editorBoard =
                    Undo.value model.editorBoard
            in
            ( { model
                | cameraFocalPoint =
                    model.cameraFocalPoint
                        |> Point3d.translateIn (Frame3d.zDirection viewpointFrame)
                            (Point2d.yCoordinate movement
                                |> Pixels.toFloat
                                |> (\f -> f / 4)
                                |> Length.meters
                            )
                        |> Point3d.translateIn (Frame3d.xDirection viewpointFrame)
                            (Point2d.xCoordinate movement
                                |> Pixels.toFloat
                                |> (\f -> f / 4)
                                |> Length.meters
                            )
                        |> Point3d.Extra.constrain
                            { min = Point3d.origin
                            , max =
                                Point3d.meters
                                    (toFloat editorBoard.maxX - 1)
                                    (toFloat editorBoard.maxY - 1)
                                    (toFloat editorBoard.maxZ - 1)
                            }
              }
            , Cmd.none
            )

        Zoom ->
            ( { model
                | cameraDistance =
                    model.cameraDistance
                        |> Quantity.plus
                            (Point2d.yCoordinate movement
                                |> Pixels.toFloat
                                |> Length.meters
                            )
                        |> Quantity.min (Length.meters 116)
                        |> Quantity.max (Length.meters 6)
              }
            , Cmd.none
            )


moveCursorByMouse : Point2d Pixels ScreenCoordinates -> Model -> ( Model, Cmd Msg )
moveCursorByMouse offset model =
    let
        ray : Axis3d Length.Meters WorldCoordinates
        ray =
            Camera3d.ray
                (editorCamera model)
                (Rectangle2d.from
                    (Point2d.pixels 0 (toFloat model.screenSize.height))
                    (Point2d.pixels (toFloat model.screenSize.width) 0)
                )
                offset

        editorBoard =
            Undo.value model.editorBoard

        maybeIntersection =
            Dict.foldl
                (\point block maybeInter ->
                    case block of
                        Empty ->
                            maybeInter

                        _ ->
                            let
                                ( x, y, z ) =
                                    point
                            in
                            if x < model.xLowerVisible || x > model.xUpperVisible || y < model.yLowerVisible || y > model.yUpperVisible || z < model.zLowerVisible || z > model.zUpperVisible then
                                maybeInter

                            else
                                let
                                    boundingBox =
                                        BoundingBox3d.withDimensions
                                            ( Length.meters 1, Length.meters 1, Length.meters 1 )
                                            (pointToPoint3d point)
                                in
                                case Axis3d.Extra.intersectionAxisAlignedBoundingBox3d ray boundingBox of
                                    Nothing ->
                                        maybeInter

                                    Just intersection ->
                                        let
                                            newDist =
                                                Point3d.distanceFrom (Axis3d.originPoint ray) (Axis3d.originPoint intersection)
                                        in
                                        case maybeInter of
                                            Nothing ->
                                                Just ( intersection, newDist )

                                            Just ( _, prevDist ) ->
                                                if newDist |> Quantity.lessThan prevDist then
                                                    Just ( intersection, newDist )

                                                else
                                                    maybeInter
                )
                Nothing
                editorBoard.blocks
    in
    case maybeIntersection of
        Nothing ->
            ( model, Cmd.none )

        Just ( intersection, _ ) ->
            ( { model
                | editorCursor =
                    case model.blockEditMode of
                        Remove ->
                            Point3d.along (Axis3d.reverse intersection) (Length.meters 0.5)
                                |> Point3d.Extra.constrain
                                    { min = Point3d.origin
                                    , max =
                                        Point3d.meters
                                            (toFloat editorBoard.maxX - 1)
                                            (toFloat editorBoard.maxY - 1)
                                            (toFloat editorBoard.maxZ - 1)
                                    }
                                |> point3dToPoint

                        Add ->
                            Point3d.along intersection (Length.meters 0.5)
                                |> Point3d.Extra.constrain
                                    { min = Point3d.origin
                                    , max =
                                        Point3d.meters
                                            (toFloat editorBoard.maxX - 1)
                                            (toFloat editorBoard.maxY - 1)
                                            (toFloat editorBoard.maxZ - 1)
                                    }
                                |> point3dToPoint

                        Select ->
                            Point3d.along (Axis3d.reverse intersection) (Length.meters 0.5)
                                |> Point3d.Extra.constrain
                                    { min = Point3d.origin
                                    , max =
                                        Point3d.meters
                                            (toFloat editorBoard.maxX - 1)
                                            (toFloat editorBoard.maxY - 1)
                                            (toFloat editorBoard.maxZ - 1)
                                    }
                                |> point3dToPoint
              }
            , Cmd.none
            )


editorCamera : Model -> Camera3d Length.Meters WorldCoordinates
editorCamera model =
    Camera3d.perspective
        { viewpoint = editorViewpoint model
        , verticalFieldOfView = Angle.degrees 30
        }


editorViewpoint : Model -> Viewpoint3d Length.Meters WorldCoordinates
editorViewpoint model =
    Viewpoint3d.orbit
        { focalPoint = model.cameraFocalPoint
        , azimuth = model.cameraRotation
        , elevation = model.cameraElevation
        , distance = model.cameraDistance
        , groundPlane = SketchPlane3d.xy
        }


tickPlayer : Float -> Model -> Model
tickPlayer deltaMs model =
    case model.editorMode of
        EditBoard ->
            model

        TestGame ->
            model
                |> setPlayerFacing
                |> movePlayer deltaMs


movePlayer : Float -> Model -> Model
movePlayer deltaMs model =
    let
        doEdgeMovement () =
            let
                edgeMovement =
                    Angle.degrees 90
                        |> Quantity.per (Duration.seconds 1)
                        |> Quantity.for (Duration.milliseconds deltaMs)
            in
            ( model.playerFrame
                |> Frame3d.rotateAround
                    (Axis3d.through
                        (model.playerFrame
                            |> Frame3d.originPoint
                            |> point3dToPoint
                            |> pointToPoint3d
                            |> Point3d.translateIn
                                (Frame3d.zDirection model.playerFrame)
                                (Length.meters -1)
                        )
                        (case model.playerFacing of
                            Forward ->
                                Frame3d.yDirection model.playerFrame

                            Backward ->
                                Frame3d.yDirection model.playerFrame
                                    |> Direction3d.reverse

                            Left ->
                                Frame3d.xDirection model.playerFrame
                                    |> Direction3d.reverse

                            Right ->
                                Frame3d.xDirection model.playerFrame
                        )
                    )
                    edgeMovement
            , edgeMovement
            )

        treatAsEmpty () =
            { model
                | playerFrame =
                    model.playerFrame
                        |> Frame3d.translateIn
                            (case model.playerFacing of
                                Forward ->
                                    Frame3d.xDirection model.playerFrame

                                Backward ->
                                    Frame3d.xDirection model.playerFrame
                                        |> Direction3d.reverse

                                Right ->
                                    Frame3d.yDirection model.playerFrame
                                        |> Direction3d.reverse

                                Left ->
                                    Frame3d.yDirection model.playerFrame
                            )
                            (Length.meters 4
                                |> Quantity.per (Duration.seconds 1)
                                |> Quantity.for (Duration.milliseconds deltaMs)
                            )
            }
    in
    case model.playerMovingAcrossEdge of
        Nothing ->
            let
                nearBlock =
                    Dict.get
                        (model.playerFrame
                            |> Frame3d.translateIn
                                (case model.playerFacing of
                                    Forward ->
                                        Frame3d.xDirection model.playerFrame

                                    Backward ->
                                        Frame3d.xDirection model.playerFrame
                                            |> Direction3d.reverse

                                    Right ->
                                        Frame3d.yDirection model.playerFrame
                                            |> Direction3d.reverse

                                    Left ->
                                        Frame3d.yDirection model.playerFrame
                                )
                                (Length.meters 0.6)
                            |> Frame3d.originPoint
                            |> point3dToPoint
                        )
                        model.board.blocks

                scorePoints m =
                    let
                        playerPoint =
                            m.playerFrame
                                |> Frame3d.originPoint

                        blockPoint =
                            playerPoint
                                |> point3dToPoint

                        currentBlock =
                            Dict.get
                                blockPoint
                                m.board.blocks
                    in
                    case currentBlock of
                        Nothing ->
                            m

                        Just Empty ->
                            m

                        Just Wall ->
                            m

                        Just Edge ->
                            m

                        Just (PlayerSpawn _) ->
                            m

                        Just (PointPickup collected) ->
                            if collected then
                                m

                            else if Point3d.distanceFrom playerPoint (pointToPoint3d blockPoint) |> Quantity.lessThan (Length.meters 0.25) then
                                let
                                    board =
                                        m.board
                                in
                                { m
                                    | score = m.score + 50
                                    , board =
                                        { board
                                            | blocks =
                                                Dict.insert blockPoint
                                                    (PointPickup True)
                                                    m.board.blocks
                                        }
                                }

                            else
                                m
            in
            scorePoints <|
                case nearBlock of
                    Nothing ->
                        model

                    Just Wall ->
                        model

                    Just Edge ->
                        let
                            ( playerFrame, distMoved ) =
                                doEdgeMovement ()
                        in
                        { model
                            | playerFrame = playerFrame
                            , playerMovingAcrossEdge = Just distMoved
                        }

                    Just (PointPickup collected) ->
                        if collected then
                            treatAsEmpty ()

                        else
                            { model
                                | playerFrame =
                                    model.playerFrame
                                        |> Frame3d.translateIn
                                            (case model.playerFacing of
                                                Forward ->
                                                    Frame3d.xDirection model.playerFrame

                                                Backward ->
                                                    Frame3d.xDirection model.playerFrame
                                                        |> Direction3d.reverse

                                                Right ->
                                                    Frame3d.yDirection model.playerFrame
                                                        |> Direction3d.reverse

                                                Left ->
                                                    Frame3d.yDirection model.playerFrame
                                            )
                                            (Length.meters 4
                                                |> Quantity.per (Duration.seconds 1)
                                                |> Quantity.for (Duration.milliseconds deltaMs)
                                            )
                            }

                    Just Empty ->
                        treatAsEmpty ()

                    Just (PlayerSpawn _) ->
                        treatAsEmpty ()

        Just edgeDistTraveled ->
            let
                ( playerFrame, distMoved ) =
                    doEdgeMovement ()

                totalMovement =
                    Quantity.plus edgeDistTraveled distMoved

                edgeTravelComplete =
                    Quantity.equalWithin (Angle.degrees 1)
                        totalMovement
                        (Angle.degrees 90)
            in
            { model
                | playerFrame =
                    if edgeTravelComplete then
                        Frame3d.unsafe
                            { originPoint =
                                playerFrame
                                    |> Frame3d.originPoint
                                    |> point3dToPoint
                                    |> pointToPoint3d
                            , xDirection =
                                playerFrame
                                    |> Frame3d.xDirection
                                    |> Direction3d.toVector
                                    |> Vector3d.normalize
                                    |> Vector3d.direction
                                    |> Maybe.withDefault Direction3d.positiveX
                                    |> correctSizeDirection
                            , yDirection =
                                playerFrame
                                    |> Frame3d.yDirection
                                    |> Direction3d.toVector
                                    |> Vector3d.normalize
                                    |> Vector3d.direction
                                    |> Maybe.withDefault Direction3d.positiveX
                                    |> correctSizeDirection
                            , zDirection =
                                playerFrame
                                    |> Frame3d.zDirection
                                    |> Direction3d.toVector
                                    |> Vector3d.normalize
                                    |> Vector3d.direction
                                    |> Maybe.withDefault Direction3d.positiveX
                                    |> correctSizeDirection
                            }

                    else
                        playerFrame
                , playerMovingAcrossEdge =
                    if edgeTravelComplete then
                        Nothing

                    else
                        Just totalMovement
            }


{-| Converts a directin like ( x: 0.9, y: 0.05, z: 0.05 ) to ( x: 1.0, y: 0.0, z: 0.0 )
-}
correctSizeDirection : Direction3d coordinates -> Direction3d coordinates
correctSizeDirection dir =
    let
        parts =
            Direction3d.unwrap dir
    in
    Direction3d.unsafe
        { x = toFloat (round parts.x)
        , y = toFloat (round parts.y)
        , z = toFloat (round parts.z)
        }


setPlayerFacing : Model -> Model
setPlayerFacing model =
    if model.playerFacing == model.playerWantFacing then
        model

    else
        case model.playerMovingAcrossEdge of
            Just _ ->
                model

            Nothing ->
                if oppositeFacings model.playerFacing model.playerWantFacing then
                    { model | playerFacing = model.playerWantFacing }

                else
                    let
                        playerBoardPoint =
                            model.playerFrame
                                |> Frame3d.originPoint
                                |> point3dToPoint
                    in
                    if
                        Quantity.equalWithin (Length.meters 0.1)
                            (Point3d.distanceFrom (pointToPoint3d playerBoardPoint) (Frame3d.originPoint model.playerFrame))
                            (Length.meters 0)
                    then
                        let
                            targetBoardPoint =
                                playerBoardPoint
                                    |> pointToPoint3d
                                    |> Point3d.translateIn
                                        (case model.playerWantFacing of
                                            Forward ->
                                                Frame3d.xDirection model.playerFrame

                                            Backward ->
                                                Frame3d.xDirection model.playerFrame
                                                    |> Direction3d.reverse

                                            Left ->
                                                Frame3d.yDirection model.playerFrame

                                            Right ->
                                                Frame3d.yDirection model.playerFrame
                                                    |> Direction3d.reverse
                                        )
                                        (Length.meters 1)
                                    |> point3dToPoint

                            treatAsEmpty () =
                                { model
                                    | playerFrame =
                                        Frame3d.unsafe
                                            { originPoint = pointToPoint3d playerBoardPoint
                                            , xDirection =
                                                model.playerFrame
                                                    |> Frame3d.xDirection
                                                    |> Direction3d.toVector
                                                    |> Vector3d.normalize
                                                    |> Vector3d.direction
                                                    |> Maybe.withDefault Direction3d.positiveX
                                                    |> correctSizeDirection
                                            , yDirection =
                                                model.playerFrame
                                                    |> Frame3d.yDirection
                                                    |> Direction3d.toVector
                                                    |> Vector3d.normalize
                                                    |> Vector3d.direction
                                                    |> Maybe.withDefault Direction3d.positiveX
                                                    |> correctSizeDirection
                                            , zDirection =
                                                model.playerFrame
                                                    |> Frame3d.zDirection
                                                    |> Direction3d.toVector
                                                    |> Vector3d.normalize
                                                    |> Vector3d.direction
                                                    |> Maybe.withDefault Direction3d.positiveX
                                                    |> correctSizeDirection
                                            }
                                    , playerFacing = model.playerWantFacing
                                }
                        in
                        case Dict.get targetBoardPoint model.board.blocks of
                            Nothing ->
                                model

                            Just block ->
                                case block of
                                    Wall ->
                                        model

                                    PointPickup _ ->
                                        treatAsEmpty ()

                                    Empty ->
                                        treatAsEmpty ()

                                    PlayerSpawn _ ->
                                        treatAsEmpty ()

                                    Edge ->
                                        { model
                                            | playerFrame =
                                                Frame3d.unsafe
                                                    { originPoint = pointToPoint3d playerBoardPoint
                                                    , xDirection = Frame3d.xDirection model.playerFrame
                                                    , yDirection = Frame3d.yDirection model.playerFrame
                                                    , zDirection = Frame3d.zDirection model.playerFrame
                                                    }
                                            , playerFacing = model.playerWantFacing
                                        }

                    else
                        model


oppositeFacings : Facing -> Facing -> Bool
oppositeFacings faceA faceB =
    case ( faceA, faceB ) of
        ( Forward, Backward ) ->
            True

        ( Backward, Forward ) ->
            True

        ( Left, Right ) ->
            True

        ( Right, Left ) ->
            True

        _ ->
            False


resetCamera : Model -> ( Model, Cmd Msg )
resetCamera model =
    let
        editorBoard =
            Undo.value model.editorBoard
    in
    ( { model
        | cameraRotation = Angle.degrees 225
        , cameraElevation = Angle.degrees 25
        , cameraDistance = Length.meters 30
        , cameraFocalPoint =
            Point3d.meters
                ((toFloat editorBoard.maxX - 1) / 2)
                ((toFloat editorBoard.maxY - 1) / 2)
                ((toFloat editorBoard.maxZ - 1) / 2)
      }
    , Cmd.none
    )


undo : Model -> ( Model, Cmd Msg )
undo model =
    ( { model
        | editorBoard = Undo.undo model.editorBoard
        , selectedBlock = Nothing
      }
    , Cmd.none
    )


redo : Model -> ( Model, Cmd Msg )
redo model =
    ( { model
        | editorBoard = Undo.redo model.editorBoard
        , selectedBlock = Nothing
      }
    , Cmd.none
    )


handleEditorKeyPressed : String -> Model -> ( Model, Cmd Msg )
handleEditorKeyPressed key model =
    if isInputKey model.inputMapping.cameraOrbit key then
        ( { model | cameraMode = Orbit }, Cmd.none )

    else if isInputKey model.inputMapping.cameraPan key then
        ( { model | cameraMode = Pan }, Cmd.none )

    else if isInputKey model.inputMapping.cameraZoom key then
        ( { model | cameraMode = Zoom }, Cmd.none )

    else if isInputKey model.inputMapping.cameraReset key then
        resetCamera model

    else if isInputKey model.inputMapping.blockSelect key then
        ( { model | blockEditMode = Select }, Cmd.none )

    else if isInputKey model.inputMapping.blockAdd key then
        ( { model | blockEditMode = Add }, Cmd.none )

    else if isInputKey model.inputMapping.blockRemove key then
        ( { model | blockEditMode = Remove }, Cmd.none )

    else if isInputKey model.inputMapping.blockTypeWall key then
        ( { model | selectedBlockType = Wall }, Cmd.none )

    else if isInputKey model.inputMapping.blockTypeEdge key then
        ( { model | selectedBlockType = Edge }, Cmd.none )

    else if isInputKey model.inputMapping.blockTypePointPickup key then
        ( { model | selectedBlockType = PointPickup False }, Cmd.none )

    else if isInputKey model.inputMapping.blockTypePlayerSpawn key then
        ( { model | selectedBlockType = PlayerSpawn { forward = PositiveX, left = PositiveY } }, Cmd.none )

    else if isInputKey model.inputMapping.undo key then
        undo model

    else if isInputKey model.inputMapping.redo key then
        redo model

    else if isInputKey model.inputMapping.toggleSettings key then
        showSettings (not model.showSettings) model

    else if key == "p" then
        ( { model
            | blockPalette =
                case model.blockPalette of
                    SimpleBlocks ->
                        RainbowBlocks

                    RainbowBlocks ->
                        SimpleBlocks
          }
        , Cmd.none
        )

    else
        ( model, Cmd.none )


handleGameKeyPressed : String -> Model -> ( Model, Cmd Msg )
handleGameKeyPressed key model =
    if isInputKey model.inputMapping.moveUp key then
        ( { model | playerWantFacing = Forward }, Cmd.none )

    else if isInputKey model.inputMapping.moveDown key then
        ( { model | playerWantFacing = Backward }, Cmd.none )

    else if isInputKey model.inputMapping.moveLeft key then
        ( { model | playerWantFacing = Left }, Cmd.none )

    else if isInputKey model.inputMapping.moveRight key then
        ( { model | playerWantFacing = Right }, Cmd.none )

    else
        ( model, Cmd.none )


isInputKey : ( String, String ) -> String -> Bool
isInputKey ( primary, secondary ) key =
    key == primary || key == secondary


decodeMouseDown : Json.Decode.Decoder Msg
decodeMouseDown =
    Json.Decode.field "button" Json.Decode.int
        |> Json.Decode.andThen
            (\button ->
                if button == 0 then
                    Json.Decode.map MouseDown
                        (Json.Decode.field "pointerId" Json.Decode.value)

                else
                    Json.Decode.fail "Non-primary mouse button"
            )


decodeMouseUp : Json.Decode.Decoder Msg
decodeMouseUp =
    Json.Decode.field "button" Json.Decode.int
        |> Json.Decode.andThen
            (\button ->
                if button == 0 then
                    Json.Decode.succeed MouseUp

                else
                    Json.Decode.fail "Non-primary mouse button"
            )


decodePointerMove : Json.Decode.Value -> Json.Decode.Decoder Msg
decodePointerMove pointer =
    Json.Decode.map4
        (\ox oy mx my ->
            MouseMove pointer
                (Point2d.pixels ox oy)
                (Point2d.pixels mx my)
        )
        (Json.Decode.field "offsetX" Json.Decode.float)
        (Json.Decode.field "offsetY" Json.Decode.float)
        (Json.Decode.field "movementX" Json.Decode.float)
        (Json.Decode.field "movementY" Json.Decode.float)


view : Model -> Browser.Document Msg
view model =
    { title = "Cube-Man"
    , body =
        case model.screen of
            Menu ->
                viewStartScreen model

            Game ->
                viewGameScreen model

            FreePlay ->
                viewFreePlayScreen model

            Editor ->
                viewEditorScreen model
    }


viewStartScreen : Model -> List (Html Msg)
viewStartScreen model =
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
                    , Html.Events.onClick (SetScreen Game)
                    ]
                    [ Html.span [ Html.Attributes.style "text-decoration" "line-through" ] [ Html.text "Play" ]
                    , Html.br [] []
                    , Html.small [] [ Html.text " Coming soon..." ]
                    ]
                , Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "padding" "0.5rem 2rem"
                    , Html.Events.onClick (SetScreen FreePlay)
                    ]
                    [ Html.text "Free Play" ]
                , Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "padding" "0.5rem 2rem"
                    , Html.Events.onClick (SetScreen Editor)
                    ]
                    [ Html.text "Level Editor" ]
                ]
            ]
        ]
    ]


viewGameScreen : Model -> List (Html Msg)
viewGameScreen model =
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
            , Html.h2
                [ Html.Attributes.style "text-align" "center"
                ]
                [ Html.text "Full game coming soon..." ]
            , Html.button
                [ Html.Attributes.type_ "button"
                , Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "padding" "0.5rem 2rem"
                , Html.Events.onClick (SetScreen Menu)
                ]
                [ Html.text "Main Menu" ]
            ]
        ]
    ]


viewFreePlayScreen : Model -> List (Html Msg)
viewFreePlayScreen model =
    [ Html.div
        [ Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "align-items" "center"
        ]
        [ Html.div
            [ Html.Attributes.style "margin-top" "5rem"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "gap" "1rem"
            , Html.Attributes.style "align-items" "center"
            ]
            [ Html.h1
                [ Html.Attributes.style "text-align" "center"
                ]
                [ Html.text "Pick a Board" ]
            , Html.button
                [ Html.Attributes.type_ "button"
                , Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "padding" "0.5rem 2rem"
                , Html.Events.onClick (SetScreen Menu)
                ]
                [ Html.text "Main Menu" ]
            ]
        , Html.div
            [ Html.Attributes.style "width" "100vw"
            , Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns" "repeat(5, 1fr)"
            , Html.Attributes.style "gap" "2rem"
            , Html.Attributes.style "padding" "2rem"

            -- , Html.Attributes.style "grid-template-rows" "auto auto"
            ]
            (List.map viewBoardPreviewTile
                [ { name = "Mini"
                  , boardEncoding = basicMiniBoard
                  }
                , { name = "Zig-Zag"
                  , boardEncoding = zigZagBoard
                  }
                ]
                ++ [ Html.div
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "align-items" "center"
                        , Html.Attributes.style "justify-content" "center"
                        , Html.Attributes.style "height" "8rem"
                        ]
                        [ Html.text "More coming soon..." ]
                   ]
            )
        ]
    ]


viewBoardPreviewTile : BoardPreviewTile -> Html Msg
viewBoardPreviewTile board =
    Html.button
        [ Html.Attributes.type_ "button"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "height" "8rem"

        -- , Html.Events.onClick (SetScreen Menu)
        ]
        [ Html.text board.name ]


type alias BoardPreviewTile =
    { name : String
    , boardEncoding : String
    }


viewEditorScreen : Model -> List (Html Msg)
viewEditorScreen model =
    [ Html.div
        [ Html.Attributes.style "display" "grid"
        , Html.Attributes.style "grid-template-columns" <|
            case model.editorMode of
                EditBoard ->
                    "auto auto"

                TestGame ->
                    "auto 8rem"
        , Html.Attributes.style "grid-template-rows" <|
            case model.editorMode of
                EditBoard ->
                    "auto auto"

                TestGame ->
                    "auto"
        ]
        [ Html.div
            ([ Html.Attributes.style "grid-column" <|
                case model.editorMode of
                    EditBoard ->
                        "1"

                    TestGame ->
                        "1 / 2"
             , Html.Attributes.style "grid-row" <|
                case model.editorMode of
                    EditBoard ->
                        "2"

                    TestGame ->
                        "1"
             ]
                ++ (case model.mouseDragging of
                        NoInteraction ->
                            [ Html.Events.on "pointerdown" decodeMouseDown
                            , Html.Events.on "pointermove" (decodePointerMove Json.Encode.null)
                            , Html.Attributes.property "___setPointerCapture" Json.Encode.null
                            ]

                        InteractionStart pointer ->
                            [ Html.Events.on "pointerup" decodeMouseUp
                            , Html.Events.on "pointermove" (decodePointerMove pointer)
                            , Html.Attributes.property "___setPointerCapture" pointer
                            ]

                        InteractionMoving pointer ->
                            [ Html.Events.on "pointerup" decodeMouseUp
                            , Html.Events.on "pointermove" (decodePointerMove pointer)
                            , Html.Attributes.property "___setPointerCapture" pointer
                            ]
                   )
            )
            [ let
                lights =
                    case model.editorMode of
                        TestGame ->
                            let
                                sun1 =
                                    Scene3d.Light.directional (Scene3d.Light.castsShadows True)
                                        { direction =
                                            Direction3d.negativeZ
                                                |> Direction3d.rotateAround Axis3d.x (Angle.degrees 70)
                                        , intensity = Illuminance.lux 80000
                                        , chromaticity = Scene3d.Light.sunlight
                                        }

                                sun2 =
                                    Scene3d.Light.directional (Scene3d.Light.castsShadows True)
                                        { direction =
                                            Direction3d.positiveZ
                                                |> Direction3d.rotateAround Axis3d.x (Angle.degrees -70)
                                        , intensity = Illuminance.lux 80000
                                        , chromaticity = Scene3d.Light.sunlight
                                        }

                                sky1 =
                                    Scene3d.Light.overhead
                                        { upDirection =
                                            Direction3d.positiveZ
                                        , chromaticity = Scene3d.Light.skylight
                                        , intensity = Illuminance.lux 20000
                                        }

                                sky2 =
                                    Scene3d.Light.overhead
                                        { upDirection =
                                            Direction3d.positiveZ
                                                |> Direction3d.rotateAround Axis3d.x (Angle.degrees 70)
                                                |> Direction3d.rotateAround Axis3d.z
                                                    (model.cameraRotation
                                                        |> Quantity.plus (Angle.degrees 90)
                                                    )
                                        , chromaticity = Scene3d.Light.skylight
                                        , intensity = Illuminance.lux 40000
                                        }

                                sky3 =
                                    Scene3d.Light.overhead
                                        { upDirection =
                                            Direction3d.positiveZ
                                                |> Direction3d.rotateAround Axis3d.x (Angle.degrees -70)
                                                |> Direction3d.rotateAround Axis3d.z
                                                    (model.cameraRotation
                                                        |> Quantity.plus (Angle.degrees -90)
                                                    )
                                        , chromaticity = Scene3d.Light.daylight
                                        , intensity = Illuminance.lux 40000
                                        }

                                environment =
                                    Scene3d.Light.overhead
                                        { upDirection = Direction3d.reverse Direction3d.positiveZ
                                        , chromaticity = Scene3d.Light.daylight
                                        , intensity = Illuminance.lux 15000
                                        }
                            in
                            Scene3d.sixLights sun1 sun2 sky1 sky2 sky3 environment

                        EditBoard ->
                            let
                                sun =
                                    Scene3d.Light.directional (Scene3d.Light.castsShadows True)
                                        { direction =
                                            Direction3d.negativeZ
                                                |> Direction3d.rotateAround Axis3d.x (Angle.degrees 70)
                                                |> Direction3d.rotateAround Axis3d.z
                                                    (model.cameraRotation
                                                        |> Quantity.plus (Angle.degrees 90)
                                                    )
                                        , intensity = Illuminance.lux 80000
                                        , chromaticity = Scene3d.Light.sunlight
                                        }

                                sky =
                                    Scene3d.Light.overhead
                                        { upDirection = Direction3d.positiveZ
                                        , chromaticity = Scene3d.Light.skylight
                                        , intensity = Illuminance.lux 20000
                                        }

                                upsideDownSky =
                                    Scene3d.Light.overhead
                                        { upDirection = Direction3d.negativeZ
                                        , chromaticity = Scene3d.Light.skylight
                                        , intensity = Illuminance.lux 40000
                                        }

                                environment =
                                    Scene3d.Light.overhead
                                        { upDirection = Direction3d.reverse Direction3d.positiveZ
                                        , chromaticity = Scene3d.Light.daylight
                                        , intensity = Illuminance.lux 15000
                                        }
                            in
                            Scene3d.fourLights sun sky environment upsideDownSky
              in
              Scene3d.custom
                { clipDepth = Length.meters 1
                , background = Scene3d.backgroundColor Color.gray
                , exposure = Scene3d.exposureValue 15
                , lights = lights
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Scene3d.Light.daylight
                , antialiasing = Scene3d.multisampling
                , dimensions = ( Pixels.int model.screenSize.width, Pixels.int model.screenSize.height )
                , camera =
                    case model.editorMode of
                        TestGame ->
                            Camera3d.perspective
                                { viewpoint =
                                    let
                                        targetPos =
                                            Frame3d.originPoint model.playerFrame
                                    in
                                    Viewpoint3d.lookAt
                                        { focalPoint = targetPos
                                        , eyePoint =
                                            targetPos
                                                |> Point3d.translateIn (Frame3d.zDirection model.playerFrame) (Length.meters 15)
                                        , upDirection = Frame3d.xDirection model.playerFrame
                                        }
                                , verticalFieldOfView = Angle.degrees 30
                                }

                        EditBoard ->
                            editorCamera model
                , entities =
                    case model.editorMode of
                        EditBoard ->
                            let
                                editorBoard =
                                    model.editorBoard
                                        |> Undo.value
                            in
                            List.concat
                                [ editorBoard.blocks
                                    |> Dict.toList
                                    |> List.map (viewBlock editorBoard model)
                                , [ viewCursor
                                        (case model.blockEditMode of
                                            Select ->
                                                Color.white

                                            Remove ->
                                                Color.red

                                            Add ->
                                                Color.green
                                        )
                                        model.cursorBounce
                                        model.editorCursor
                                  , viewOrientationArrows
                                  , case model.selectedBlock of
                                        Nothing ->
                                            Scene3d.nothing

                                        Just ( point, _ ) ->
                                            viewCursor Color.yellow model.cursorBounce point
                                  , if model.showBoardBounds then
                                        viewBounds editorBoard

                                    else
                                        Scene3d.nothing
                                  ]
                                ]

                        TestGame ->
                            List.concat
                                [ model.board.blocks
                                    |> Dict.toList
                                    |> List.map (viewBlock model.board model)
                                , [ viewPlayer model.playerFacing model.playerFrame ]
                                ]
                }
            ]
        , viewHeader model
        , case model.editorMode of
            TestGame ->
                Html.div
                    [ Html.Attributes.style "grid-column" "2"
                    , Html.Attributes.style "grid-row" "1"
                    , Html.Attributes.style "padding" "0.5rem"
                    ]
                    [ Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick ChangeMode
                        ]
                        [ Html.text "Edit Level"
                        ]
                    ]

            EditBoard ->
                let
                    editorBoard =
                        model.editorBoard
                            |> Undo.value
                in
                Html.div
                    [ Html.Attributes.style "grid-column" "2"
                    , Html.Attributes.style "grid-row" "2"
                    , Html.Attributes.style "padding" "0.5rem"
                    , Html.Attributes.style "height" "80vh"
                    , Html.Attributes.style "overflow" "auto"
                    ]
                    [ case model.selectedBlock of
                        Nothing ->
                            Html.span [] [ Html.text "No block selected" ]

                        Just ( point, block ) ->
                            case block of
                                Empty ->
                                    Html.span [] [ Html.text "Empty block" ]

                                Edge ->
                                    Html.span [] [ Html.text "Edge" ]

                                Wall ->
                                    Html.span [] [ Html.text "Wall" ]

                                PointPickup _ ->
                                    Html.span [] [ Html.text "Point Pickup" ]

                                PlayerSpawn details ->
                                    Html.form
                                        []
                                        [ Html.span [] [ Html.text "Player Spawn" ]
                                        , Html.br [] []
                                        , Html.label []
                                            [ Html.span [] [ Html.text "Forward Direction " ]
                                            , Html.Extra.select
                                                []
                                                { value = Just details.forward
                                                , options =
                                                    [ PositiveX
                                                    , NegativeX
                                                    , PositiveY
                                                    , NegativeY
                                                    , PositiveZ
                                                    , NegativeZ
                                                    ]
                                                , toLabel = axisToLabel
                                                , toKey = axisToLabel
                                                , onSelect =
                                                    \value ->
                                                        case value of
                                                            Nothing ->
                                                                SetBlock point (PlayerSpawn details)

                                                            Just axis ->
                                                                SetBlock point (PlayerSpawn { details | forward = axis })
                                                }
                                            ]
                                        , Html.br [] []
                                        , Html.label []
                                            [ Html.span [] [ Html.text "Left Direction " ]
                                            , Html.Extra.select
                                                []
                                                { value = Just details.left
                                                , options =
                                                    [ PositiveX
                                                    , NegativeX
                                                    , PositiveY
                                                    , NegativeY
                                                    , PositiveZ
                                                    , NegativeZ
                                                    ]
                                                , toLabel = axisToLabel
                                                , toKey = axisToLabel
                                                , onSelect =
                                                    \value ->
                                                        case value of
                                                            Nothing ->
                                                                SetBlock point (PlayerSpawn details)

                                                            Just axis ->
                                                                SetBlock point (PlayerSpawn { details | left = axis })
                                                }
                                            ]
                                        ]
                    , Html.hr [] []
                    , Html.form []
                        [ Html.fieldset []
                            [ Html.label []
                                [ Html.span [] [ Html.text "X Size " ]
                                , Html.input
                                    [ Html.Attributes.type_ "number"
                                    , Html.Attributes.value model.editorMaxXRaw
                                    , Html.Attributes.min "1"
                                    , Html.Attributes.max "20"
                                    , Html.Attributes.step "1"
                                    , Html.Events.onInput MaxXChanged
                                    ]
                                    []
                                ]
                            , Html.br [] []
                            , Html.label []
                                [ Html.span [] [ Html.text "X Visibility" ]
                                , Html.Extra.dualRange
                                    []
                                    { valueLow = toFloat model.xLowerVisible
                                    , valueHigh = toFloat model.xUpperVisible
                                    , onInputLow = round >> XLowerVisibleChanged
                                    , onInputHigh = round >> XUpperVisibleChanged
                                    , min = 0
                                    , max = toFloat (editorBoard.maxX - 1)
                                    , step = 1
                                    , colorMin = Nothing
                                    , colorMid = Nothing
                                    , colorMax = Nothing
                                    , thumb1Image = Nothing
                                    , thumb2Image = Nothing
                                    }
                                ]
                            ]
                        , Html.br [] []
                        , Html.fieldset []
                            [ Html.label []
                                [ Html.span [] [ Html.text "Y Size " ]
                                , Html.input
                                    [ Html.Attributes.type_ "number"
                                    , Html.Attributes.value model.editorMaxYRaw
                                    , Html.Attributes.min "1"
                                    , Html.Attributes.max "20"
                                    , Html.Attributes.step "1"
                                    , Html.Events.onInput MaxYChanged
                                    ]
                                    []
                                ]
                            , Html.br [] []
                            , Html.label []
                                [ Html.span [] [ Html.text "Y Visibility" ]
                                , Html.Extra.dualRange
                                    []
                                    { valueLow = toFloat model.yLowerVisible
                                    , valueHigh = toFloat model.yUpperVisible
                                    , onInputLow = round >> YLowerVisibleChanged
                                    , onInputHigh = round >> YUpperVisibleChanged
                                    , min = 0
                                    , max = toFloat (editorBoard.maxY - 1)
                                    , step = 1
                                    , colorMin = Nothing
                                    , colorMid = Nothing
                                    , colorMax = Nothing
                                    , thumb1Image = Nothing
                                    , thumb2Image = Nothing
                                    }
                                ]
                            ]
                        , Html.br [] []
                        , Html.fieldset []
                            [ Html.label []
                                [ Html.span [] [ Html.text "Z Size " ]
                                , Html.input
                                    [ Html.Attributes.type_ "number"
                                    , Html.Attributes.value model.editorMaxZRaw
                                    , Html.Attributes.min "1"
                                    , Html.Attributes.max "20"
                                    , Html.Attributes.step "1"
                                    , Html.Events.onInput MaxZChanged
                                    ]
                                    []
                                ]
                            , Html.br [] []
                            , Html.label []
                                [ Html.span [] [ Html.text "Z Visibility" ]
                                , Html.Extra.dualRange
                                    []
                                    { valueLow = toFloat model.zLowerVisible
                                    , valueHigh = toFloat model.zUpperVisible
                                    , onInputLow = round >> ZLowerVisibleChanged
                                    , onInputHigh = round >> ZUpperVisibleChanged
                                    , min = 0
                                    , max = toFloat (editorBoard.maxZ - 1)
                                    , step = 1
                                    , colorMin = Nothing
                                    , colorMid = Nothing
                                    , colorMax = Nothing
                                    , thumb1Image = Nothing
                                    , thumb2Image = Nothing
                                    }
                                ]
                            ]
                        ]
                    , Html.hr [] []
                    , Html.form
                        [ Html.Events.onSubmit (LoadBoard model.boardEncoding)
                        , Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "gap" "1rem"
                        ]
                        [ Html.label
                            [ Html.Attributes.style "display" "flex"
                            , Html.Attributes.style "gap" "1rem"
                            , Html.Attributes.style "align-items" "center"
                            ]
                            [ Html.span [] [ Html.text "Encoded Level:" ]
                            , Html.input
                                [ Html.Attributes.value model.boardEncoding
                                , Html.Events.onInput EncodingChanged
                                ]
                                []
                            ]
                        , Html.button
                            [ Html.Attributes.type_ "submit" ]
                            [ Html.text "Load" ]
                        , case model.boardLoadError of
                            Nothing ->
                                Html.text ""

                            Just error ->
                                Html.small []
                                    [ Html.text <|
                                        case error of
                                            DataCorrupted ->
                                                "Board data is corrupted"

                                            SerializerOutOfDate ->
                                                "Board data is from a different version of the game"

                                            OtherError ->
                                                "Unexpected error"
                                    ]
                        ]
                    , Html.br [] []
                    , Html.label []
                        [ Html.span [] [ Html.text "Load an example board " ]
                        , Html.Extra.select
                            []
                            { value = Nothing
                            , options =
                                [ DefaultBoard
                                , BasicMiniBoard
                                , ZigZagBoard
                                ]
                            , toLabel =
                                \value ->
                                    case value of
                                        DefaultBoard ->
                                            "9 x 9 blank slate"

                                        BasicMiniBoard ->
                                            "Basic mini"

                                        ZigZagBoard ->
                                            "Zig zag"
                            , toKey =
                                \value ->
                                    case value of
                                        DefaultBoard ->
                                            "DefaultBoard"

                                        BasicMiniBoard ->
                                            "BasicMiniBoard"

                                        ZigZagBoard ->
                                            "ZigZagBoard"
                            , onSelect =
                                \value ->
                                    case value of
                                        Nothing ->
                                            NoOp

                                        Just DefaultBoard ->
                                            LoadBoard defaultBoard

                                        Just BasicMiniBoard ->
                                            LoadBoard basicMiniBoard

                                        Just ZigZagBoard ->
                                            LoadBoard zigZagBoard
                            }
                        ]
                    ]
        ]
    ]


viewInputKeyHoverText : ( String, String ) -> String
viewInputKeyHoverText ( primary, secondary ) =
    if secondary == "" then
        primary

    else
        primary ++ " | " ++ secondary


viewHeader : Model -> Html Msg
viewHeader model =
    case model.editorMode of
        TestGame ->
            Html.div
                [ Html.Attributes.style "grid-column" "1 /3"
                , Html.Attributes.style "grid-row" "1"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "padding" "0.5rem"
                , Html.Attributes.style "gap" "1rem"
                ]
                [ Html.h3 [] [ Html.text ("Score: " ++ String.fromInt model.score) ]
                ]

        EditBoard ->
            Html.div
                [ Html.Attributes.style "grid-column" "1 /3"
                , Html.Attributes.style "grid-row" "1"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "padding" "0.5rem"
                , Html.Attributes.style "gap" "1rem"
                ]
                [ Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Events.onClick ChangeMode
                    ]
                    [ Html.text "Play Level"
                    ]
                , Html.span
                    [ Html.Attributes.style "width" "12rem"
                    , Html.Attributes.style "color" "red"
                    ]
                    [ case model.boardPlayError of
                        Nothing ->
                            Html.text ""

                        Just MissingPlayerSpawn ->
                            Html.text "Missing Player Spawn"
                    ]
                , Html.div
                    [ Html.Attributes.attribute "role" "group" ]
                    [ Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick Undo
                        , Html.Attributes.title ("Undo - " ++ viewInputKeyHoverText model.inputMapping.undo)
                        , Html.Attributes.Extra.aria "disabled" <|
                            Html.Attributes.Extra.bool (not <| Undo.canUndo model.editorBoard)
                        ]
                        [ Phosphor.arrowCounterClockwise Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick Redo
                        , Html.Attributes.title ("Redo - " ++ viewInputKeyHoverText model.inputMapping.redo)
                        , Html.Attributes.Extra.aria "disabled" <|
                            Html.Attributes.Extra.bool (not <| Undo.canRedo model.editorBoard)
                        ]
                        [ Phosphor.arrowClockwise Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    ]
                , Html.div
                    [ Html.Attributes.attribute "role" "group" ]
                    [ Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (SetCameraMode Orbit)
                        , Html.Attributes.title ("Camera orbit - " ++ viewInputKeyHoverText model.inputMapping.cameraOrbit)
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.cameraMode == Orbit)
                        ]
                        [ Phosphor.arrowsClockwise Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (SetCameraMode Pan)
                        , Html.Attributes.title ("Camera pan - " ++ viewInputKeyHoverText model.inputMapping.cameraPan)
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.cameraMode == Pan)
                        ]
                        [ Phosphor.arrowsOutCardinal Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (SetCameraMode Zoom)
                        , Html.Attributes.title ("Camera zoom - " ++ viewInputKeyHoverText model.inputMapping.cameraZoom)
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.cameraMode == Zoom)
                        ]
                        [ Phosphor.arrowsVertical Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick ResetCamera
                        , Html.Attributes.title ("Camera reset - " ++ viewInputKeyHoverText model.inputMapping.cameraReset)
                        ]
                        [ Phosphor.clockCounterClockwise Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    ]
                , Html.div
                    [ Html.Attributes.attribute "role" "group" ]
                    [ Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (SetBlockEditMode Select)
                        , Html.Attributes.title ("Select block - " ++ viewInputKeyHoverText model.inputMapping.blockSelect)
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.blockEditMode == Select)
                        ]
                        [ Phosphor.cursor Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (SetBlockEditMode Add)
                        , Html.Attributes.title ("Add block - " ++ viewInputKeyHoverText model.inputMapping.blockAdd)
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.blockEditMode == Add)
                        ]
                        [ Phosphor.plus Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (SetBlockEditMode Remove)
                        , Html.Attributes.title ("Remove block - " ++ viewInputKeyHoverText model.inputMapping.blockRemove)
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.blockEditMode == Remove)
                        ]
                        [ Phosphor.x Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    ]
                , Html.div
                    [ Html.Attributes.attribute "role" "group"
                    ]
                    [ Html.button
                        [ Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.selectedBlockType == Wall)
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick (BlockTypeSelected Wall)
                        , Html.Attributes.title ("Wall - " ++ viewInputKeyHoverText model.inputMapping.blockTypeWall)
                        ]
                        [ Html.text "Wall"
                        ]
                    , Html.button
                        [ Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.selectedBlockType == Edge)
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick (BlockTypeSelected Edge)
                        , Html.Attributes.title ("Edge - " ++ viewInputKeyHoverText model.inputMapping.blockTypeEdge)
                        ]
                        [ Html.text "Edge"
                        ]
                    , Html.button
                        [ Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.selectedBlockType == PointPickup False)
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick (BlockTypeSelected (PointPickup False))
                        , Html.Attributes.title ("Point Pickup - " ++ viewInputKeyHoverText model.inputMapping.blockTypePointPickup)
                        ]
                        [ Html.text "Point Pickup"
                        ]
                    , Html.button
                        [ Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.selectedBlockType == PlayerSpawn { forward = PositiveX, left = PositiveY })
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick (BlockTypeSelected (PlayerSpawn { forward = PositiveX, left = PositiveY }))
                        , Html.Attributes.title ("Player Spawn - " ++ viewInputKeyHoverText model.inputMapping.blockTypePlayerSpawn)
                        ]
                        [ Html.text "Player Spawn"
                        ]
                    ]
                , Html.button
                    [ Html.Attributes.Extra.aria "pressed" <|
                        Html.Attributes.Extra.bool model.showBoardBounds
                    , Html.Attributes.type_ "button"
                    , Html.Events.onClick (ShowBoardBounds (not model.showBoardBounds))
                    , Html.Attributes.title "Show board bounds"
                    ]
                    [ Phosphor.gridNine Phosphor.Regular
                        |> Phosphor.toHtml []
                    ]
                , Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Events.onClick (ShowSettings True)
                    , Html.Attributes.title ("Settings - " ++ viewInputKeyHoverText model.inputMapping.toggleSettings)
                    ]
                    [ Phosphor.gear Phosphor.Regular
                        |> Phosphor.toHtml []
                    ]
                , Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Events.onClick (SetScreen Menu)
                    , Html.Attributes.style "margin-left" "auto"
                    ]
                    [ Html.text "Exit" ]
                , Html.Extra.modal { open = model.showSettings, onClose = ShowSettings False }
                    []
                    [ Html.h2
                        [ Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "margin-top" "0"
                        ]
                        [ Html.text "Settings"
                        , Html.button
                            [ Html.Attributes.style "float" "right"
                            , Html.Attributes.style "background" "none"
                            , Html.Attributes.type_ "button"
                            , Html.Attributes.title "Close"
                            , Html.Events.onClick (ShowSettings False)
                            ]
                            [ Phosphor.xCircle Phosphor.Regular
                                |> Phosphor.toHtml []
                            ]
                        ]
                    , let
                        viewMapping mapping =
                            let
                                ( primary, secondary ) =
                                    mapping.keys
                            in
                            Html.tr []
                                [ Html.th [ Html.Attributes.attribute "align" "left" ] [ Html.text mapping.label ]
                                , Html.td [ Html.Attributes.attribute "align" "center" ]
                                    [ Html.input
                                        [ Html.Attributes.value primary
                                        , Html.Attributes.placeholder "Must be set"
                                        , Html.Events.custom "input" (decodeInputMappingChange mapping.setPrimary)
                                        , Html.Attributes.style "text-align" "center"
                                        ]
                                        []
                                    ]
                                , Html.td [ Html.Attributes.attribute "align" "center" ]
                                    [ Html.input
                                        [ Html.Attributes.value secondary
                                        , Html.Attributes.placeholder "Not set"
                                        , Html.Events.custom "input" (decodeInputMappingChange mapping.setSecondary)
                                        , Html.Attributes.style "text-align" "center"
                                        ]
                                        []
                                    ]
                                ]
                      in
                      Html.table
                        []
                        [ Html.thead []
                            [ Html.tr []
                                [ Html.th [ Html.Attributes.attribute "align" "left" ] [ Html.text "Input" ]
                                , Html.th [] [ Html.text "Primary Key" ]
                                , Html.th [] [ Html.text "Secondary Key" ]
                                ]
                            ]
                        , Html.tbody []
                            (Html.tr []
                                [ Html.th [] [ Html.h3 [] [ Html.text "Character Movement" ] ]
                                ]
                                :: List.map viewMapping
                                    [ { label = "Face up"
                                      , keys = model.inputMapping.moveUp
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.moveUp
                                                in
                                                { inputMapping | moveUp = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.moveUp
                                                in
                                                { inputMapping | moveUp = ( primary, key ) }
                                      }
                                    , { label = "Face down"
                                      , keys = model.inputMapping.moveDown
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.moveDown
                                                in
                                                { inputMapping | moveDown = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.moveDown
                                                in
                                                { inputMapping | moveDown = ( primary, key ) }
                                      }
                                    , { label = "Face left"
                                      , keys = model.inputMapping.moveLeft
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.moveLeft
                                                in
                                                { inputMapping | moveLeft = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.moveLeft
                                                in
                                                { inputMapping | moveLeft = ( primary, key ) }
                                      }
                                    , { label = "Face right"
                                      , keys = model.inputMapping.moveRight
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.moveRight
                                                in
                                                { inputMapping | moveRight = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.moveRight
                                                in
                                                { inputMapping | moveRight = ( primary, key ) }
                                      }
                                    ]
                                ++ [ Html.tr []
                                        [ Html.th [] [ Html.h3 [] [ Html.text "Editor" ] ]
                                        ]
                                   , Html.tr []
                                        [ Html.th [] [ Html.text "Camera" ]
                                        ]
                                   ]
                                ++ List.map viewMapping
                                    [ { label = "Orbit"
                                      , keys = model.inputMapping.cameraOrbit
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.cameraOrbit
                                                in
                                                { inputMapping | cameraOrbit = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.cameraOrbit
                                                in
                                                { inputMapping | cameraOrbit = ( primary, key ) }
                                      }
                                    , { label = "Pan"
                                      , keys = model.inputMapping.cameraPan
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.cameraPan
                                                in
                                                { inputMapping | cameraPan = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.cameraPan
                                                in
                                                { inputMapping | cameraPan = ( primary, key ) }
                                      }
                                    , { label = "Zoom"
                                      , keys = model.inputMapping.cameraZoom
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.cameraZoom
                                                in
                                                { inputMapping | cameraZoom = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.cameraZoom
                                                in
                                                { inputMapping | cameraZoom = ( primary, key ) }
                                      }
                                    , { label = "Reset"
                                      , keys = model.inputMapping.cameraReset
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.cameraReset
                                                in
                                                { inputMapping | cameraReset = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.cameraReset
                                                in
                                                { inputMapping | cameraReset = ( primary, key ) }
                                      }
                                    ]
                                ++ [ Html.tr []
                                        [ Html.th [] [ Html.text "Block Edit" ]
                                        ]
                                   ]
                                ++ List.map viewMapping
                                    [ { label = "Select"
                                      , keys = model.inputMapping.blockSelect
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.blockSelect
                                                in
                                                { inputMapping | blockSelect = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.blockSelect
                                                in
                                                { inputMapping | blockSelect = ( primary, key ) }
                                      }
                                    , { label = "Add"
                                      , keys = model.inputMapping.blockAdd
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.blockAdd
                                                in
                                                { inputMapping | blockAdd = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.blockAdd
                                                in
                                                { inputMapping | blockAdd = ( primary, key ) }
                                      }
                                    , { label = "Remove"
                                      , keys = model.inputMapping.blockRemove
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.blockRemove
                                                in
                                                { inputMapping | blockRemove = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.blockRemove
                                                in
                                                { inputMapping | blockRemove = ( primary, key ) }
                                      }
                                    ]
                                ++ [ Html.tr []
                                        [ Html.th [] [ Html.text "Block Type" ]
                                        ]
                                   ]
                                ++ List.map viewMapping
                                    [ { label = "Wall"
                                      , keys = model.inputMapping.blockTypeWall
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.blockTypeWall
                                                in
                                                { inputMapping | blockTypeWall = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.blockTypeWall
                                                in
                                                { inputMapping | blockTypeWall = ( primary, key ) }
                                      }
                                    , { label = "Edge"
                                      , keys = model.inputMapping.blockTypeEdge
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.blockTypeEdge
                                                in
                                                { inputMapping | blockTypeEdge = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.blockTypeEdge
                                                in
                                                { inputMapping | blockTypeEdge = ( primary, key ) }
                                      }
                                    , { label = "Point Pickup"
                                      , keys = model.inputMapping.blockTypePointPickup
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.blockTypePointPickup
                                                in
                                                { inputMapping | blockTypePointPickup = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.blockTypePointPickup
                                                in
                                                { inputMapping | blockTypePointPickup = ( primary, key ) }
                                      }
                                    , { label = "Player Spawn"
                                      , keys = model.inputMapping.blockTypePlayerSpawn
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.blockTypePlayerSpawn
                                                in
                                                { inputMapping | blockTypePlayerSpawn = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.blockTypePlayerSpawn
                                                in
                                                { inputMapping | blockTypePlayerSpawn = ( primary, key ) }
                                      }
                                    ]
                                ++ [ Html.tr []
                                        [ Html.th [] [ Html.text "Undo / Redo" ]
                                        ]
                                   ]
                                ++ List.map viewMapping
                                    [ { label = "Undo"
                                      , keys = model.inputMapping.undo
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.undo
                                                in
                                                { inputMapping | undo = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.undo
                                                in
                                                { inputMapping | undo = ( primary, key ) }
                                      }
                                    , { label = "Redo"
                                      , keys = model.inputMapping.redo
                                      , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.redo
                                                in
                                                { inputMapping | redo = ( key, secondary ) }
                                      , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.redo
                                                in
                                                { inputMapping | redo = ( primary, key ) }
                                      }
                                    ]
                                ++ [ Html.tr []
                                        [ Html.th [] [ Html.text "Other" ]
                                        ]
                                   , viewMapping
                                        { label = "Open Settings"
                                        , keys = model.inputMapping.toggleSettings
                                        , setPrimary =
                                            \key inputMapping ->
                                                let
                                                    ( _, secondary ) =
                                                        inputMapping.toggleSettings
                                                in
                                                { inputMapping | toggleSettings = ( key, secondary ) }
                                        , setSecondary =
                                            \key inputMapping ->
                                                let
                                                    ( primary, _ ) =
                                                        inputMapping.toggleSettings
                                                in
                                                { inputMapping | toggleSettings = ( primary, key ) }
                                        }
                                   ]
                            )
                        ]
                    ]
                ]


decodeInputMappingChange : (String -> InputMapping -> InputMapping) -> Json.Decode.Decoder { message : Msg, preventDefault : Bool, stopPropagation : Bool }
decodeInputMappingChange fn =
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
                        { message = SetMapping (fn trimmed)
                        , preventDefault = True
                        , stopPropagation = True
                        }
            )


type ExampleBoards
    = DefaultBoard
    | BasicMiniBoard
    | ZigZagBoard


viewBounds : Board -> Scene3d.Entity WorldCoordinates
viewBounds board =
    Scene3d.group
        (List.concat
            [ List.map
                (\y ->
                    Scene3d.lineSegment
                        (Scene3d.Material.color Color.orange)
                        (LineSegment3d.from
                            (Point3d.meters -0.5 (toFloat y - 0.5) -0.5)
                            (Point3d.meters (toFloat board.maxX - 0.5) (toFloat y - 0.5) -0.5)
                        )
                )
                (List.range 0 board.maxY)
            , List.map
                (\x ->
                    Scene3d.lineSegment
                        (Scene3d.Material.color Color.orange)
                        (LineSegment3d.from
                            (Point3d.meters (toFloat x - 0.5) -0.5 -0.5)
                            (Point3d.meters (toFloat x - 0.5) (toFloat board.maxY - 0.5) -0.5)
                        )
                )
                (List.range 0 board.maxX)
            , [ Scene3d.lineSegment
                    (Scene3d.Material.color Color.orange)
                    (LineSegment3d.from
                        (Point3d.meters -0.5 -0.5 (toFloat board.maxZ - 0.5))
                        (Point3d.meters (toFloat board.maxX - 0.5) -0.5 (toFloat board.maxZ - 0.5))
                    )
              , Scene3d.lineSegment
                    (Scene3d.Material.color Color.orange)
                    (LineSegment3d.from
                        (Point3d.meters -0.5 (toFloat board.maxY - 0.5) (toFloat board.maxZ - 0.5))
                        (Point3d.meters (toFloat board.maxX - 0.5) (toFloat board.maxY - 0.5) (toFloat board.maxZ - 0.5))
                    )
              , Scene3d.lineSegment
                    (Scene3d.Material.color Color.orange)
                    (LineSegment3d.from
                        (Point3d.meters -0.5 -0.5 (toFloat board.maxZ - 0.5))
                        (Point3d.meters -0.5 (toFloat board.maxY - 0.5) (toFloat board.maxZ - 0.5))
                    )
              , Scene3d.lineSegment
                    (Scene3d.Material.color Color.orange)
                    (LineSegment3d.from
                        (Point3d.meters (toFloat board.maxX - 0.5) -0.5 (toFloat board.maxZ - 0.5))
                        (Point3d.meters (toFloat board.maxX - 0.5) (toFloat board.maxY - 0.5) (toFloat board.maxZ - 0.5))
                    )
              , Scene3d.lineSegment
                    (Scene3d.Material.color Color.orange)
                    (LineSegment3d.from
                        (Point3d.meters -0.5 -0.5 -0.5)
                        (Point3d.meters -0.5 -0.5 (toFloat board.maxZ - 0.5))
                    )
              , Scene3d.lineSegment
                    (Scene3d.Material.color Color.orange)
                    (LineSegment3d.from
                        (Point3d.meters (toFloat board.maxX - 0.5) -0.5 -0.5)
                        (Point3d.meters (toFloat board.maxX - 0.5) -0.5 (toFloat board.maxZ - 0.5))
                    )
              , Scene3d.lineSegment
                    (Scene3d.Material.color Color.orange)
                    (LineSegment3d.from
                        (Point3d.meters -0.5 (toFloat board.maxY - 0.5) -0.5)
                        (Point3d.meters -0.5 (toFloat board.maxY - 0.5) (toFloat board.maxZ - 0.5))
                    )
              , Scene3d.lineSegment
                    (Scene3d.Material.color Color.orange)
                    (LineSegment3d.from
                        (Point3d.meters (toFloat board.maxX - 0.5) (toFloat board.maxY - 0.5) -0.5)
                        (Point3d.meters (toFloat board.maxX - 0.5) (toFloat board.maxY - 0.5) (toFloat board.maxZ - 0.5))
                    )
              ]
            ]
        )


viewOrientationArrows : Scene3d.Entity WorldCoordinates
viewOrientationArrows =
    Scene3d.group
        [ Scene3d.lineSegment
            (Scene3d.Material.color Color.red)
            (LineSegment3d.from
                (Point3d.meters -1 -1 -0.5)
                (Point3d.meters 1 -1 -0.5)
            )
        , Scene3d.cone
            (Scene3d.Material.matte Color.red)
            (Cone3d.startingAt (Point3d.meters 1 -1 -0.5)
                Direction3d.positiveX
                { radius = Length.meters 0.125
                , length = Length.meters 0.25
                }
            )
        , Scene3d.lineSegment
            (Scene3d.Material.color Color.green)
            (LineSegment3d.from
                (Point3d.meters -1 -1 -0.5)
                (Point3d.meters -1 1 -0.5)
            )
        , Scene3d.cone
            (Scene3d.Material.matte Color.green)
            (Cone3d.startingAt (Point3d.meters -1 1 -0.5)
                Direction3d.positiveY
                { radius = Length.meters 0.125
                , length = Length.meters 0.25
                }
            )
        , Scene3d.lineSegment
            (Scene3d.Material.color Color.blue)
            (LineSegment3d.from
                (Point3d.meters -1 -1 -0.5)
                (Point3d.meters -1 -1 1.5)
            )
        , Scene3d.cone
            (Scene3d.Material.matte Color.blue)
            (Cone3d.startingAt (Point3d.meters -1 -1 1.5)
                Direction3d.positiveZ
                { radius = Length.meters 0.125
                , length = Length.meters 0.25
                }
            )
        ]


type Axis
    = PositiveX
    | NegativeX
    | PositiveY
    | NegativeY
    | PositiveZ
    | NegativeZ


axisToLabel : Axis -> String
axisToLabel axis =
    case axis of
        PositiveX ->
            "Positive X"

        NegativeX ->
            "Negative X"

        PositiveY ->
            "Positive Y"

        NegativeY ->
            "Negative Y"

        PositiveZ ->
            "Positive Z"

        NegativeZ ->
            "Negative Z"


axisToDirection3d : Axis -> Direction3d coordinates
axisToDirection3d axis =
    case axis of
        PositiveX ->
            Direction3d.positiveX

        NegativeX ->
            Direction3d.negativeX

        PositiveY ->
            Direction3d.positiveY

        NegativeY ->
            Direction3d.negativeY

        PositiveZ ->
            Direction3d.positiveZ

        NegativeZ ->
            Direction3d.negativeZ


viewPlayer : Facing -> Frame3d Length.Meters WorldCoordinates { defines : {} } -> Scene3d.Entity WorldCoordinates
viewPlayer facing frame =
    Scene3d.group
        [ Scene3d.sphereWithShadow
            (Scene3d.Material.matte Color.gray)
            (Sphere3d.atPoint Point3d.origin
                (Length.meters 0.5)
                |> Sphere3d.placeIn frame
            )
        , Scene3d.coneWithShadow
            (Scene3d.Material.matte Color.red)
            (Cone3d.startingAt Point3d.origin
                Direction3d.positiveX
                { radius = Length.meters 0.5
                , length = Length.meters 0.75
                }
                |> Cone3d.placeIn frame
            )
        ]
        |> Scene3d.rotateAround (Axis3d.through (Frame3d.originPoint frame) (Frame3d.zDirection frame))
            (Angle.degrees <|
                case facing of
                    Forward ->
                        0

                    Backward ->
                        180

                    Left ->
                        90

                    Right ->
                        -90
            )


viewBlock : Board -> Model -> ( Point, Block ) -> Scene3d.Entity WorldCoordinates
viewBlock board model ( point, block ) =
    let
        ( x, y, z ) =
            point
    in
    if x < model.xLowerVisible || x > model.xUpperVisible || y < model.yLowerVisible || y > model.yUpperVisible || z < model.zLowerVisible || z > model.zUpperVisible then
        Scene3d.nothing

    else
        case block of
            Wall ->
                Scene3d.blockWithShadow
                    (Scene3d.Material.matte <|
                        case model.blockPalette of
                            SimpleBlocks ->
                                Color.gray

                            RainbowBlocks ->
                                Color.rgb
                                    (toFloat x * 1.2 / toFloat board.maxX)
                                    (toFloat y * 1.2 / toFloat board.maxY)
                                    (toFloat z * 1.2 / toFloat board.maxZ)
                    )
                    (Block3d.centeredOn
                        (Frame3d.atPoint
                            (Point3d.meters
                                (toFloat x)
                                (toFloat y)
                                (toFloat z)
                            )
                        )
                        ( Length.meters 1, Length.meters 1, Length.meters 1 )
                    )

            PointPickup collected ->
                if collected && model.editorMode == TestGame then
                    Scene3d.nothing

                else
                    Scene3d.sphereWithShadow
                        (Scene3d.Material.emissive
                            (Scene3d.Light.color Color.yellow)
                            (Luminance.nits 30000)
                        )
                        (Sphere3d.atPoint
                            (Point3d.meters
                                (toFloat x)
                                (toFloat y)
                                (toFloat z)
                            )
                            (Length.meters 0.125)
                        )

            PlayerSpawn { forward, left } ->
                case model.editorMode of
                    TestGame ->
                        Scene3d.nothing

                    EditBoard ->
                        let
                            center =
                                Point3d.meters
                                    (toFloat x)
                                    (toFloat y)
                                    (toFloat z)

                            carl =
                                SketchPlane3d.unsafe
                                    { originPoint = pointToPoint3d point
                                    , xDirection = axisToDirection3d forward
                                    , yDirection = axisToDirection3d left
                                    }
                                    |> SketchPlane3d.toFrame
                        in
                        Scene3d.group
                            [ Scene3d.sphereWithShadow
                                (Scene3d.Material.emissive
                                    (Scene3d.Light.color Color.white)
                                    (Luminance.nits 100000)
                                )
                                (Sphere3d.atPoint
                                    center
                                    (Length.meters 0.25)
                                )
                            , Scene3d.coneWithShadow
                                (Scene3d.Material.emissive (Scene3d.Light.color Color.red)
                                    (Luminance.nits 10000)
                                )
                                (Cone3d.startingAt center
                                    (Frame3d.xDirection carl)
                                    { radius = Length.meters 0.125
                                    , length = Length.meters 0.75
                                    }
                                )
                            , Scene3d.coneWithShadow
                                (Scene3d.Material.emissive (Scene3d.Light.color Color.green)
                                    (Luminance.nits 10000)
                                )
                                (Cone3d.startingAt center
                                    (Frame3d.yDirection carl)
                                    { radius = Length.meters 0.125
                                    , length = Length.meters 0.75
                                    }
                                )
                            , Scene3d.coneWithShadow
                                (Scene3d.Material.emissive (Scene3d.Light.color Color.blue)
                                    (Luminance.nits 10000)
                                )
                                (Cone3d.startingAt center
                                    (Frame3d.zDirection carl)
                                    { radius = Length.meters 0.125
                                    , length = Length.meters 0.75
                                    }
                                )
                            ]

            Empty ->
                Scene3d.nothing

            Edge ->
                case model.editorMode of
                    TestGame ->
                        Scene3d.nothing

                    EditBoard ->
                        Scene3d.blockWithShadow
                            (Scene3d.Material.metal
                                { baseColor = Color.orange
                                , roughness = 1
                                }
                            )
                            (Block3d.centeredOn
                                (Frame3d.atPoint
                                    (Point3d.meters
                                        (toFloat x)
                                        (toFloat y)
                                        (toFloat z)
                                    )
                                )
                                ( Length.meters 0.5, Length.meters 0.5, Length.meters 0.5 )
                            )


viewCursor : Color -> Animation Float -> Point -> Scene3d.Entity WorldCoordinates
viewCursor color bounceAnim ( x, y, z ) =
    let
        center =
            Point3d.meters
                (toFloat x)
                (toFloat y)
                (toFloat z)

        ( bounce, _ ) =
            Animation.step
                { easing = identity
                , interpolate =
                    \a b dist ->
                        Quantity.interpolateFrom
                            (Quantity.float a)
                            (Quantity.float b)
                            dist
                            |> Quantity.toFloat
                }
                0
                bounceAnim

        offset =
            Length.meters (bounce * 0.1 + 0.5)

        length =
            Length.meters (bounce * -0.1 + 0.95)
    in
    Scene3d.group
        [ -- X bars
          Scene3d.block
            (Scene3d.Material.color color)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.positiveY offset
                    |> Frame3d.translateIn Direction3d.positiveZ offset
                )
                ( length, Length.meters 0.1, Length.meters 0.1 )
            )
        , Scene3d.block
            (Scene3d.Material.color color)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.positiveY offset
                    |> Frame3d.translateIn Direction3d.negativeZ offset
                )
                ( length, Length.meters 0.1, Length.meters 0.1 )
            )
        , Scene3d.block
            (Scene3d.Material.color color)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.negativeY offset
                    |> Frame3d.translateIn Direction3d.positiveZ offset
                )
                ( length, Length.meters 0.1, Length.meters 0.1 )
            )
        , Scene3d.block
            (Scene3d.Material.color color)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.negativeY offset
                    |> Frame3d.translateIn Direction3d.negativeZ offset
                )
                ( length, Length.meters 0.1, Length.meters 0.1 )
            )

        -- Z bars
        , Scene3d.block
            (Scene3d.Material.color color)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.negativeY offset
                    |> Frame3d.translateIn Direction3d.negativeX offset
                )
                ( Length.meters 0.1, Length.meters 0.1, length )
            )
        , Scene3d.block
            (Scene3d.Material.color color)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.positiveY offset
                    |> Frame3d.translateIn Direction3d.negativeX offset
                )
                ( Length.meters 0.1, Length.meters 0.1, length )
            )
        , Scene3d.block
            (Scene3d.Material.color color)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.negativeY offset
                    |> Frame3d.translateIn Direction3d.positiveX offset
                )
                ( Length.meters 0.1, Length.meters 0.1, length )
            )
        , Scene3d.block
            (Scene3d.Material.color color)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.positiveY offset
                    |> Frame3d.translateIn Direction3d.positiveX offset
                )
                ( Length.meters 0.1, Length.meters 0.1, length )
            )

        -- Y bars
        , Scene3d.block
            (Scene3d.Material.color color)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.positiveZ offset
                    |> Frame3d.translateIn Direction3d.negativeX offset
                )
                ( Length.meters 0.1, length, Length.meters 0.1 )
            )
        , Scene3d.block
            (Scene3d.Material.color color)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.negativeZ offset
                    |> Frame3d.translateIn Direction3d.negativeX offset
                )
                ( Length.meters 0.1, length, Length.meters 0.1 )
            )
        , Scene3d.block
            (Scene3d.Material.color color)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.positiveZ offset
                    |> Frame3d.translateIn Direction3d.positiveX offset
                )
                ( Length.meters 0.1, length, Length.meters 0.1 )
            )
        , Scene3d.block
            (Scene3d.Material.color color)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.negativeZ offset
                    |> Frame3d.translateIn Direction3d.positiveX offset
                )
                ( Length.meters 0.1, length, Length.meters 0.1 )
            )
        ]



-- SAMPLE LEVELS


defaultBoard : String
defaultBoard =
    "[1,[9,9,9,[[[0,0,0],[1]],[[0,0,1],[1]],[[0,0,2],[1]],[[0,0,3],[1]],[[0,0,4],[1]],[[0,0,5],[1]],[[0,0,6],[1]],[[0,0,7],[1]],[[0,0,8],[1]],[[0,1,0],[1]],[[0,1,1],[1]],[[0,1,2],[1]],[[0,1,3],[1]],[[0,1,4],[1]],[[0,1,5],[1]],[[0,1,6],[1]],[[0,1,7],[1]],[[0,1,8],[1]],[[0,2,0],[1]],[[0,2,1],[1]],[[0,2,2],[1]],[[0,2,3],[1]],[[0,2,4],[1]],[[0,2,5],[1]],[[0,2,6],[1]],[[0,2,7],[1]],[[0,2,8],[1]],[[0,3,0],[1]],[[0,3,1],[1]],[[0,3,2],[1]],[[0,3,3],[1]],[[0,3,4],[1]],[[0,3,5],[1]],[[0,3,6],[1]],[[0,3,7],[1]],[[0,3,8],[1]],[[0,4,0],[1]],[[0,4,1],[1]],[[0,4,2],[1]],[[0,4,3],[1]],[[0,4,4],[1]],[[0,4,5],[1]],[[0,4,6],[1]],[[0,4,7],[1]],[[0,4,8],[1]],[[0,5,0],[1]],[[0,5,1],[1]],[[0,5,2],[1]],[[0,5,3],[1]],[[0,5,4],[1]],[[0,5,5],[1]],[[0,5,6],[1]],[[0,5,7],[1]],[[0,5,8],[1]],[[0,6,0],[1]],[[0,6,1],[1]],[[0,6,2],[1]],[[0,6,3],[1]],[[0,6,4],[1]],[[0,6,5],[1]],[[0,6,6],[1]],[[0,6,7],[1]],[[0,6,8],[1]],[[0,7,0],[1]],[[0,7,1],[1]],[[0,7,2],[1]],[[0,7,3],[1]],[[0,7,4],[1]],[[0,7,5],[1]],[[0,7,6],[1]],[[0,7,7],[1]],[[0,7,8],[1]],[[0,8,0],[1]],[[0,8,1],[1]],[[0,8,2],[1]],[[0,8,3],[1]],[[0,8,4],[1]],[[0,8,5],[1]],[[0,8,6],[1]],[[0,8,7],[1]],[[0,8,8],[1]],[[1,0,0],[1]],[[1,0,1],[1]],[[1,0,2],[1]],[[1,0,3],[1]],[[1,0,4],[1]],[[1,0,5],[1]],[[1,0,6],[1]],[[1,0,7],[1]],[[1,0,8],[1]],[[1,1,0],[1]],[[1,1,1],[1]],[[1,1,2],[1]],[[1,1,3],[1]],[[1,1,4],[1]],[[1,1,5],[1]],[[1,1,6],[1]],[[1,1,7],[1]],[[1,1,8],[1]],[[1,2,0],[1]],[[1,2,1],[1]],[[1,2,2],[1]],[[1,2,3],[1]],[[1,2,4],[1]],[[1,2,5],[1]],[[1,2,6],[1]],[[1,2,7],[1]],[[1,2,8],[1]],[[1,3,0],[1]],[[1,3,1],[1]],[[1,3,2],[1]],[[1,3,3],[1]],[[1,3,4],[1]],[[1,3,5],[1]],[[1,3,6],[1]],[[1,3,7],[1]],[[1,3,8],[1]],[[1,4,0],[1]],[[1,4,1],[1]],[[1,4,2],[1]],[[1,4,3],[1]],[[1,4,4],[1]],[[1,4,5],[1]],[[1,4,6],[1]],[[1,4,7],[1]],[[1,4,8],[1]],[[1,5,0],[1]],[[1,5,1],[1]],[[1,5,2],[1]],[[1,5,3],[1]],[[1,5,4],[1]],[[1,5,5],[1]],[[1,5,6],[1]],[[1,5,7],[1]],[[1,5,8],[1]],[[1,6,0],[1]],[[1,6,1],[1]],[[1,6,2],[1]],[[1,6,3],[1]],[[1,6,4],[1]],[[1,6,5],[1]],[[1,6,6],[1]],[[1,6,7],[1]],[[1,6,8],[1]],[[1,7,0],[1]],[[1,7,1],[1]],[[1,7,2],[1]],[[1,7,3],[1]],[[1,7,4],[1]],[[1,7,5],[1]],[[1,7,6],[1]],[[1,7,7],[1]],[[1,7,8],[1]],[[1,8,0],[1]],[[1,8,1],[1]],[[1,8,2],[1]],[[1,8,3],[1]],[[1,8,4],[1]],[[1,8,5],[1]],[[1,8,6],[1]],[[1,8,7],[1]],[[1,8,8],[1]],[[2,0,0],[1]],[[2,0,1],[1]],[[2,0,2],[1]],[[2,0,3],[1]],[[2,0,4],[1]],[[2,0,5],[1]],[[2,0,6],[1]],[[2,0,7],[1]],[[2,0,8],[1]],[[2,1,0],[1]],[[2,1,1],[1]],[[2,1,2],[1]],[[2,1,3],[1]],[[2,1,4],[1]],[[2,1,5],[1]],[[2,1,6],[1]],[[2,1,7],[1]],[[2,1,8],[1]],[[2,2,0],[1]],[[2,2,1],[1]],[[2,2,2],[1]],[[2,2,3],[1]],[[2,2,4],[1]],[[2,2,5],[1]],[[2,2,6],[1]],[[2,2,7],[1]],[[2,2,8],[1]],[[2,3,0],[1]],[[2,3,1],[1]],[[2,3,2],[1]],[[2,3,3],[1]],[[2,3,4],[1]],[[2,3,5],[1]],[[2,3,6],[1]],[[2,3,7],[1]],[[2,3,8],[1]],[[2,4,0],[1]],[[2,4,1],[1]],[[2,4,2],[1]],[[2,4,3],[1]],[[2,4,4],[1]],[[2,4,5],[1]],[[2,4,6],[1]],[[2,4,7],[1]],[[2,4,8],[1]],[[2,5,0],[1]],[[2,5,1],[1]],[[2,5,2],[1]],[[2,5,3],[1]],[[2,5,4],[1]],[[2,5,5],[1]],[[2,5,6],[1]],[[2,5,7],[1]],[[2,5,8],[1]],[[2,6,0],[1]],[[2,6,1],[1]],[[2,6,2],[1]],[[2,6,3],[1]],[[2,6,4],[1]],[[2,6,5],[1]],[[2,6,6],[1]],[[2,6,7],[1]],[[2,6,8],[1]],[[2,7,0],[1]],[[2,7,1],[1]],[[2,7,2],[1]],[[2,7,3],[1]],[[2,7,4],[1]],[[2,7,5],[1]],[[2,7,6],[1]],[[2,7,7],[1]],[[2,7,8],[1]],[[2,8,0],[1]],[[2,8,1],[1]],[[2,8,2],[1]],[[2,8,3],[1]],[[2,8,4],[1]],[[2,8,5],[1]],[[2,8,6],[1]],[[2,8,7],[1]],[[2,8,8],[1]],[[3,0,0],[1]],[[3,0,1],[1]],[[3,0,2],[1]],[[3,0,3],[1]],[[3,0,4],[1]],[[3,0,5],[1]],[[3,0,6],[1]],[[3,0,7],[1]],[[3,0,8],[1]],[[3,1,0],[1]],[[3,1,1],[1]],[[3,1,2],[1]],[[3,1,3],[1]],[[3,1,4],[1]],[[3,1,5],[1]],[[3,1,6],[1]],[[3,1,7],[1]],[[3,1,8],[1]],[[3,2,0],[1]],[[3,2,1],[1]],[[3,2,2],[1]],[[3,2,3],[1]],[[3,2,4],[1]],[[3,2,5],[1]],[[3,2,6],[1]],[[3,2,7],[1]],[[3,2,8],[1]],[[3,3,0],[1]],[[3,3,1],[1]],[[3,3,2],[1]],[[3,3,3],[1]],[[3,3,4],[1]],[[3,3,5],[1]],[[3,3,6],[1]],[[3,3,7],[1]],[[3,3,8],[1]],[[3,4,0],[1]],[[3,4,1],[1]],[[3,4,2],[1]],[[3,4,3],[1]],[[3,4,4],[1]],[[3,4,5],[1]],[[3,4,6],[1]],[[3,4,7],[1]],[[3,4,8],[1]],[[3,5,0],[1]],[[3,5,1],[1]],[[3,5,2],[1]],[[3,5,3],[1]],[[3,5,4],[1]],[[3,5,5],[1]],[[3,5,6],[1]],[[3,5,7],[1]],[[3,5,8],[1]],[[3,6,0],[1]],[[3,6,1],[1]],[[3,6,2],[1]],[[3,6,3],[1]],[[3,6,4],[1]],[[3,6,5],[1]],[[3,6,6],[1]],[[3,6,7],[1]],[[3,6,8],[1]],[[3,7,0],[1]],[[3,7,1],[1]],[[3,7,2],[1]],[[3,7,3],[1]],[[3,7,4],[1]],[[3,7,5],[1]],[[3,7,6],[1]],[[3,7,7],[1]],[[3,7,8],[1]],[[3,8,0],[1]],[[3,8,1],[1]],[[3,8,2],[1]],[[3,8,3],[1]],[[3,8,4],[1]],[[3,8,5],[1]],[[3,8,6],[1]],[[3,8,7],[1]],[[3,8,8],[1]],[[4,0,0],[1]],[[4,0,1],[1]],[[4,0,2],[1]],[[4,0,3],[1]],[[4,0,4],[1]],[[4,0,5],[1]],[[4,0,6],[1]],[[4,0,7],[1]],[[4,0,8],[1]],[[4,1,0],[1]],[[4,1,1],[1]],[[4,1,2],[1]],[[4,1,3],[1]],[[4,1,4],[1]],[[4,1,5],[1]],[[4,1,6],[1]],[[4,1,7],[1]],[[4,1,8],[1]],[[4,2,0],[1]],[[4,2,1],[1]],[[4,2,2],[1]],[[4,2,3],[1]],[[4,2,4],[1]],[[4,2,5],[1]],[[4,2,6],[1]],[[4,2,7],[1]],[[4,2,8],[1]],[[4,3,0],[1]],[[4,3,1],[1]],[[4,3,2],[1]],[[4,3,3],[1]],[[4,3,4],[1]],[[4,3,5],[1]],[[4,3,6],[1]],[[4,3,7],[1]],[[4,3,8],[1]],[[4,4,0],[1]],[[4,4,1],[1]],[[4,4,2],[1]],[[4,4,3],[1]],[[4,4,4],[1]],[[4,4,5],[1]],[[4,4,6],[1]],[[4,4,7],[1]],[[4,4,8],[1]],[[4,5,0],[1]],[[4,5,1],[1]],[[4,5,2],[1]],[[4,5,3],[1]],[[4,5,4],[1]],[[4,5,5],[1]],[[4,5,6],[1]],[[4,5,7],[1]],[[4,5,8],[1]],[[4,6,0],[1]],[[4,6,1],[1]],[[4,6,2],[1]],[[4,6,3],[1]],[[4,6,4],[1]],[[4,6,5],[1]],[[4,6,6],[1]],[[4,6,7],[1]],[[4,6,8],[1]],[[4,7,0],[1]],[[4,7,1],[1]],[[4,7,2],[1]],[[4,7,3],[1]],[[4,7,4],[1]],[[4,7,5],[1]],[[4,7,6],[1]],[[4,7,7],[1]],[[4,7,8],[1]],[[4,8,0],[1]],[[4,8,1],[1]],[[4,8,2],[1]],[[4,8,3],[1]],[[4,8,4],[1]],[[4,8,5],[1]],[[4,8,6],[1]],[[4,8,7],[1]],[[4,8,8],[1]],[[5,0,0],[1]],[[5,0,1],[1]],[[5,0,2],[1]],[[5,0,3],[1]],[[5,0,4],[1]],[[5,0,5],[1]],[[5,0,6],[1]],[[5,0,7],[1]],[[5,0,8],[1]],[[5,1,0],[1]],[[5,1,1],[1]],[[5,1,2],[1]],[[5,1,3],[1]],[[5,1,4],[1]],[[5,1,5],[1]],[[5,1,6],[1]],[[5,1,7],[1]],[[5,1,8],[1]],[[5,2,0],[1]],[[5,2,1],[1]],[[5,2,2],[1]],[[5,2,3],[1]],[[5,2,4],[1]],[[5,2,5],[1]],[[5,2,6],[1]],[[5,2,7],[1]],[[5,2,8],[1]],[[5,3,0],[1]],[[5,3,1],[1]],[[5,3,2],[1]],[[5,3,3],[1]],[[5,3,4],[1]],[[5,3,5],[1]],[[5,3,6],[1]],[[5,3,7],[1]],[[5,3,8],[1]],[[5,4,0],[1]],[[5,4,1],[1]],[[5,4,2],[1]],[[5,4,3],[1]],[[5,4,4],[1]],[[5,4,5],[1]],[[5,4,6],[1]],[[5,4,7],[1]],[[5,4,8],[1]],[[5,5,0],[1]],[[5,5,1],[1]],[[5,5,2],[1]],[[5,5,3],[1]],[[5,5,4],[1]],[[5,5,5],[1]],[[5,5,6],[1]],[[5,5,7],[1]],[[5,5,8],[1]],[[5,6,0],[1]],[[5,6,1],[1]],[[5,6,2],[1]],[[5,6,3],[1]],[[5,6,4],[1]],[[5,6,5],[1]],[[5,6,6],[1]],[[5,6,7],[1]],[[5,6,8],[1]],[[5,7,0],[1]],[[5,7,1],[1]],[[5,7,2],[1]],[[5,7,3],[1]],[[5,7,4],[1]],[[5,7,5],[1]],[[5,7,6],[1]],[[5,7,7],[1]],[[5,7,8],[1]],[[5,8,0],[1]],[[5,8,1],[1]],[[5,8,2],[1]],[[5,8,3],[1]],[[5,8,4],[1]],[[5,8,5],[1]],[[5,8,6],[1]],[[5,8,7],[1]],[[5,8,8],[1]],[[6,0,0],[1]],[[6,0,1],[1]],[[6,0,2],[1]],[[6,0,3],[1]],[[6,0,4],[1]],[[6,0,5],[1]],[[6,0,6],[1]],[[6,0,7],[1]],[[6,0,8],[1]],[[6,1,0],[1]],[[6,1,1],[1]],[[6,1,2],[1]],[[6,1,3],[1]],[[6,1,4],[1]],[[6,1,5],[1]],[[6,1,6],[1]],[[6,1,7],[1]],[[6,1,8],[1]],[[6,2,0],[1]],[[6,2,1],[1]],[[6,2,2],[1]],[[6,2,3],[1]],[[6,2,4],[1]],[[6,2,5],[1]],[[6,2,6],[1]],[[6,2,7],[1]],[[6,2,8],[1]],[[6,3,0],[1]],[[6,3,1],[1]],[[6,3,2],[1]],[[6,3,3],[1]],[[6,3,4],[1]],[[6,3,5],[1]],[[6,3,6],[1]],[[6,3,7],[1]],[[6,3,8],[1]],[[6,4,0],[1]],[[6,4,1],[1]],[[6,4,2],[1]],[[6,4,3],[1]],[[6,4,4],[1]],[[6,4,5],[1]],[[6,4,6],[1]],[[6,4,7],[1]],[[6,4,8],[1]],[[6,5,0],[1]],[[6,5,1],[1]],[[6,5,2],[1]],[[6,5,3],[1]],[[6,5,4],[1]],[[6,5,5],[1]],[[6,5,6],[1]],[[6,5,7],[1]],[[6,5,8],[1]],[[6,6,0],[1]],[[6,6,1],[1]],[[6,6,2],[1]],[[6,6,3],[1]],[[6,6,4],[1]],[[6,6,5],[1]],[[6,6,6],[1]],[[6,6,7],[1]],[[6,6,8],[1]],[[6,7,0],[1]],[[6,7,1],[1]],[[6,7,2],[1]],[[6,7,3],[1]],[[6,7,4],[1]],[[6,7,5],[1]],[[6,7,6],[1]],[[6,7,7],[1]],[[6,7,8],[1]],[[6,8,0],[1]],[[6,8,1],[1]],[[6,8,2],[1]],[[6,8,3],[1]],[[6,8,4],[1]],[[6,8,5],[1]],[[6,8,6],[1]],[[6,8,7],[1]],[[6,8,8],[1]],[[7,0,0],[1]],[[7,0,1],[1]],[[7,0,2],[1]],[[7,0,3],[1]],[[7,0,4],[1]],[[7,0,5],[1]],[[7,0,6],[1]],[[7,0,7],[1]],[[7,0,8],[1]],[[7,1,0],[1]],[[7,1,1],[1]],[[7,1,2],[1]],[[7,1,3],[1]],[[7,1,4],[1]],[[7,1,5],[1]],[[7,1,6],[1]],[[7,1,7],[1]],[[7,1,8],[1]],[[7,2,0],[1]],[[7,2,1],[1]],[[7,2,2],[1]],[[7,2,3],[1]],[[7,2,4],[1]],[[7,2,5],[1]],[[7,2,6],[1]],[[7,2,7],[1]],[[7,2,8],[1]],[[7,3,0],[1]],[[7,3,1],[1]],[[7,3,2],[1]],[[7,3,3],[1]],[[7,3,4],[1]],[[7,3,5],[1]],[[7,3,6],[1]],[[7,3,7],[1]],[[7,3,8],[1]],[[7,4,0],[1]],[[7,4,1],[1]],[[7,4,2],[1]],[[7,4,3],[1]],[[7,4,4],[1]],[[7,4,5],[1]],[[7,4,6],[1]],[[7,4,7],[1]],[[7,4,8],[1]],[[7,5,0],[1]],[[7,5,1],[1]],[[7,5,2],[1]],[[7,5,3],[1]],[[7,5,4],[1]],[[7,5,5],[1]],[[7,5,6],[1]],[[7,5,7],[1]],[[7,5,8],[1]],[[7,6,0],[1]],[[7,6,1],[1]],[[7,6,2],[1]],[[7,6,3],[1]],[[7,6,4],[1]],[[7,6,5],[1]],[[7,6,6],[1]],[[7,6,7],[1]],[[7,6,8],[1]],[[7,7,0],[1]],[[7,7,1],[1]],[[7,7,2],[1]],[[7,7,3],[1]],[[7,7,4],[1]],[[7,7,5],[1]],[[7,7,6],[1]],[[7,7,7],[1]],[[7,7,8],[1]],[[7,8,0],[1]],[[7,8,1],[1]],[[7,8,2],[1]],[[7,8,3],[1]],[[7,8,4],[1]],[[7,8,5],[1]],[[7,8,6],[1]],[[7,8,7],[1]],[[7,8,8],[1]],[[8,0,0],[1]],[[8,0,1],[1]],[[8,0,2],[1]],[[8,0,3],[1]],[[8,0,4],[1]],[[8,0,5],[1]],[[8,0,6],[1]],[[8,0,7],[1]],[[8,0,8],[1]],[[8,1,0],[1]],[[8,1,1],[1]],[[8,1,2],[1]],[[8,1,3],[1]],[[8,1,4],[1]],[[8,1,5],[1]],[[8,1,6],[1]],[[8,1,7],[1]],[[8,1,8],[1]],[[8,2,0],[1]],[[8,2,1],[1]],[[8,2,2],[1]],[[8,2,3],[1]],[[8,2,4],[1]],[[8,2,5],[1]],[[8,2,6],[1]],[[8,2,7],[1]],[[8,2,8],[1]],[[8,3,0],[1]],[[8,3,1],[1]],[[8,3,2],[1]],[[8,3,3],[1]],[[8,3,4],[1]],[[8,3,5],[1]],[[8,3,6],[1]],[[8,3,7],[1]],[[8,3,8],[1]],[[8,4,0],[1]],[[8,4,1],[1]],[[8,4,2],[1]],[[8,4,3],[1]],[[8,4,4],[1]],[[8,4,5],[1]],[[8,4,6],[1]],[[8,4,7],[1]],[[8,4,8],[1]],[[8,5,0],[1]],[[8,5,1],[1]],[[8,5,2],[1]],[[8,5,3],[1]],[[8,5,4],[1]],[[8,5,5],[1]],[[8,5,6],[1]],[[8,5,7],[1]],[[8,5,8],[1]],[[8,6,0],[1]],[[8,6,1],[1]],[[8,6,2],[1]],[[8,6,3],[1]],[[8,6,4],[1]],[[8,6,5],[1]],[[8,6,6],[1]],[[8,6,7],[1]],[[8,6,8],[1]],[[8,7,0],[1]],[[8,7,1],[1]],[[8,7,2],[1]],[[8,7,3],[1]],[[8,7,4],[1]],[[8,7,5],[1]],[[8,7,6],[1]],[[8,7,7],[1]],[[8,7,8],[1]],[[8,8,0],[1]],[[8,8,1],[1]],[[8,8,2],[1]],[[8,8,3],[1]],[[8,8,4],[1]],[[8,8,5],[1]],[[8,8,6],[1]],[[8,8,7],[1]],[[8,8,8],[1]]]]]"


basicMiniBoard : String
basicMiniBoard =
    "[1,[5,5,5,[[[0,0,0],[1]],[[0,0,1],[1]],[[0,0,2],[2]],[[0,0,3],[1]],[[0,0,4],[1]],[[0,1,0],[1]],[[0,1,1],[1]],[[0,1,2],[3,false]],[[0,1,3],[1]],[[0,1,4],[1]],[[0,2,0],[2]],[[0,2,1],[3,false]],[[0,2,2],[3,false]],[[0,2,3],[3,false]],[[0,2,4],[2]],[[0,3,0],[1]],[[0,3,1],[1]],[[0,3,2],[3,false]],[[0,3,3],[1]],[[0,3,4],[1]],[[0,4,0],[1]],[[0,4,1],[1]],[[0,4,2],[2]],[[0,4,3],[1]],[[0,4,4],[1]],[[1,0,0],[1]],[[1,0,1],[1]],[[1,0,2],[3,false]],[[1,0,3],[1]],[[1,0,4],[1]],[[1,1,0],[1]],[[1,1,1],[1]],[[1,1,2],[1]],[[1,1,3],[1]],[[1,1,4],[1]],[[1,2,0],[3,false]],[[1,2,1],[1]],[[1,2,2],[1]],[[1,2,3],[1]],[[1,2,4],[3,false]],[[1,3,0],[1]],[[1,3,1],[1]],[[1,3,2],[1]],[[1,3,3],[1]],[[1,3,4],[1]],[[1,4,0],[1]],[[1,4,1],[1]],[[1,4,2],[3,false]],[[1,4,3],[1]],[[1,4,4],[1]],[[2,0,0],[2]],[[2,0,1],[3,false]],[[2,0,2],[3,false]],[[2,0,3],[3,false]],[[2,0,4],[2]],[[2,1,0],[3,false]],[[2,1,1],[1]],[[2,1,2],[1]],[[2,1,3],[1]],[[2,1,4],[3,false]],[[2,2,0],[3,false]],[[2,2,1],[1]],[[2,2,2],[1]],[[2,2,3],[1]],[[2,2,4],[4,[[0],[2]]]],[[2,3,0],[3,false]],[[2,3,1],[1]],[[2,3,2],[1]],[[2,3,3],[1]],[[2,3,4],[3,false]],[[2,4,0],[2]],[[2,4,1],[3,false]],[[2,4,2],[3,false]],[[2,4,3],[3,false]],[[2,4,4],[2]],[[3,0,0],[1]],[[3,0,1],[1]],[[3,0,2],[3,false]],[[3,0,3],[1]],[[3,0,4],[1]],[[3,1,0],[1]],[[3,1,1],[1]],[[3,1,2],[1]],[[3,1,3],[1]],[[3,1,4],[1]],[[3,2,0],[3,false]],[[3,2,1],[1]],[[3,2,2],[1]],[[3,2,3],[1]],[[3,2,4],[3,false]],[[3,3,0],[1]],[[3,3,1],[1]],[[3,3,2],[1]],[[3,3,3],[1]],[[3,3,4],[1]],[[3,4,0],[1]],[[3,4,1],[1]],[[3,4,2],[3,false]],[[3,4,3],[1]],[[3,4,4],[1]],[[4,0,0],[1]],[[4,0,1],[1]],[[4,0,2],[2]],[[4,0,3],[1]],[[4,0,4],[1]],[[4,1,0],[1]],[[4,1,1],[1]],[[4,1,2],[3,false]],[[4,1,3],[1]],[[4,1,4],[1]],[[4,2,0],[2]],[[4,2,1],[3,false]],[[4,2,2],[3,false]],[[4,2,3],[3,false]],[[4,2,4],[2]],[[4,3,0],[1]],[[4,3,1],[1]],[[4,3,2],[3,false]],[[4,3,3],[1]],[[4,3,4],[1]],[[4,4,0],[1]],[[4,4,1],[1]],[[4,4,2],[2]],[[4,4,3],[1]],[[4,4,4],[1]]]]]"


zigZagBoard : String
zigZagBoard =
    "[1,[9,9,9,[[[0,0,0],[1]],[[0,0,1],[1]],[[0,0,2],[1]],[[0,0,3],[1]],[[0,0,4],[1]],[[0,0,5],[1]],[[0,0,6],[1]],[[0,0,7],[1]],[[0,0,8],[1]],[[0,1,0],[2]],[[0,1,1],[0]],[[0,1,2],[3,false]],[[0,1,3],[1]],[[0,1,4],[1]],[[0,1,5],[1]],[[0,1,6],[3,false]],[[0,1,7],[0]],[[0,1,8],[2]],[[0,2,0],[1]],[[0,2,1],[1]],[[0,2,2],[3,false]],[[0,2,3],[1]],[[0,2,4],[1]],[[0,2,5],[1]],[[0,2,6],[3,false]],[[0,2,7],[1]],[[0,2,8],[1]],[[0,3,0],[1]],[[0,3,1],[1]],[[0,3,2],[3,false]],[[0,3,3],[1]],[[0,3,4],[1]],[[0,3,5],[1]],[[0,3,6],[3,false]],[[0,3,7],[1]],[[0,3,8],[1]],[[0,4,0],[1]],[[0,4,1],[1]],[[0,4,2],[3,false]],[[0,4,3],[3,false]],[[0,4,4],[3,false]],[[0,4,5],[3,false]],[[0,4,6],[3,false]],[[0,4,7],[1]],[[0,4,8],[1]],[[0,5,0],[1]],[[0,5,1],[1]],[[0,5,2],[3,false]],[[0,5,3],[1]],[[0,5,4],[1]],[[0,5,5],[1]],[[0,5,6],[3,false]],[[0,5,7],[1]],[[0,5,8],[1]],[[0,6,0],[1]],[[0,6,1],[1]],[[0,6,2],[3,false]],[[0,6,3],[1]],[[0,6,4],[1]],[[0,6,5],[1]],[[0,6,6],[3,false]],[[0,6,7],[1]],[[0,6,8],[1]],[[0,7,0],[2]],[[0,7,1],[0]],[[0,7,2],[3,false]],[[0,7,3],[1]],[[0,7,4],[1]],[[0,7,5],[1]],[[0,7,6],[3,false]],[[0,7,7],[0]],[[0,7,8],[2]],[[0,8,0],[1]],[[0,8,1],[1]],[[0,8,2],[1]],[[0,8,3],[1]],[[0,8,4],[1]],[[0,8,5],[1]],[[0,8,6],[1]],[[0,8,7],[1]],[[0,8,8],[1]],[[1,0,0],[2]],[[1,0,1],[0]],[[1,0,2],[3,false]],[[1,0,3],[1]],[[1,0,4],[1]],[[1,0,5],[1]],[[1,0,6],[3,false]],[[1,0,7],[0]],[[1,0,8],[2]],[[1,1,0],[0]],[[1,1,1],[1]],[[1,1,2],[1]],[[1,1,3],[1]],[[1,1,4],[1]],[[1,1,5],[1]],[[1,1,6],[1]],[[1,1,7],[1]],[[1,1,8],[0]],[[1,2,0],[3,false]],[[1,2,1],[1]],[[1,2,2],[1]],[[1,2,3],[1]],[[1,2,4],[1]],[[1,2,5],[1]],[[1,2,6],[1]],[[1,2,7],[1]],[[1,2,8],[1]],[[1,3,0],[3,false]],[[1,3,1],[1]],[[1,3,2],[1]],[[1,3,3],[1]],[[1,3,4],[1]],[[1,3,5],[1]],[[1,3,6],[1]],[[1,3,7],[1]],[[1,3,8],[1]],[[1,4,0],[3,false]],[[1,4,1],[1]],[[1,4,2],[1]],[[1,4,3],[1]],[[1,4,4],[1]],[[1,4,5],[1]],[[1,4,6],[1]],[[1,4,7],[1]],[[1,4,8],[1]],[[1,5,0],[3,false]],[[1,5,1],[1]],[[1,5,2],[1]],[[1,5,3],[1]],[[1,5,4],[1]],[[1,5,5],[1]],[[1,5,6],[1]],[[1,5,7],[1]],[[1,5,8],[1]],[[1,6,0],[3,false]],[[1,6,1],[1]],[[1,6,2],[1]],[[1,6,3],[1]],[[1,6,4],[1]],[[1,6,5],[1]],[[1,6,6],[1]],[[1,6,7],[1]],[[1,6,8],[1]],[[1,7,0],[0]],[[1,7,1],[1]],[[1,7,2],[1]],[[1,7,3],[1]],[[1,7,4],[1]],[[1,7,5],[1]],[[1,7,6],[1]],[[1,7,7],[1]],[[1,7,8],[0]],[[1,8,0],[2]],[[1,8,1],[0]],[[1,8,2],[3,false]],[[1,8,3],[1]],[[1,8,4],[1]],[[1,8,5],[1]],[[1,8,6],[3,false]],[[1,8,7],[0]],[[1,8,8],[2]],[[2,0,0],[1]],[[2,0,1],[1]],[[2,0,2],[3,false]],[[2,0,3],[1]],[[2,0,4],[1]],[[2,0,5],[1]],[[2,0,6],[3,false]],[[2,0,7],[1]],[[2,0,8],[1]],[[2,1,0],[1]],[[2,1,1],[1]],[[2,1,2],[1]],[[2,1,3],[1]],[[2,1,4],[1]],[[2,1,5],[1]],[[2,1,6],[1]],[[2,1,7],[1]],[[2,1,8],[3,false]],[[2,2,0],[1]],[[2,2,1],[1]],[[2,2,2],[1]],[[2,2,3],[1]],[[2,2,4],[1]],[[2,2,5],[1]],[[2,2,6],[1]],[[2,2,7],[1]],[[2,2,8],[1]],[[2,3,0],[1]],[[2,3,1],[1]],[[2,3,2],[1]],[[2,3,3],[1]],[[2,3,4],[1]],[[2,3,5],[1]],[[2,3,6],[1]],[[2,3,7],[1]],[[2,3,8],[1]],[[2,4,0],[3,false]],[[2,4,1],[1]],[[2,4,2],[1]],[[2,4,3],[1]],[[2,4,4],[1]],[[2,4,5],[1]],[[2,4,6],[1]],[[2,4,7],[1]],[[2,4,8],[1]],[[2,5,0],[1]],[[2,5,1],[1]],[[2,5,2],[1]],[[2,5,3],[1]],[[2,5,4],[1]],[[2,5,5],[1]],[[2,5,6],[1]],[[2,5,7],[1]],[[2,5,8],[1]],[[2,6,0],[1]],[[2,6,1],[1]],[[2,6,2],[1]],[[2,6,3],[1]],[[2,6,4],[1]],[[2,6,5],[1]],[[2,6,6],[1]],[[2,6,7],[1]],[[2,6,8],[1]],[[2,7,0],[1]],[[2,7,1],[1]],[[2,7,2],[1]],[[2,7,3],[1]],[[2,7,4],[1]],[[2,7,5],[1]],[[2,7,6],[1]],[[2,7,7],[1]],[[2,7,8],[3,false]],[[2,8,0],[1]],[[2,8,1],[1]],[[2,8,2],[3,false]],[[2,8,3],[1]],[[2,8,4],[1]],[[2,8,5],[1]],[[2,8,6],[3,false]],[[2,8,7],[1]],[[2,8,8],[1]],[[3,0,0],[1]],[[3,0,1],[1]],[[3,0,2],[3,false]],[[3,0,3],[1]],[[3,0,4],[1]],[[3,0,5],[1]],[[3,0,6],[3,false]],[[3,0,7],[1]],[[3,0,8],[1]],[[3,1,0],[1]],[[3,1,1],[1]],[[3,1,2],[1]],[[3,1,3],[1]],[[3,1,4],[1]],[[3,1,5],[1]],[[3,1,6],[1]],[[3,1,7],[1]],[[3,1,8],[3,false]],[[3,2,0],[1]],[[3,2,1],[1]],[[3,2,2],[1]],[[3,2,3],[1]],[[3,2,4],[1]],[[3,2,5],[1]],[[3,2,6],[1]],[[3,2,7],[1]],[[3,2,8],[1]],[[3,3,0],[1]],[[3,3,1],[1]],[[3,3,2],[1]],[[3,3,3],[1]],[[3,3,4],[1]],[[3,3,5],[1]],[[3,3,6],[1]],[[3,3,7],[1]],[[3,3,8],[1]],[[3,4,0],[3,false]],[[3,4,1],[1]],[[3,4,2],[1]],[[3,4,3],[1]],[[3,4,4],[1]],[[3,4,5],[1]],[[3,4,6],[1]],[[3,4,7],[1]],[[3,4,8],[1]],[[3,5,0],[1]],[[3,5,1],[1]],[[3,5,2],[1]],[[3,5,3],[1]],[[3,5,4],[1]],[[3,5,5],[1]],[[3,5,6],[1]],[[3,5,7],[1]],[[3,5,8],[1]],[[3,6,0],[1]],[[3,6,1],[1]],[[3,6,2],[1]],[[3,6,3],[1]],[[3,6,4],[1]],[[3,6,5],[1]],[[3,6,6],[1]],[[3,6,7],[1]],[[3,6,8],[1]],[[3,7,0],[1]],[[3,7,1],[1]],[[3,7,2],[1]],[[3,7,3],[1]],[[3,7,4],[1]],[[3,7,5],[1]],[[3,7,6],[1]],[[3,7,7],[1]],[[3,7,8],[3,false]],[[3,8,0],[1]],[[3,8,1],[1]],[[3,8,2],[3,false]],[[3,8,3],[1]],[[3,8,4],[1]],[[3,8,5],[1]],[[3,8,6],[3,false]],[[3,8,7],[1]],[[3,8,8],[1]],[[4,0,0],[1]],[[4,0,1],[1]],[[4,0,2],[3,false]],[[4,0,3],[3,false]],[[4,0,4],[3,false]],[[4,0,5],[3,false]],[[4,0,6],[3,false]],[[4,0,7],[1]],[[4,0,8],[1]],[[4,1,0],[1]],[[4,1,1],[1]],[[4,1,2],[1]],[[4,1,3],[1]],[[4,1,4],[1]],[[4,1,5],[1]],[[4,1,6],[1]],[[4,1,7],[1]],[[4,1,8],[3,false]],[[4,2,0],[1]],[[4,2,1],[1]],[[4,2,2],[1]],[[4,2,3],[1]],[[4,2,4],[1]],[[4,2,5],[1]],[[4,2,6],[1]],[[4,2,7],[1]],[[4,2,8],[3,false]],[[4,3,0],[1]],[[4,3,1],[1]],[[4,3,2],[1]],[[4,3,3],[1]],[[4,3,4],[1]],[[4,3,5],[1]],[[4,3,6],[1]],[[4,3,7],[1]],[[4,3,8],[3,false]],[[4,4,0],[3,false]],[[4,4,1],[1]],[[4,4,2],[1]],[[4,4,3],[1]],[[4,4,4],[1]],[[4,4,5],[1]],[[4,4,6],[1]],[[4,4,7],[1]],[[4,4,8],[3,false]],[[4,5,0],[1]],[[4,5,1],[1]],[[4,5,2],[1]],[[4,5,3],[1]],[[4,5,4],[1]],[[4,5,5],[1]],[[4,5,6],[1]],[[4,5,7],[1]],[[4,5,8],[3,false]],[[4,6,0],[1]],[[4,6,1],[1]],[[4,6,2],[1]],[[4,6,3],[1]],[[4,6,4],[1]],[[4,6,5],[1]],[[4,6,6],[1]],[[4,6,7],[1]],[[4,6,8],[3,false]],[[4,7,0],[1]],[[4,7,1],[1]],[[4,7,2],[1]],[[4,7,3],[1]],[[4,7,4],[1]],[[4,7,5],[1]],[[4,7,6],[1]],[[4,7,7],[1]],[[4,7,8],[3,false]],[[4,8,0],[1]],[[4,8,1],[1]],[[4,8,2],[3,false]],[[4,8,3],[0]],[[4,8,4],[0]],[[4,8,5],[4,[[5],[1]]]],[[4,8,6],[3,false]],[[4,8,7],[1]],[[4,8,8],[1]],[[5,0,0],[1]],[[5,0,1],[1]],[[5,0,2],[3,false]],[[5,0,3],[1]],[[5,0,4],[1]],[[5,0,5],[1]],[[5,0,6],[3,false]],[[5,0,7],[1]],[[5,0,8],[1]],[[5,1,0],[1]],[[5,1,1],[1]],[[5,1,2],[1]],[[5,1,3],[1]],[[5,1,4],[1]],[[5,1,5],[1]],[[5,1,6],[1]],[[5,1,7],[1]],[[5,1,8],[3,false]],[[5,2,0],[1]],[[5,2,1],[1]],[[5,2,2],[1]],[[5,2,3],[1]],[[5,2,4],[1]],[[5,2,5],[1]],[[5,2,6],[1]],[[5,2,7],[1]],[[5,2,8],[1]],[[5,3,0],[1]],[[5,3,1],[1]],[[5,3,2],[1]],[[5,3,3],[1]],[[5,3,4],[1]],[[5,3,5],[1]],[[5,3,6],[1]],[[5,3,7],[1]],[[5,3,8],[1]],[[5,4,0],[3,false]],[[5,4,1],[1]],[[5,4,2],[1]],[[5,4,3],[1]],[[5,4,4],[1]],[[5,4,5],[1]],[[5,4,6],[1]],[[5,4,7],[1]],[[5,4,8],[1]],[[5,5,0],[1]],[[5,5,1],[1]],[[5,5,2],[1]],[[5,5,3],[1]],[[5,5,4],[1]],[[5,5,5],[1]],[[5,5,6],[1]],[[5,5,7],[1]],[[5,5,8],[1]],[[5,6,0],[1]],[[5,6,1],[1]],[[5,6,2],[1]],[[5,6,3],[1]],[[5,6,4],[1]],[[5,6,5],[1]],[[5,6,6],[1]],[[5,6,7],[1]],[[5,6,8],[1]],[[5,7,0],[1]],[[5,7,1],[1]],[[5,7,2],[1]],[[5,7,3],[1]],[[5,7,4],[1]],[[5,7,5],[1]],[[5,7,6],[1]],[[5,7,7],[1]],[[5,7,8],[3,false]],[[5,8,0],[1]],[[5,8,1],[1]],[[5,8,2],[3,false]],[[5,8,3],[1]],[[5,8,4],[1]],[[5,8,5],[1]],[[5,8,6],[3,false]],[[5,8,7],[1]],[[5,8,8],[1]],[[6,0,0],[1]],[[6,0,1],[1]],[[6,0,2],[3,false]],[[6,0,3],[1]],[[6,0,4],[1]],[[6,0,5],[1]],[[6,0,6],[3,false]],[[6,0,7],[1]],[[6,0,8],[1]],[[6,1,0],[1]],[[6,1,1],[1]],[[6,1,2],[1]],[[6,1,3],[1]],[[6,1,4],[1]],[[6,1,5],[1]],[[6,1,6],[1]],[[6,1,7],[1]],[[6,1,8],[3,false]],[[6,2,0],[1]],[[6,2,1],[1]],[[6,2,2],[1]],[[6,2,3],[1]],[[6,2,4],[1]],[[6,2,5],[1]],[[6,2,6],[1]],[[6,2,7],[1]],[[6,2,8],[1]],[[6,3,0],[1]],[[6,3,1],[1]],[[6,3,2],[1]],[[6,3,3],[1]],[[6,3,4],[1]],[[6,3,5],[1]],[[6,3,6],[1]],[[6,3,7],[1]],[[6,3,8],[1]],[[6,4,0],[3,false]],[[6,4,1],[1]],[[6,4,2],[1]],[[6,4,3],[1]],[[6,4,4],[1]],[[6,4,5],[1]],[[6,4,6],[1]],[[6,4,7],[1]],[[6,4,8],[1]],[[6,5,0],[1]],[[6,5,1],[1]],[[6,5,2],[1]],[[6,5,3],[1]],[[6,5,4],[1]],[[6,5,5],[1]],[[6,5,6],[1]],[[6,5,7],[1]],[[6,5,8],[1]],[[6,6,0],[1]],[[6,6,1],[1]],[[6,6,2],[1]],[[6,6,3],[1]],[[6,6,4],[1]],[[6,6,5],[1]],[[6,6,6],[1]],[[6,6,7],[1]],[[6,6,8],[1]],[[6,7,0],[1]],[[6,7,1],[1]],[[6,7,2],[1]],[[6,7,3],[1]],[[6,7,4],[1]],[[6,7,5],[1]],[[6,7,6],[1]],[[6,7,7],[1]],[[6,7,8],[3,false]],[[6,8,0],[1]],[[6,8,1],[1]],[[6,8,2],[3,false]],[[6,8,3],[1]],[[6,8,4],[1]],[[6,8,5],[1]],[[6,8,6],[3,false]],[[6,8,7],[1]],[[6,8,8],[1]],[[7,0,0],[2]],[[7,0,1],[0]],[[7,0,2],[3,false]],[[7,0,3],[1]],[[7,0,4],[1]],[[7,0,5],[1]],[[7,0,6],[3,false]],[[7,0,7],[0]],[[7,0,8],[2]],[[7,1,0],[0]],[[7,1,1],[1]],[[7,1,2],[1]],[[7,1,3],[1]],[[7,1,4],[1]],[[7,1,5],[1]],[[7,1,6],[1]],[[7,1,7],[1]],[[7,1,8],[0]],[[7,2,0],[3,false]],[[7,2,1],[1]],[[7,2,2],[1]],[[7,2,3],[1]],[[7,2,4],[1]],[[7,2,5],[1]],[[7,2,6],[1]],[[7,2,7],[1]],[[7,2,8],[1]],[[7,3,0],[3,false]],[[7,3,1],[1]],[[7,3,2],[1]],[[7,3,3],[1]],[[7,3,4],[1]],[[7,3,5],[1]],[[7,3,6],[1]],[[7,3,7],[1]],[[7,3,8],[1]],[[7,4,0],[3,false]],[[7,4,1],[1]],[[7,4,2],[1]],[[7,4,3],[1]],[[7,4,4],[1]],[[7,4,5],[1]],[[7,4,6],[1]],[[7,4,7],[1]],[[7,4,8],[1]],[[7,5,0],[3,false]],[[7,5,1],[1]],[[7,5,2],[1]],[[7,5,3],[1]],[[7,5,4],[1]],[[7,5,5],[1]],[[7,5,6],[1]],[[7,5,7],[1]],[[7,5,8],[1]],[[7,6,0],[3,false]],[[7,6,1],[1]],[[7,6,2],[1]],[[7,6,3],[1]],[[7,6,4],[1]],[[7,6,5],[1]],[[7,6,6],[1]],[[7,6,7],[1]],[[7,6,8],[1]],[[7,7,0],[0]],[[7,7,1],[1]],[[7,7,2],[1]],[[7,7,3],[1]],[[7,7,4],[1]],[[7,7,5],[1]],[[7,7,6],[1]],[[7,7,7],[1]],[[7,7,8],[0]],[[7,8,0],[2]],[[7,8,1],[0]],[[7,8,2],[3,false]],[[7,8,3],[1]],[[7,8,4],[1]],[[7,8,5],[1]],[[7,8,6],[3,false]],[[7,8,7],[0]],[[7,8,8],[2]],[[8,0,0],[1]],[[8,0,1],[1]],[[8,0,2],[1]],[[8,0,3],[1]],[[8,0,4],[1]],[[8,0,5],[1]],[[8,0,6],[1]],[[8,0,7],[1]],[[8,0,8],[1]],[[8,1,0],[2]],[[8,1,1],[0]],[[8,1,2],[3,false]],[[8,1,3],[1]],[[8,1,4],[1]],[[8,1,5],[1]],[[8,1,6],[3,false]],[[8,1,7],[0]],[[8,1,8],[2]],[[8,2,0],[1]],[[8,2,1],[1]],[[8,2,2],[3,false]],[[8,2,3],[1]],[[8,2,4],[1]],[[8,2,5],[1]],[[8,2,6],[3,false]],[[8,2,7],[1]],[[8,2,8],[1]],[[8,3,0],[1]],[[8,3,1],[1]],[[8,3,2],[3,false]],[[8,3,3],[1]],[[8,3,4],[1]],[[8,3,5],[1]],[[8,3,6],[3,false]],[[8,3,7],[1]],[[8,3,8],[1]],[[8,4,0],[1]],[[8,4,1],[1]],[[8,4,2],[3,false]],[[8,4,3],[3,false]],[[8,4,4],[3,false]],[[8,4,5],[3,false]],[[8,4,6],[3,false]],[[8,4,7],[1]],[[8,4,8],[1]],[[8,5,0],[1]],[[8,5,1],[1]],[[8,5,2],[3,false]],[[8,5,3],[1]],[[8,5,4],[1]],[[8,5,5],[1]],[[8,5,6],[3,false]],[[8,5,7],[1]],[[8,5,8],[1]],[[8,6,0],[1]],[[8,6,1],[1]],[[8,6,2],[3,false]],[[8,6,3],[1]],[[8,6,4],[1]],[[8,6,5],[1]],[[8,6,6],[3,false]],[[8,6,7],[1]],[[8,6,8],[1]],[[8,7,0],[2]],[[8,7,1],[0]],[[8,7,2],[3,false]],[[8,7,3],[1]],[[8,7,4],[1]],[[8,7,5],[1]],[[8,7,6],[3,false]],[[8,7,7],[0]],[[8,7,8],[2]],[[8,8,0],[1]],[[8,8,1],[1]],[[8,8,2],[1]],[[8,8,3],[1]],[[8,8,4],[1]],[[8,8,5],[1]],[[8,8,6],[1]],[[8,8,7],[1]],[[8,8,8],[1]]]]]"
