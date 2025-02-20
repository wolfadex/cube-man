module Screen.Editor exposing
    ( BlockEditMode
    , CameraMode
    , EditorMode
    , EditorMouseInteraction
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Angle exposing (Angle)
import Animation exposing (Animation)
import Axis3d exposing (Axis3d)
import Axis3d.Extra
import Block3d
import Board exposing (Board)
import BoundingBox3d
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cone3d
import Cylinder3d
import Dict
import Direction3d
import Duration exposing (Duration)
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Html.Attributes.Extra
import Html.Events
import Html.Extra
import Illuminance
import Input
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
import Screen exposing (Screen)
import Serialize
import Set exposing (Set)
import Shared
import SketchPlane3d
import Sphere3d
import Task
import Undo
import Viewpoint3d exposing (Viewpoint3d)


type alias Model =
    { level : Board.Level

    --
    , editorBoard : Undo.Stack Board
    , boardLoadError : Maybe Board.BoardLoadError
    , boardPlayError : Maybe Board.BoardPlayError
    , xLowerVisible : Int
    , xUpperVisible : Int
    , yLowerVisible : Int
    , yUpperVisible : Int
    , zLowerVisible : Int
    , zUpperVisible : Int
    , selectedBlockType : Board.Block
    , editorCursor : Board.Point
    , editorKeysDown : Set String
    , cameraRotation : Angle
    , cameraElevation : Angle
    , cameraDistance : Length
    , cameraFocalPoint : Point3d Length.Meters Board.WorldCoordinates
    , mouseDragging : EditorMouseInteraction
    , cursorBounce : Animation Float
    , boardEncoding : String
    , editorMode : EditorMode
    , blockEditMode : BlockEditMode
    , cameraMode : CameraMode
    , showBoardBounds : Bool
    , selectedBlock : Maybe ( Board.Point, Board.Block )
    , editorMaxXRaw : String
    , editorMaxYRaw : String
    , editorMaxZRaw : String
    , showSettings : Bool
    , screenSize : Maybe Shared.ScreenSize
    }


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


init : ( Model, Cmd Msg )
init =
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
                List.repeat (maxX * maxY * maxZ) Board.Wall
                    |> List.foldl
                        (\block ( index, blocks ) ->
                            ( index + 1
                            , Dict.insert (Board.indexToPoint sizeHelper index) block blocks
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
      , selectedBlockType = Board.Wall
      , level = Board.emptyLevel
      , editorBoard = Undo.init board
      , boardLoadError = Nothing
      , boardPlayError = Nothing
      , screenSize = Nothing
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
                |> Serialize.encodeToJson Board.boardCodec
                |> Json.Encode.encode 0
      , editorMaxXRaw = String.fromInt maxX
      , editorMaxYRaw = String.fromInt maxY
      , editorMaxZRaw = String.fromInt maxZ

      -- , blockPalette = SimpleBlocks
      , showSettings = False
      }
    , Browser.Dom.getViewportOf "editor-viewport"
        |> Task.attempt EditorViewportResized
    )


subscriptions : { toSharedMsg : Shared.Msg -> msg, toMsg : Msg -> msg } -> Model -> Sub msg
subscriptions { toSharedMsg, toMsg } model =
    Sub.batch
        [ Browser.Events.onResize (\width height -> Shared.SetScreenSize { width = width, height = height } |> toSharedMsg)
        , if model.showSettings then
            Sub.none

          else
            Sub.batch
                [ Browser.Events.onKeyDown (decodeKeyDown toMsg)
                , Browser.Events.onKeyUp (decodeKeyUp toMsg)
                , Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick >> toMsg)
                , Browser.Events.onResize (\_ _ -> toMsg BrowserResized)
                ]
        ]


decodeKeyDown : (Msg -> msg) -> Json.Decode.Decoder msg
decodeKeyDown toMsg =
    Json.Decode.map (KeyDown >> toMsg)
        (Json.Decode.field "key" Json.Decode.string)


decodeKeyUp : (Msg -> msg) -> Json.Decode.Decoder msg
decodeKeyUp toMsg =
    Json.Decode.map (KeyUp >> toMsg)
        (Json.Decode.field "key" Json.Decode.string)


type Msg
    = NoOp
    | Tick Duration
    | KeyDown String
    | KeyUp String
    | MouseDown Json.Decode.Value
    | MouseUp
    | MouseMove Json.Decode.Value (Point2d Pixels Board.ScreenCoordinates) (Point2d Pixels Board.ScreenCoordinates)
    | EncodingChanged String
    | LoadEditorBoard String
    | ChangeMode
    | RestartLevel
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
    | BlockTypeSelected Board.Block
    | SetBlock Board.Point Board.Block
    | MaxXChanged String
    | MaxYChanged String
    | MaxZChanged String
    | ShowBoardBounds Bool
    | ShowSettings Bool
    | EditorViewportResized (Result Browser.Dom.Error Browser.Dom.Viewport)
    | BrowserResized


update : (Shared.Msg -> msg) -> Shared.LoadedModel -> (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update toSharedMsg sharedModel toMsg msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick deltaMs ->
            ( model
                |> tick deltaMs
            , Cmd.none
            )

        EncodingChanged boardEncoding ->
            ( { model | boardEncoding = boardEncoding }, Cmd.none )

        LoadEditorBoard encoding ->
            let
                loadedBoard =
                    encoding
                        |> Json.Decode.decodeString Json.Decode.value
                        |> Result.withDefault Json.Encode.null
                        |> Serialize.decodeFromJson Board.boardCodec
                        |> Result.mapError
                            (\error ->
                                case error of
                                    Serialize.CustomError _ ->
                                        Board.OtherError

                                    Serialize.DataCorrupted ->
                                        Board.DataCorrupted

                                    Serialize.SerializerOutOfDate ->
                                        Board.SerializerOutOfDate
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

        RestartLevel ->
            let
                board =
                    Undo.value model.editorBoard
            in
            case Board.init board of
                Nothing ->
                    ( model, Cmd.none )

                Just level ->
                    ( { model
                        | level = level
                      }
                    , Browser.Dom.getViewportOf "editor-viewport"
                        |> Task.attempt (EditorViewportResized >> toMsg)
                    )

        ChangeMode ->
            case model.editorMode of
                EditBoard ->
                    let
                        board =
                            Undo.value model.editorBoard
                    in
                    case Board.init board of
                        Nothing ->
                            ( { model | boardPlayError = Just Board.MissingPlayerSpawn }, Cmd.none )

                        Just level ->
                            ( { model
                                | editorMode = TestGame
                                , level = level
                              }
                            , Browser.Dom.getViewportOf "editor-viewport"
                                |> Task.attempt (EditorViewportResized >> toMsg)
                            )

                TestGame ->
                    ( { model
                        | editorMode = EditBoard
                      }
                    , Browser.Dom.getViewportOf "editor-viewport"
                        |> Task.attempt (EditorViewportResized >> toMsg)
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
            case model.editorMode of
                EditBoard ->
                    handleEditorKeyPressed toSharedMsg sharedModel toMsg key { model | editorKeysDown = Set.insert key model.editorKeysDown }

                TestGame ->
                    ( Board.handleGameKeyPressed
                        (\m -> { m | showSettings = True })
                        sharedModel.inputMapping
                        key
                        { model | editorKeysDown = Set.insert key model.editorKeysDown }
                    , Cmd.none
                    )

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
                                                    Board.Empty
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
                                    |> Serialize.encodeToJson Board.boardCodec
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
                                                    Board.PlayerSpawn _ ->
                                                        board.blocks
                                                            |> Dict.map
                                                                (\_ block ->
                                                                    case block of
                                                                        Board.PlayerSpawn _ ->
                                                                            Board.Empty

                                                                        _ ->
                                                                            block
                                                                )
                                                            |> Dict.insert model.editorCursor
                                                                model.selectedBlockType

                                                    Board.EnemySpawner _ ->
                                                        board.blocks
                                                            |> Dict.map
                                                                (\_ block ->
                                                                    case block of
                                                                        Board.EnemySpawner _ ->
                                                                            Board.Wall

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
                                    |> Serialize.encodeToJson Board.boardCodec
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

                                    Just Board.MissingPlayerSpawn ->
                                        case Board.findSpawn (Undo.value editorBoard) of
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

        BrowserResized ->
            ( model
            , Browser.Dom.getViewportOf "editor-viewport"
                |> Task.attempt (EditorViewportResized >> toMsg)
            )

        EditorViewportResized (Err _) ->
            ( model, Cmd.none )

        EditorViewportResized (Ok { viewport }) ->
            ( { model
                | screenSize =
                    Just
                        { width = round viewport.width
                        , height = round viewport.height
                        }
              }
            , Cmd.none
            )


moveCameraByMouse : Json.Encode.Value -> Point2d Pixels Board.ScreenCoordinates -> Model -> ( Model, Cmd msg )
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


moveCursorByMouse : Point2d Pixels Board.ScreenCoordinates -> Model -> ( Model, Cmd msg )
moveCursorByMouse offset model =
    case model.screenSize of
        Nothing ->
            ( model, Cmd.none )

        Just screenSize ->
            let
                ray : Axis3d Length.Meters Board.WorldCoordinates
                ray =
                    Camera3d.ray
                        (editorCamera model)
                        (Rectangle2d.from
                            (Point2d.pixels 0 (toFloat screenSize.height))
                            (Point2d.pixels (toFloat screenSize.width) 0)
                        )
                        offset

                editorBoard =
                    Undo.value model.editorBoard

                maybeIntersection =
                    Dict.foldl
                        (\point block maybeInter ->
                            case block of
                                Board.Empty ->
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
                                                    (Board.pointToPoint3d point)
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
                                        |> Board.point3dToPoint

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
                                        |> Board.point3dToPoint

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
                                        |> Board.point3dToPoint
                      }
                    , Cmd.none
                    )


tick : Duration -> Model -> Model
tick deltaMs model =
    case model.editorMode of
        EditBoard ->
            model

        TestGame ->
            { model
                | level =
                    model.level
                        |> Board.tick deltaMs
            }


handleEditorKeyPressed : (Shared.Msg -> msg) -> Shared.LoadedModel -> (Msg -> msg) -> String -> Model -> ( Model, Cmd msg )
handleEditorKeyPressed toSharedMsg sharedModel _ key model =
    if Input.isInputKey sharedModel.inputMapping.cameraOrbit key then
        ( { model | cameraMode = Orbit }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.cameraPan key then
        ( { model | cameraMode = Pan }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.cameraZoom key then
        ( { model | cameraMode = Zoom }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.cameraReset key then
        resetCamera model

    else if Input.isInputKey sharedModel.inputMapping.blockSelect key then
        ( { model | blockEditMode = Select }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.blockAdd key then
        ( { model | blockEditMode = Add }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.blockRemove key then
        ( { model | blockEditMode = Remove }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.blockTypeWall key then
        ( { model | selectedBlockType = Board.Wall }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.blockTypeEdge key then
        ( { model | selectedBlockType = Board.Edge }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.blockTypePointPickup key then
        ( { model | selectedBlockType = Board.PointPickup False }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.blockTypeEnemySpawner key then
        ( { model | selectedBlockType = Board.EnemySpawner Board.defaultEnemySpawnerDetails }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.blockTypePlayerSpawn key then
        ( { model | selectedBlockType = Board.PlayerSpawn { forward = Board.PositiveX, left = Board.PositiveY } }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.undo key then
        undo model

    else if Input.isInputKey sharedModel.inputMapping.redo key then
        redo model

    else if Input.isInputKey sharedModel.inputMapping.toggleSettings key then
        showSettings (not model.showSettings) model

    else if key == "p" then
        ( model
        , (Shared.SetBlockPalette <|
            case sharedModel.blockPalette of
                Board.SimpleBlocks ->
                    Board.RainbowBlocks

                Board.RainbowBlocks ->
                    Board.SimpleBlocks
          )
            |> Task.succeed
            |> Task.perform toSharedMsg
        )

    else if key == "Escape" then
        ( { model | showSettings = True }
        , Cmd.none
        )

    else
        ( model, Cmd.none )


showSettings : Bool -> Model -> ( Model, Cmd msg )
showSettings show model =
    ( { model | showSettings = show }, Cmd.none )


resetCamera : Model -> ( Model, Cmd msg )
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


undo : Model -> ( Model, Cmd msg )
undo model =
    ( { model
        | editorBoard = Undo.undo model.editorBoard
        , selectedBlock = Nothing
      }
    , Cmd.none
    )


redo : Model -> ( Model, Cmd msg )
redo model =
    ( { model
        | editorBoard = Undo.redo model.editorBoard
        , selectedBlock = Nothing
      }
    , Cmd.none
    )


view : { setScreen : Screen -> msg, toSharedMsg : Shared.Msg -> msg, sharedModel : Shared.LoadedModel, toMsg : Msg -> msg, model : Model } -> List (Html msg)
view { setScreen, toSharedMsg, sharedModel, toMsg, model } =
    [ Html.div
        [ Html.Attributes.style "display" "grid"
        , Html.Attributes.style "grid-template-columns" <|
            case model.editorMode of
                EditBoard ->
                    "calc(100vw - 25rem) 25rem"

                TestGame ->
                    "calc(100vw - 10rem) 10rem"
        , Html.Attributes.style "grid-template-rows" <|
            case model.editorMode of
                EditBoard ->
                    "auto auto"

                TestGame ->
                    "auto"
        , Html.Attributes.style "width" "100vw"
        ]
        [ Html.div
            ([ Html.Attributes.id "editor-viewport"
             , case model.editorMode of
                EditBoard ->
                    Html.Attributes.class ""

                TestGame ->
                    Html.Attributes.style "width" <|
                        -- "calc(100vw - 25rem)"
                        "100vw"
             , Html.Attributes.style "grid-column" <|
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
                            [ Html.Events.on "pointerdown" (decodeMouseDown toMsg)
                            , Html.Events.on "pointermove" (decodePointerMove toMsg Json.Encode.null)
                            , Html.Attributes.property "___setPointerCapture" Json.Encode.null
                            ]

                        InteractionStart pointer ->
                            [ Html.Events.on "pointerup" (decodeMouseUp toMsg)
                            , Html.Events.on "pointermove" (decodePointerMove toMsg pointer)
                            , Html.Attributes.property "___setPointerCapture" pointer
                            ]

                        InteractionMoving pointer ->
                            [ Html.Events.on "pointerup" (decodeMouseUp toMsg)
                            , Html.Events.on "pointermove" (decodePointerMove toMsg pointer)
                            , Html.Attributes.property "___setPointerCapture" pointer
                            ]
                   )
            )
            [ case model.screenSize of
                Nothing ->
                    Html.div [] [ Html.text "Loading..." ]

                Just screenSize ->
                    let
                        lights =
                            case model.editorMode of
                                TestGame ->
                                    Board.gameLights model.level.board (Frame3d.originPoint model.level.playerFrame)

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
                    Board.view3dScene
                        lights
                        (case model.editorMode of
                            TestGame ->
                                sharedModel.screenSize

                            EditBoard ->
                                screenSize
                        )
                        (case model.editorMode of
                            TestGame ->
                                Board.gamePlayCamera model.level.playerFrame

                            EditBoard ->
                                editorCamera model
                        )
                        (case model.editorMode of
                            EditBoard ->
                                let
                                    editorBoard =
                                        model.editorBoard
                                            |> Undo.value
                                in
                                List.concat
                                    [ editorBoard.blocks
                                        |> Dict.toList
                                        |> List.map (viewBlock sharedModel editorBoard model)
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
                                    [ model.level.board.blocks
                                        |> Dict.toList
                                        |> List.map Board.viewBlock
                                    , [ Board.viewPlayer model.level ]
                                    , List.map Board.viewEnemy model.level.enemies
                                    ]
                        )
            ]
        , viewHeader setScreen toSharedMsg sharedModel toMsg model
        , viewSettings toSharedMsg sharedModel toMsg model
        , case model.editorMode of
            TestGame ->
                Html.div
                    [ Html.Attributes.style "grid-column" "2"
                    , Html.Attributes.style "grid-row" "1"
                    , Html.Attributes.style "padding" "0.5rem"
                    ]
                    [ Html.div
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "gap" "1rem"
                        , Html.Attributes.style "align-items" "flex-start"
                        ]
                        [ Html.button
                            [ Html.Attributes.type_ "button"
                            , Html.Events.onClick (toMsg (ShowSettings True))
                            , Html.Attributes.title ("Settings - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.toggleSettings)
                            ]
                            [ Phosphor.gear Phosphor.Regular
                                |> Phosphor.toHtml []
                            ]
                        , Html.button
                            [ Html.Attributes.type_ "button"
                            , Html.Events.onClick (toMsg ChangeMode)
                            ]
                            [ Html.text "Edit Level"
                            ]
                        ]
                    , Board.viewGameOver model.level
                        (Html.div
                            [ Html.Attributes.style "display" "flex"
                            , Html.Attributes.style "flex-direction" "column"
                            , Html.Attributes.style "gap" "0.5rem"
                            ]
                            [ Html.button
                                [ Html.Attributes.type_ "button"
                                , Html.Events.onClick (toMsg RestartLevel)
                                ]
                                [ Html.text "Restart" ]
                            , Html.button
                                [ Html.Attributes.type_ "button"
                                , Html.Events.onClick (toMsg ChangeMode)
                                ]
                                [ Html.text "Edit" ]
                            ]
                        )
                    , Board.viewAllPointsCollected model.level
                        (Html.div
                            [ Html.Attributes.style "display" "flex"
                            , Html.Attributes.style "flex-direction" "column"
                            , Html.Attributes.style "gap" "0.5rem"
                            ]
                            [ Html.button
                                [ Html.Attributes.type_ "button"
                                , Html.Events.onClick (toMsg RestartLevel)
                                ]
                                [ Html.text "Restart" ]
                            , Html.button
                                [ Html.Attributes.type_ "button"
                                , Html.Events.onClick (toMsg ChangeMode)
                                ]
                                [ Html.text "Edit" ]
                            ]
                        )
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
                    , Html.Attributes.style "width" "25rem"
                    , Html.Attributes.style "overflow" "auto"
                    ]
                    [ case model.selectedBlock of
                        Nothing ->
                            Html.span [] [ Html.text "No block selected" ]

                        Just ( point, block ) ->
                            case block of
                                Board.Empty ->
                                    Html.span [] [ Html.text "Empty block" ]

                                Board.Edge ->
                                    Html.span [] [ Html.text "Edge" ]

                                Board.Wall ->
                                    Html.span [] [ Html.text "Wall" ]

                                Board.PointPickup _ ->
                                    Html.span [] [ Html.text "Point Pickup" ]

                                Board.EnemySpawner details ->
                                    Html.form
                                        []
                                        [ Html.span [] [ Html.text "Enemy Spawner" ]
                                        , Html.br [] []
                                        , Html.label []
                                            [ Html.span [] [ Html.text "Seconds betwwen spawns " ]
                                            , Html.input
                                                [ Html.Attributes.type_ "number"
                                                , Html.Attributes.min "0.5"
                                                , Html.Attributes.max "10"
                                                , Html.Attributes.step "0.5"
                                                , details.timeBetweenSpawns
                                                    |> Duration.inSeconds
                                                    |> String.fromFloat
                                                    |> Html.Attributes.value
                                                ]
                                                []
                                            ]
                                        ]

                                Board.PlayerSpawn details ->
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
                                                    [ Board.PositiveX
                                                    , Board.NegativeX
                                                    , Board.PositiveY
                                                    , Board.NegativeY
                                                    , Board.PositiveZ
                                                    , Board.NegativeZ
                                                    ]
                                                , toLabel = Board.axisToLabel
                                                , toKey = Board.axisToLabel
                                                , onSelect =
                                                    \value ->
                                                        toMsg <|
                                                            case value of
                                                                Nothing ->
                                                                    SetBlock point (Board.PlayerSpawn details)

                                                                Just axis ->
                                                                    SetBlock point (Board.PlayerSpawn { details | forward = axis })
                                                }
                                            ]
                                        , Html.br [] []
                                        , Html.label []
                                            [ Html.span [] [ Html.text "Left Direction " ]
                                            , Html.Extra.select
                                                []
                                                { value = Just details.left
                                                , options =
                                                    [ Board.PositiveX
                                                    , Board.NegativeX
                                                    , Board.PositiveY
                                                    , Board.NegativeY
                                                    , Board.PositiveZ
                                                    , Board.NegativeZ
                                                    ]
                                                , toLabel = Board.axisToLabel
                                                , toKey = Board.axisToLabel
                                                , onSelect =
                                                    \value ->
                                                        toMsg <|
                                                            case value of
                                                                Nothing ->
                                                                    SetBlock point (Board.PlayerSpawn details)

                                                                Just axis ->
                                                                    SetBlock point (Board.PlayerSpawn { details | left = axis })
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
                                    , Html.Events.onInput (MaxXChanged >> toMsg)
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
                                    , onInputLow = round >> XLowerVisibleChanged >> toMsg
                                    , onInputHigh = round >> XUpperVisibleChanged >> toMsg
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
                                    , Html.Events.onInput (MaxYChanged >> toMsg)
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
                                    , onInputLow = round >> YLowerVisibleChanged >> toMsg
                                    , onInputHigh = round >> YUpperVisibleChanged >> toMsg
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
                                    , Html.Events.onInput (MaxZChanged >> toMsg)
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
                                    , onInputLow = round >> ZLowerVisibleChanged >> toMsg
                                    , onInputHigh = round >> ZUpperVisibleChanged >> toMsg
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
                        [ Html.Events.onSubmit (toMsg (LoadEditorBoard model.boardEncoding))
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
                                , Html.Events.onInput (EncodingChanged >> toMsg)
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
                                            Board.DataCorrupted ->
                                                "Board data is corrupted"

                                            Board.SerializerOutOfDate ->
                                                "Board data is from a different version of the game"

                                            Board.OtherError ->
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
                                [ Board.DefaultBoard
                                , Board.BasicMiniBoard
                                , Board.ZigZagBoard
                                ]
                            , toLabel =
                                \value ->
                                    case value of
                                        Board.DefaultBoard ->
                                            "9 x 9 blank slate"

                                        Board.BasicMiniBoard ->
                                            "Basic mini"

                                        Board.ZigZagBoard ->
                                            "Zig zag"
                            , toKey =
                                \value ->
                                    case value of
                                        Board.DefaultBoard ->
                                            "DefaultBoard"

                                        Board.BasicMiniBoard ->
                                            "BasicMiniBoard"

                                        Board.ZigZagBoard ->
                                            "ZigZagBoard"
                            , onSelect =
                                \value ->
                                    toMsg <|
                                        case value of
                                            Nothing ->
                                                NoOp

                                            Just Board.DefaultBoard ->
                                                LoadEditorBoard Board.defaultBoard

                                            Just Board.BasicMiniBoard ->
                                                LoadEditorBoard Board.basicMiniBoard

                                            Just Board.ZigZagBoard ->
                                                LoadEditorBoard Board.zigZagBoard
                            }
                        ]
                    ]
        ]
    ]


viewSettings : (Shared.Msg -> msg) -> Shared.LoadedModel -> (Msg -> msg) -> Model -> Html msg
viewSettings toSharedMsg sharedModel toMsg model =
    Html.Extra.modal { open = model.showSettings, onClose = toMsg (ShowSettings False) }
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
                , Html.Events.onClick (toMsg (ShowSettings False))
                ]
                [ Phosphor.xCircle Phosphor.Regular
                    |> Phosphor.toHtml []
                ]
            ]
        , Html.span [] [ Html.text "Hold 'Shift' and move mouse to use camera actions (orbit, pan, zoom)" ]
        , Html.br [] []
        , Html.br [] []
        , let
            viewMapping =
                Input.viewMapping (Shared.SetMapping >> toSharedMsg)
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
                          , keys = sharedModel.inputMapping.moveUp
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
                          , keys = sharedModel.inputMapping.moveDown
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
                          , keys = sharedModel.inputMapping.moveLeft
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
                          , keys = sharedModel.inputMapping.moveRight
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
                          , keys = sharedModel.inputMapping.cameraOrbit
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
                          , keys = sharedModel.inputMapping.cameraPan
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
                          , keys = sharedModel.inputMapping.cameraZoom
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
                          , keys = sharedModel.inputMapping.cameraReset
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
                          , keys = sharedModel.inputMapping.blockSelect
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
                          , keys = sharedModel.inputMapping.blockAdd
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
                          , keys = sharedModel.inputMapping.blockRemove
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
                          , keys = sharedModel.inputMapping.blockTypeWall
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
                          , keys = sharedModel.inputMapping.blockTypeEdge
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
                          , keys = sharedModel.inputMapping.blockTypePointPickup
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
                          , keys = sharedModel.inputMapping.blockTypePlayerSpawn
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
                          , keys = sharedModel.inputMapping.undo
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
                          , keys = sharedModel.inputMapping.redo
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
                            , keys = sharedModel.inputMapping.toggleSettings
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


editorCamera : Model -> Camera3d Length.Meters Board.WorldCoordinates
editorCamera model =
    Camera3d.perspective
        { viewpoint = editorViewpoint model
        , verticalFieldOfView = Angle.degrees 30
        }


editorViewpoint : Model -> Viewpoint3d Length.Meters Board.WorldCoordinates
editorViewpoint model =
    Viewpoint3d.orbit
        { focalPoint = model.cameraFocalPoint
        , azimuth = model.cameraRotation
        , elevation = model.cameraElevation
        , distance = model.cameraDistance
        , groundPlane = SketchPlane3d.xy
        }


decodeMouseDown : (Msg -> msg) -> Json.Decode.Decoder msg
decodeMouseDown toMsg =
    Json.Decode.field "button" Json.Decode.int
        |> Json.Decode.andThen
            (\button ->
                if button == 0 then
                    Json.Decode.map (MouseDown >> toMsg)
                        (Json.Decode.field "pointerId" Json.Decode.value)

                else
                    Json.Decode.fail "Non-primary mouse button"
            )


decodeMouseUp : (Msg -> msg) -> Json.Decode.Decoder msg
decodeMouseUp toMsg =
    Json.Decode.field "button" Json.Decode.int
        |> Json.Decode.andThen
            (\button ->
                if button == 0 then
                    Json.Decode.succeed (toMsg MouseUp)

                else
                    Json.Decode.fail "Non-primary mouse button"
            )


decodePointerMove : (Msg -> msg) -> Json.Decode.Value -> Json.Decode.Decoder msg
decodePointerMove toMsg pointer =
    Json.Decode.map4
        (\ox oy mx my ->
            toMsg
                (MouseMove pointer
                    (Point2d.pixels ox oy)
                    (Point2d.pixels mx my)
                )
        )
        (Json.Decode.field "offsetX" Json.Decode.float)
        (Json.Decode.field "offsetY" Json.Decode.float)
        (Json.Decode.field "movementX" Json.Decode.float)
        (Json.Decode.field "movementY" Json.Decode.float)


viewBlock : Shared.LoadedModel -> Board -> Model -> ( Board.Point, Board.Block ) -> Scene3d.Entity Board.WorldCoordinates
viewBlock sharedModel board model ( point, block ) =
    let
        ( x, y, z ) =
            point
    in
    if x < model.xLowerVisible || x > model.xUpperVisible || y < model.yLowerVisible || y > model.yUpperVisible || z < model.zLowerVisible || z > model.zUpperVisible then
        Scene3d.nothing

    else
        case block of
            Board.Wall ->
                -- let
                --     ( wallMesh, wallTexture ) =
                --         sharedModel.wallMesh
                -- in
                Scene3d.blockWithShadow
                    (Scene3d.Material.matte <|
                        case sharedModel.blockPalette of
                            Board.SimpleBlocks ->
                                Color.gray

                            Board.RainbowBlocks ->
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

            -- Scene3d.meshWithShadow
            --     (Scene3d.Material.color Color.gray)
            --     wallMesh
            --     (Scene3d.Mesh.shadow wallMesh)
            Board.PointPickup collected ->
                if collected && (model.editorMode == TestGame) then
                    Scene3d.nothing

                else
                    Scene3d.sphere
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

            Board.EnemySpawner _ ->
                let
                    center =
                        Point3d.meters
                            (toFloat x)
                            (toFloat y)
                            (toFloat z)

                    length =
                        Length.meters 1
                in
                Scene3d.group
                    [ Scene3d.cylinderWithShadow
                        (Scene3d.Material.color Color.red)
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveX
                            { radius = Length.meters 0.05
                            , length = length
                            }
                            |> Cylinder3d.placeIn
                                (center
                                    |> Frame3d.atPoint
                                    |> Frame3d.translateAlongOwn Frame3d.yAxis
                                        (Length.meters 0.4)
                                    |> Frame3d.translateAlongOwn Frame3d.zAxis
                                        (Length.meters -0.4)
                                )
                        )
                    , Scene3d.cylinderWithShadow
                        (Scene3d.Material.color Color.red)
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveX
                            { radius = Length.meters 0.05
                            , length = length
                            }
                            |> Cylinder3d.placeIn
                                (center
                                    |> Frame3d.atPoint
                                    |> Frame3d.translateAlongOwn Frame3d.yAxis
                                        (Length.meters 0.4)
                                    |> Frame3d.translateAlongOwn Frame3d.zAxis
                                        (Length.meters 0.4)
                                )
                        )
                    , Scene3d.cylinderWithShadow
                        (Scene3d.Material.color Color.red)
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveX
                            { radius = Length.meters 0.05
                            , length = length
                            }
                            |> Cylinder3d.placeIn
                                (center
                                    |> Frame3d.atPoint
                                    |> Frame3d.translateAlongOwn Frame3d.yAxis
                                        (Length.meters -0.4)
                                    |> Frame3d.translateAlongOwn Frame3d.zAxis
                                        (Length.meters 0.4)
                                )
                        )
                    , Scene3d.cylinderWithShadow
                        (Scene3d.Material.color Color.red)
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveX
                            { radius = Length.meters 0.05
                            , length = length
                            }
                            |> Cylinder3d.placeIn
                                (center
                                    |> Frame3d.atPoint
                                    |> Frame3d.translateAlongOwn Frame3d.yAxis
                                        (Length.meters -0.4)
                                    |> Frame3d.translateAlongOwn Frame3d.zAxis
                                        (Length.meters -0.4)
                                )
                        )

                    --
                    , Scene3d.cylinderWithShadow
                        (Scene3d.Material.color Color.red)
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveY
                            { radius = Length.meters 0.05
                            , length = length
                            }
                            |> Cylinder3d.placeIn
                                (center
                                    |> Frame3d.atPoint
                                    |> Frame3d.translateAlongOwn Frame3d.xAxis
                                        (Length.meters -0.4)
                                    |> Frame3d.translateAlongOwn Frame3d.zAxis
                                        (Length.meters -0.4)
                                )
                        )
                    , Scene3d.cylinderWithShadow
                        (Scene3d.Material.color Color.red)
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveY
                            { radius = Length.meters 0.05
                            , length = length
                            }
                            |> Cylinder3d.placeIn
                                (center
                                    |> Frame3d.atPoint
                                    |> Frame3d.translateAlongOwn Frame3d.xAxis
                                        (Length.meters 0.4)
                                    |> Frame3d.translateAlongOwn Frame3d.zAxis
                                        (Length.meters -0.4)
                                )
                        )
                    , Scene3d.cylinderWithShadow
                        (Scene3d.Material.color Color.red)
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveY
                            { radius = Length.meters 0.05
                            , length = length
                            }
                            |> Cylinder3d.placeIn
                                (center
                                    |> Frame3d.atPoint
                                    |> Frame3d.translateAlongOwn Frame3d.xAxis
                                        (Length.meters -0.4)
                                    |> Frame3d.translateAlongOwn Frame3d.zAxis
                                        (Length.meters 0.4)
                                )
                        )
                    , Scene3d.cylinderWithShadow
                        (Scene3d.Material.color Color.red)
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveY
                            { radius = Length.meters 0.05
                            , length = length
                            }
                            |> Cylinder3d.placeIn
                                (center
                                    |> Frame3d.atPoint
                                    |> Frame3d.translateAlongOwn Frame3d.xAxis
                                        (Length.meters 0.4)
                                    |> Frame3d.translateAlongOwn Frame3d.zAxis
                                        (Length.meters 0.4)
                                )
                        )

                    --
                    , Scene3d.cylinderWithShadow
                        (Scene3d.Material.color Color.red)
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveZ
                            { radius = Length.meters 0.05
                            , length = length
                            }
                            |> Cylinder3d.placeIn
                                (center
                                    |> Frame3d.atPoint
                                    |> Frame3d.translateAlongOwn Frame3d.xAxis
                                        (Length.meters 0.4)
                                    |> Frame3d.translateAlongOwn Frame3d.yAxis
                                        (Length.meters 0.4)
                                )
                        )
                    , Scene3d.cylinderWithShadow
                        (Scene3d.Material.color Color.red)
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveZ
                            { radius = Length.meters 0.05
                            , length = length
                            }
                            |> Cylinder3d.placeIn
                                (center
                                    |> Frame3d.atPoint
                                    |> Frame3d.translateAlongOwn Frame3d.xAxis
                                        (Length.meters -0.4)
                                    |> Frame3d.translateAlongOwn Frame3d.yAxis
                                        (Length.meters 0.4)
                                )
                        )
                    , Scene3d.cylinderWithShadow
                        (Scene3d.Material.color Color.red)
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveZ
                            { radius = Length.meters 0.05
                            , length = length
                            }
                            |> Cylinder3d.placeIn
                                (center
                                    |> Frame3d.atPoint
                                    |> Frame3d.translateAlongOwn Frame3d.xAxis
                                        (Length.meters 0.4)
                                    |> Frame3d.translateAlongOwn Frame3d.yAxis
                                        (Length.meters -0.4)
                                )
                        )
                    , Scene3d.cylinderWithShadow
                        (Scene3d.Material.color Color.red)
                        (Cylinder3d.centeredOn Point3d.origin
                            Direction3d.positiveZ
                            { radius = Length.meters 0.05
                            , length = length
                            }
                            |> Cylinder3d.placeIn
                                (center
                                    |> Frame3d.atPoint
                                    |> Frame3d.translateAlongOwn Frame3d.xAxis
                                        (Length.meters -0.4)
                                    |> Frame3d.translateAlongOwn Frame3d.yAxis
                                        (Length.meters -0.4)
                                )
                        )
                    ]

            Board.PlayerSpawn { forward, left } ->
                let
                    center =
                        Point3d.meters
                            (toFloat x)
                            (toFloat y)
                            (toFloat z)

                    frame =
                        SketchPlane3d.unsafe
                            { originPoint = Board.pointToPoint3d point
                            , xDirection = Board.axisToDirection3d forward
                            , yDirection = Board.axisToDirection3d left
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
                            (Frame3d.xDirection frame)
                            { radius = Length.meters 0.125
                            , length = Length.meters 0.75
                            }
                        )
                    , Scene3d.coneWithShadow
                        (Scene3d.Material.emissive (Scene3d.Light.color Color.green)
                            (Luminance.nits 10000)
                        )
                        (Cone3d.startingAt center
                            (Frame3d.yDirection frame)
                            { radius = Length.meters 0.125
                            , length = Length.meters 0.75
                            }
                        )
                    , Scene3d.coneWithShadow
                        (Scene3d.Material.emissive (Scene3d.Light.color Color.blue)
                            (Luminance.nits 10000)
                        )
                        (Cone3d.startingAt center
                            (Frame3d.zDirection frame)
                            { radius = Length.meters 0.125
                            , length = Length.meters 0.75
                            }
                        )
                    ]

            Board.Empty ->
                Scene3d.nothing

            Board.Edge ->
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


viewCursor : Color -> Animation Float -> Board.Point -> Scene3d.Entity Board.WorldCoordinates
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


viewOrientationArrows : Scene3d.Entity Board.WorldCoordinates
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


viewBounds : Board -> Scene3d.Entity Board.WorldCoordinates
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


viewHeader : (Screen -> msg) -> (Shared.Msg -> msg) -> Shared.LoadedModel -> (Msg -> msg) -> Model -> Html msg
viewHeader setScreen _ sharedModel toMsg model =
    case model.editorMode of
        TestGame ->
            -- Html.div
            --     [ Html.Attributes.style "grid-column" "1 /3"
            --     , Html.Attributes.style "grid-row" "1"
            --     , Html.Attributes.style "display" "flex"
            --     , Html.Attributes.style "padding" "0.5rem"
            --     , Html.Attributes.style "gap" "1rem"
            --     ]
            --     [ Html.h3 [] [ Html.text ("Score: " ++ String.fromInt model.level.score) ]
            --     ]
            Board.viewStats model.level

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
                    , Html.Events.onClick (toMsg ChangeMode)
                    ]
                    [ Html.text "Play Level"
                    ]
                , case model.boardPlayError of
                    Nothing ->
                        Html.span
                            [ Html.Attributes.style "width" "12rem"
                            ]
                            [ Html.text ""
                            ]

                    Just Board.MissingPlayerSpawn ->
                        Html.span
                            [ Html.Attributes.style "width" "12rem"
                            , Html.Attributes.style "color" "red"
                            , Html.Attributes.style "background" "white"
                            , Html.Attributes.style "padding" "0.25rem 0.5rem"
                            ]
                            [ Html.text "Missing Player Spawn"
                            ]
                , Html.div
                    [ Html.Attributes.attribute "role" "group" ]
                    [ Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg Undo)
                        , Html.Attributes.title ("Undo - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.undo)
                        , Html.Attributes.Extra.aria "disabled" <|
                            Html.Attributes.Extra.bool (not <| Undo.canUndo model.editorBoard)
                        ]
                        [ Phosphor.arrowCounterClockwise Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg Redo)
                        , Html.Attributes.title ("Redo - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.redo)
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
                        , Html.Events.onClick (toMsg (SetCameraMode Orbit))
                        , Html.Attributes.title ("Camera orbit - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.cameraOrbit)
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.cameraMode == Orbit)
                        ]
                        [ Phosphor.arrowsClockwise Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg (SetCameraMode Pan))
                        , Html.Attributes.title ("Camera pan - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.cameraPan)
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.cameraMode == Pan)
                        ]
                        [ Phosphor.arrowsOutCardinal Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg (SetCameraMode Zoom))
                        , Html.Attributes.title ("Camera zoom - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.cameraZoom)
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.cameraMode == Zoom)
                        ]
                        [ Phosphor.arrowsVertical Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg ResetCamera)
                        , Html.Attributes.title ("Camera reset - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.cameraReset)
                        ]
                        [ Phosphor.clockCounterClockwise Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    ]
                , Html.div
                    [ Html.Attributes.attribute "role" "group" ]
                    [ Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg (SetBlockEditMode Select))
                        , Html.Attributes.title ("Select block - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.blockSelect)
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.blockEditMode == Select)
                        ]
                        [ Phosphor.cursor Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg (SetBlockEditMode Add))
                        , Html.Attributes.title ("Add block - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.blockAdd)
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.blockEditMode == Add)
                        ]
                        [ Phosphor.plus Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg (SetBlockEditMode Remove))
                        , Html.Attributes.title ("Remove block - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.blockRemove)
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
                            Html.Attributes.Extra.bool (model.selectedBlockType == Board.Wall)
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg (BlockTypeSelected Board.Wall))
                        , Html.Attributes.title ("Wall - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.blockTypeWall)
                        ]
                        [ Html.text "Wall"
                        ]
                    , Html.button
                        [ Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.selectedBlockType == Board.Edge)
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg (BlockTypeSelected Board.Edge))
                        , Html.Attributes.title ("Edge - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.blockTypeEdge)
                        ]
                        [ Html.text "Edge"
                        ]
                    , Html.button
                        [ Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.selectedBlockType == Board.PointPickup False)
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg (BlockTypeSelected (Board.PointPickup False)))
                        , Html.Attributes.title ("Point Pickup - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.blockTypePointPickup)
                        ]
                        [ Html.text "Point Pickup"
                        ]
                    , Html.button
                        [ Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.selectedBlockType == Board.EnemySpawner Board.defaultEnemySpawnerDetails)
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg (BlockTypeSelected (Board.EnemySpawner Board.defaultEnemySpawnerDetails)))
                        , Html.Attributes.title ("Enemy Spawner - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.blockTypeEnemySpawner)
                        ]
                        [ Html.text "Enemy Spawner"
                        ]
                    , Html.button
                        [ Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.selectedBlockType == Board.PlayerSpawn { forward = Board.PositiveX, left = Board.PositiveY })
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg (BlockTypeSelected (Board.PlayerSpawn { forward = Board.PositiveX, left = Board.PositiveY })))
                        , Html.Attributes.title ("Player Spawn - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.blockTypePlayerSpawn)
                        ]
                        [ Html.text "Player Spawn"
                        ]
                    ]
                , Html.button
                    [ Html.Attributes.Extra.aria "pressed" <|
                        Html.Attributes.Extra.bool model.showBoardBounds
                    , Html.Attributes.type_ "button"
                    , Html.Events.onClick (toMsg (ShowBoardBounds (not model.showBoardBounds)))
                    , Html.Attributes.title "Show board bounds"
                    ]
                    [ Phosphor.gridNine Phosphor.Regular
                        |> Phosphor.toHtml []
                    ]
                , Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Events.onClick (toMsg (ShowSettings True))
                    , Html.Attributes.title ("Settings - " ++ Input.viewInputKeyHoverText sharedModel.inputMapping.toggleSettings)
                    ]
                    [ Phosphor.gear Phosphor.Regular
                        |> Phosphor.toHtml []
                    ]
                , Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Events.onClick (setScreen Screen.Menu)
                    , Html.Attributes.style "margin-left" "auto"
                    ]
                    [ Html.text "Exit" ]
                ]
