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
import Html.Range
import Json.Decode
import Json.Encode
import Length
import LineSegment3d
import List.Cartesian
import Luminance
import Phosphor
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
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
import Viewpoint3d


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { board : Board
    , editorBoard : Undo.Stack Board
    , boardLoadError : Maybe BoardLoadError
    , screenSize : { width : Int, height : Int }
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
    , mouseDragging : EditorMouseInteraction
    , cursorBounce : Animation Float
    , boardEncoding : String
    , mode : Mode
    , editMode : EditMode
    , showBoardBounds : Bool
    , selectedBlock : Maybe ( Point, Block )
    , playerFrame : Frame3d Length.Meters WorldCoordinates { defines : {} }
    , playerFacing : Facing
    , playerWantFacing : Facing
    , playerMovingAcrossEdge : Maybe Angle
    , editorMaxXRaw : String
    , editorMaxYRaw : String
    , editorMaxZRaw : String
    }


type EditorMouseInteraction
    = NoInteraction
    | InteractionStart Json.Decode.Value
    | InteractionMoving Json.Decode.Value


type Mode
    = Editor
    | Game


type EditMode
    = Add
    | Remove
    | Select


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
            8

        maxY =
            8

        maxZ =
            8

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
      , editorBoard = Undo.init board
      , boardLoadError = Nothing
      , screenSize = { width = 800, height = 600 }
      , editorCursor = ( 0, 0, 0 )
      , editorKeysDown = Set.empty
      , cameraRotation = Angle.degrees 225
      , cameraElevation = Angle.degrees 15
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
      , mode = Editor
      , editMode = Select
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
      }
    , Cmd.none
    )


type BoardLoadError
    = DataCorrupted
    | SerializerOutOfDate
    | OtherError


subscriptions : Model -> Sub Msg
subscriptions _ =
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
    = Tick Float
    | KeyPressed String
    | KeyDown String
    | KeyUp String
    | MouseDown Json.Decode.Value
    | MouseUp
    | MouseMove Json.Decode.Value (Point2d Pixels ScreenCoordinates) (Point2d Pixels ScreenCoordinates)
    | EncodingChanged String
    | LoadBoard
    | ChangeMode
    | SetEditMode EditMode
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick deltaMs ->
            ( model
                |> tickPlayer deltaMs
            , Cmd.none
            )

        EncodingChanged boardEncoding ->
            ( { model | boardEncoding = boardEncoding }, Cmd.none )

        LoadBoard ->
            let
                loadedBoard =
                    model.boardEncoding
                        |> Json.Decode.decodeString Json.Decode.value
                        |> Result.withDefault Json.Encode.null
                        |> Serialize.decodeFromJson boardCodec
                        |> Result.map Undo.init
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
                    ( { model
                        | editorBoard = editorBoard
                        , boardLoadError = Nothing
                        , selectedBlock = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | boardLoadError = Just error
                      }
                    , Cmd.none
                    )

        ChangeMode ->
            case model.mode of
                Editor ->
                    let
                        board =
                            Undo.value model.editorBoard
                    in
                    case findSpawn board of
                        Nothing ->
                            Debug.todo "No player spawn found"

                        Just spawnFrame ->
                            ( { model
                                | mode = Game
                                , board = board
                                , playerFrame = spawnFrame
                              }
                            , Cmd.none
                            )

                Game ->
                    ( { model
                        | mode = Editor
                      }
                    , Cmd.none
                    )

        SetEditMode editMode ->
            ( { model | editMode = editMode }, Cmd.none )

        Undo ->
            ( { model
                | editorBoard = Undo.undo model.editorBoard
                , selectedBlock = Nothing
              }
            , Cmd.none
            )

        Redo ->
            ( { model
                | editorBoard = Undo.redo model.editorBoard
                , selectedBlock = Nothing
              }
            , Cmd.none
            )

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
                case model.editMode of
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
                    { model
                        | editorMaxXRaw = maxXStr
                        , editorBoard =
                            Undo.insertWith
                                (\editorBoard ->
                                    { editorBoard
                                        | maxX = maxX
                                        , blocks =
                                            if maxX < (editorBoard.maxX + 1) then
                                                List.Cartesian.map3 (\x y z -> ( x, y, z ))
                                                    (List.range maxX (editorBoard.maxX + 1))
                                                    (List.range 0 editorBoard.maxY)
                                                    (List.range 0 editorBoard.maxZ)
                                                    |> List.foldl Dict.remove editorBoard.blocks

                                            else
                                                editorBoard.blocks
                                    }
                                )
                                model.editorBoard
                        , xLowerVisible = min model.xLowerVisible maxX
                        , xUpperVisible =
                            if model.xUpperVisible + 1 == (Undo.value model.editorBoard).maxX then
                                maxX - 1

                            else
                                min model.xUpperVisible (maxX - 1)
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
                    { model
                        | editorMaxYRaw = maxYStr
                        , editorBoard =
                            Undo.insertWith
                                (\editorBoard ->
                                    { editorBoard
                                        | maxY = maxY
                                        , blocks =
                                            if maxY < (editorBoard.maxY + 1) then
                                                List.Cartesian.map3 (\x y z -> ( x, y, z ))
                                                    (List.range 0 editorBoard.maxX)
                                                    (List.range maxY (editorBoard.maxY + 1))
                                                    (List.range 0 editorBoard.maxZ)
                                                    |> List.foldl Dict.remove editorBoard.blocks

                                            else
                                                editorBoard.blocks
                                    }
                                )
                                model.editorBoard
                        , yLowerVisible = min model.yLowerVisible maxY
                        , yUpperVisible =
                            if model.yUpperVisible + 1 == (Undo.value model.editorBoard).maxY then
                                maxY - 1

                            else
                                min model.yUpperVisible (maxY - 1)
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
                    { model
                        | editorMaxZRaw = maxZStr
                        , editorBoard =
                            Undo.insertWith
                                (\editorBoard ->
                                    { editorBoard
                                        | maxZ = maxZ
                                        , blocks =
                                            if maxZ < (editorBoard.maxZ + 1) then
                                                List.Cartesian.map3 (\x y z -> ( x, y, z ))
                                                    (List.range 0 editorBoard.maxX)
                                                    (List.range 0 editorBoard.maxY)
                                                    (List.range maxZ (editorBoard.maxZ + 1))
                                                    |> List.foldl Dict.remove editorBoard.blocks

                                            else
                                                editorBoard.blocks
                                    }
                                )
                                model.editorBoard
                        , zLowerVisible = min model.zLowerVisible maxZ
                        , zUpperVisible =
                            if model.zUpperVisible + 1 == (Undo.value model.editorBoard).maxZ then
                                maxZ - 1

                            else
                                min model.zUpperVisible (maxZ - 1)
                    }
            , Cmd.none
            )

        ShowBoardBounds show ->
            ( { model | showBoardBounds = show }, Cmd.none )

        KeyPressed key ->
            case model.mode of
                Editor ->
                    ( model, Cmd.none )

                Game ->
                    handleGameKeyPressed key model


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
                    case model.editMode of
                        Remove ->
                            Point3d.along (Axis3d.reverse intersection) (Length.meters 0.5)
                                |> point3dToPoint

                        Add ->
                            Point3d.along intersection (Length.meters 0.5)
                                |> point3dToPoint

                        Select ->
                            Point3d.along (Axis3d.reverse intersection) (Length.meters 0.5)
                                |> point3dToPoint
              }
            , Cmd.none
            )


editorCamera : Model -> Camera3d Length.Meters WorldCoordinates
editorCamera model =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbit
                { focalPoint = Point3d.meters 3.5 3.5 2
                , azimuth = model.cameraRotation
                , elevation = model.cameraElevation
                , distance = Length.meters 30
                , groundPlane = SketchPlane3d.xy
                }
        , verticalFieldOfView = Angle.degrees 30
        }


tickPlayer : Float -> Model -> Model
tickPlayer deltaMs model =
    case model.mode of
        Editor ->
            model

        Game ->
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
            in
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


handleGameKeyPressed : String -> Model -> ( Model, Cmd Msg )
handleGameKeyPressed key model =
    case key of
        "w" ->
            ( { model | playerWantFacing = Forward }, Cmd.none )

        "s" ->
            ( { model | playerWantFacing = Backward }, Cmd.none )

        "d" ->
            ( { model | playerWantFacing = Right }, Cmd.none )

        "a" ->
            ( { model | playerWantFacing = Left }, Cmd.none )

        _ ->
            ( model, Cmd.none )


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
        [ Html.div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns" <|
                case model.mode of
                    Editor ->
                        "auto 20rem"

                    Game ->
                        "auto 8rem"
            , Html.Attributes.style "grid-template-rows" <|
                case model.mode of
                    Editor ->
                        "auto auto"

                    Game ->
                        "auto"
            ]
            [ Html.div
                ([ Html.Attributes.style "grid-column" <|
                    case model.mode of
                        Editor ->
                            "1"

                        Game ->
                            "1 / 2"
                 , Html.Attributes.style "grid-row" <|
                    case model.mode of
                        Editor ->
                            "2"

                        Game ->
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
                [ Scene3d.sunny
                    { clipDepth = Length.meters 1
                    , background = Scene3d.backgroundColor Color.gray
                    , shadows = True
                    , dimensions = ( Pixels.int model.screenSize.width, Pixels.int model.screenSize.height )
                    , upDirection = Direction3d.positiveZ
                    , camera =
                        case model.mode of
                            Game ->
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

                            Editor ->
                                editorCamera model
                    , sunlightDirection =
                        Direction3d.positiveZ
                            |> Direction3d.rotateAround Axis3d.x (Angle.degrees 60)
                            |> Direction3d.rotateAround Axis3d.z
                                (model.cameraRotation
                                    |> Quantity.plus (Angle.degrees -60)
                                )
                    , entities =
                        case model.mode of
                            Editor ->
                                let
                                    editorBoard =
                                        model.editorBoard
                                            |> Undo.value
                                in
                                List.concat
                                    [ editorBoard.blocks
                                        |> Dict.toList
                                        |> List.map (viewBlock editorBoard model)
                                    , [ viewCursor Color.white model.cursorBounce model.editorCursor
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

                            Game ->
                                List.concat
                                    [ model.board.blocks
                                        |> Dict.toList
                                        |> List.map (viewBlock model.board model)
                                    , [ viewPlayer model.playerFacing model.playerFrame ]
                                    ]
                    }
                ]
            , viewEditorHeader model
            , case model.mode of
                Game ->
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

                Editor ->
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
                                , Html.Range.view
                                    { max = toFloat (editorBoard.maxX - 1)
                                    , min = 0
                                    , lowValue = toFloat model.xLowerVisible
                                    , highValue = toFloat model.xUpperVisible
                                    , onLowChange = round >> XLowerVisibleChanged
                                    , onHighChange = round >> XUpperVisibleChanged
                                    }
                                ]
                            , Html.label []
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
                                , Html.Range.view
                                    { max = toFloat (editorBoard.maxY - 1)
                                    , min = 0
                                    , lowValue = toFloat model.yLowerVisible
                                    , highValue = toFloat model.yUpperVisible
                                    , onLowChange = round >> YLowerVisibleChanged
                                    , onHighChange = round >> YUpperVisibleChanged
                                    }
                                ]
                            , Html.label []
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
                                , Html.Range.view
                                    { max = toFloat (editorBoard.maxZ - 1)
                                    , min = 0
                                    , lowValue = toFloat model.zLowerVisible
                                    , highValue = toFloat model.zUpperVisible
                                    , onLowChange = round >> ZLowerVisibleChanged
                                    , onHighChange = round >> ZUpperVisibleChanged
                                    }
                                ]
                            ]
                        , Html.hr [] []
                        , Html.form
                            [ Html.Events.onSubmit LoadBoard
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
                        ]
            ]
        ]
    }


viewEditorHeader : Model -> Html Msg
viewEditorHeader model =
    case model.mode of
        Game ->
            Html.text ""

        Editor ->
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
                , Html.div
                    [ Html.Attributes.attribute "role" "group" ]
                    [ Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick Undo
                        , Html.Attributes.title "Undo"
                        , Html.Attributes.Extra.aria "disabled" <|
                            Html.Attributes.Extra.bool (not <| Undo.canUndo model.editorBoard)
                        ]
                        [ Phosphor.arrowCounterClockwise Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick Redo
                        , Html.Attributes.title "Redo"
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
                        , Html.Events.onClick (SetEditMode Select)
                        , Html.Attributes.title "Select block"
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.editMode == Select)
                        ]
                        [ Phosphor.cursor Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (SetEditMode Add)
                        , Html.Attributes.title "Add block"
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.editMode == Add)
                        ]
                        [ Phosphor.plus Phosphor.Regular
                            |> Phosphor.toHtml []
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (SetEditMode Remove)
                        , Html.Attributes.title "Remove block"
                        , Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.editMode == Remove)
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
                        ]
                        [ Html.text "Wall"
                        ]
                    , Html.button
                        [ Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.selectedBlockType == Edge)
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick (BlockTypeSelected Edge)
                        ]
                        [ Html.text "Edge"
                        ]
                    , Html.button
                        [ Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.selectedBlockType == PointPickup False)
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick (BlockTypeSelected (PointPickup False))
                        ]
                        [ Html.text "Point Pickup"
                        ]
                    , Html.button
                        [ Html.Attributes.Extra.aria "current" <|
                            Html.Attributes.Extra.bool (model.selectedBlockType == PlayerSpawn { forward = PositiveX, left = PositiveY })
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick (BlockTypeSelected (PlayerSpawn { forward = PositiveX, left = PositiveY }))
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
                ]


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
        , Scene3d.coneWithShadow
            (Scene3d.Material.matte Color.green)
            (Cone3d.startingAt Point3d.origin
                Direction3d.positiveY
                { radius = Length.meters 0.5
                , length = Length.meters 0.75
                }
                |> Cone3d.placeIn frame
            )
        , Scene3d.coneWithShadow
            (Scene3d.Material.matte Color.blue)
            (Cone3d.startingAt Point3d.origin
                Direction3d.positiveZ
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
                    (Scene3d.Material.matte
                        (Color.rgb
                            (toFloat x * 1.2 / toFloat board.maxX)
                            (toFloat y * 1.2 / toFloat board.maxY)
                            (toFloat z * 1.2 / toFloat board.maxZ)
                        )
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
                if collected && model.mode == Game then
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
                case model.mode of
                    Game ->
                        Scene3d.nothing

                    Editor ->
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
                case model.mode of
                    Game ->
                        Scene3d.nothing

                    Editor ->
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
