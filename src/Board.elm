module Board exposing
    ( Axis(..)
    , Block(..)
    , BlockPalette(..)
    , Board
    , BoardLoadError(..)
    , BoardPlayError(..)
    , ExampleBoards(..)
    , Facing(..)
    , Point
    , ScreenCoordinates(..)
    , WorldCoordinates(..)
    , axisToDirection3d
    , axisToLabel
    , basicMiniBoard
    , boardCodec
    , defaultBoard
    , empty
    , findSpawn
    , gameLights
    , gamePlayCamera
    , indexToPoint
    , movePlayer
    , point3dToPoint
    , pointToPoint3d
    , setPlayerFacing
    , view3dScene
    , viewBlock
    , viewPlayer
    , zigZagBoard
    )

import Angle exposing (Angle)
import Axis3d
import Block3d
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Illuminance
import Length
import Luminance
import Pixels
import Point3d exposing (Point3d)
import Quantity
import Scene3d
import Scene3d.Light
import Scene3d.Material
import Scene3d.Mesh
import Serialize
import SketchPlane3d
import Sphere3d
import Vector3d
import Viewpoint3d


type WorldCoordinates
    = WorldCoordinates Never


type ScreenCoordinates
    = ScreenCoordinates Never


type alias Point =
    ( Int, Int, Int )


type alias Board =
    { maxX : Int
    , maxY : Int
    , maxZ : Int
    , blocks : Dict Point Block
    }


empty : Board
empty =
    { maxX = 0
    , maxY = 0
    , maxZ = 0
    , blocks = Dict.empty
    }


type Block
    = Empty
    | Wall
    | Edge
    | PointPickup Bool
    | PlayerSpawn { forward : Axis, left : Axis }


pointCodec : Serialize.Codec e Point
pointCodec =
    Serialize.triple Serialize.int Serialize.int Serialize.int


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
        |> Serialize.field .forward axisCodec
        |> Serialize.field .left axisCodec
        |> Serialize.finishRecord


axisCodec : Serialize.Codec e Axis
axisCodec =
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


type Facing
    = Forward
    | Backward
    | Left
    | Right


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


type BlockPalette
    = SimpleBlocks
    | RainbowBlocks


gameLights : Scene3d.Lights WorldCoordinates
gameLights =
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
                            (Angle.degrees 90)
                , chromaticity = Scene3d.Light.skylight
                , intensity = Illuminance.lux 40000
                }

        sky3 =
            Scene3d.Light.overhead
                { upDirection =
                    Direction3d.positiveZ
                        |> Direction3d.rotateAround Axis3d.x (Angle.degrees -70)
                        |> Direction3d.rotateAround Axis3d.z
                            (Angle.degrees -90)
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


gamePlayCamera : Frame3d Length.Meters WorldCoordinates { defines : WorldCoordinates } -> Camera3d Length.Meters WorldCoordinates
gamePlayCamera playerFrame =
    Camera3d.perspective
        { viewpoint =
            let
                targetPos =
                    Frame3d.originPoint playerFrame
            in
            Viewpoint3d.lookAt
                { focalPoint = targetPos
                , eyePoint =
                    targetPos
                        |> Point3d.translateIn (Frame3d.zDirection playerFrame) (Length.meters 15)
                , upDirection = Frame3d.xDirection playerFrame
                }
        , verticalFieldOfView = Angle.degrees 30
        }


view3dScene : Scene3d.Lights WorldCoordinates -> { width : Int, height : Int } -> Camera3d Length.Meters WorldCoordinates -> List (Scene3d.Entity WorldCoordinates) -> Html msg
view3dScene lights screenSize camera entities =
    Scene3d.custom
        { clipDepth = Length.meters 1
        , background = Scene3d.backgroundColor Color.gray
        , exposure = Scene3d.exposureValue 15
        , lights = lights
        , toneMapping = Scene3d.noToneMapping
        , whiteBalance = Scene3d.Light.daylight
        , antialiasing = Scene3d.multisampling
        , dimensions = ( Pixels.int screenSize.width, Pixels.int screenSize.height )
        , camera = camera
        , entities = entities
        }


viewPlayer : ( Scene3d.Mesh.Unlit WorldCoordinates, Scene3d.Material.Texture Color ) -> Facing -> Frame3d Length.Meters WorldCoordinates { defines : WorldCoordinates } -> Scene3d.Entity WorldCoordinates
viewPlayer ( playerMesh, playerTexture ) facing frame =
    let
        point =
            Frame3d.originPoint frame
    in
    -- Scene3d.group
    --     [ Scene3d.sphereWithShadow
    --         (Scene3d.Material.matte Color.gray)
    --         (Sphere3d.atPoint Point3d.origin
    --             (Length.meters 0.5)
    --             |> Sphere3d.placeIn frame
    --         )
    --     , Scene3d.coneWithShadow
    --         (Scene3d.Material.matte Color.red)
    --         (Cone3d.startingAt Point3d.origin
    --             Direction3d.positiveX
    --             { radius = Length.meters 0.5
    --             , length = Length.meters 0.75
    --             }
    --             |> Cone3d.placeIn frame
    --         )
    --     ]
    Scene3d.meshWithShadow
        (Scene3d.Material.color Color.gray)
        playerMesh
        (Scene3d.Mesh.shadow playerMesh)
        |> Scene3d.scaleAbout Point3d.origin 0.3
        |> Scene3d.translateBy (Vector3d.from Point3d.origin (Frame3d.originPoint frame))
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


viewBlock : ( Point, Block ) -> Scene3d.Entity WorldCoordinates
viewBlock ( point, block ) =
    let
        ( x, y, z ) =
            point
    in
    case block of
        Wall ->
            Scene3d.blockWithShadow
                (Scene3d.Material.matte <|
                    Color.gray
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
            if collected then
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

        PlayerSpawn _ ->
            Scene3d.nothing

        Empty ->
            Scene3d.nothing

        Edge ->
            Scene3d.nothing


setPlayerFacing :
    { m
        | playerFacing : Facing
        , playerWantFacing : Facing
        , playerMovingAcrossEdge : Maybe Angle
        , playerFrame : Frame3d Length.Meters WorldCoordinates { defines : WorldCoordinates }
        , board : Board
    }
    ->
        { m
            | playerFacing : Facing
            , playerWantFacing : Facing
            , playerMovingAcrossEdge : Maybe Angle
            , playerFrame : Frame3d Length.Meters WorldCoordinates { defines : WorldCoordinates }
            , board : Board
        }
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


movePlayer :
    Float
    ->
        { m
            | playerFacing : Facing
            , playerMovingAcrossEdge : Maybe Angle
            , playerFrame : Frame3d Length.Meters WorldCoordinates { defines : WorldCoordinates }
            , board : Board
            , score : Int
        }
    ->
        { m
            | playerFacing : Facing
            , playerMovingAcrossEdge : Maybe Angle
            , playerFrame : Frame3d Length.Meters WorldCoordinates { defines : WorldCoordinates }
            , board : Board
            , score : Int
        }
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


findSpawn : Board -> Maybe (Frame3d Length.Meters WorldCoordinates { defines : WorldCoordinates })
findSpawn board =
    findSpawnHelper (Dict.toList board.blocks)


findSpawnHelper : List ( Point, Block ) -> Maybe (Frame3d Length.Meters WorldCoordinates { defines : WorldCoordinates })
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


type BoardPlayError
    = MissingPlayerSpawn


type BoardLoadError
    = DataCorrupted
    | SerializerOutOfDate
    | OtherError



-- SAMPLE LEVELS


type ExampleBoards
    = DefaultBoard
    | BasicMiniBoard
    | ZigZagBoard


defaultBoard : String
defaultBoard =
    "[1,[9,9,9,[[[0,0,0],[1]],[[0,0,1],[1]],[[0,0,2],[1]],[[0,0,3],[1]],[[0,0,4],[1]],[[0,0,5],[1]],[[0,0,6],[1]],[[0,0,7],[1]],[[0,0,8],[1]],[[0,1,0],[1]],[[0,1,1],[1]],[[0,1,2],[1]],[[0,1,3],[1]],[[0,1,4],[1]],[[0,1,5],[1]],[[0,1,6],[1]],[[0,1,7],[1]],[[0,1,8],[1]],[[0,2,0],[1]],[[0,2,1],[1]],[[0,2,2],[1]],[[0,2,3],[1]],[[0,2,4],[1]],[[0,2,5],[1]],[[0,2,6],[1]],[[0,2,7],[1]],[[0,2,8],[1]],[[0,3,0],[1]],[[0,3,1],[1]],[[0,3,2],[1]],[[0,3,3],[1]],[[0,3,4],[1]],[[0,3,5],[1]],[[0,3,6],[1]],[[0,3,7],[1]],[[0,3,8],[1]],[[0,4,0],[1]],[[0,4,1],[1]],[[0,4,2],[1]],[[0,4,3],[1]],[[0,4,4],[1]],[[0,4,5],[1]],[[0,4,6],[1]],[[0,4,7],[1]],[[0,4,8],[1]],[[0,5,0],[1]],[[0,5,1],[1]],[[0,5,2],[1]],[[0,5,3],[1]],[[0,5,4],[1]],[[0,5,5],[1]],[[0,5,6],[1]],[[0,5,7],[1]],[[0,5,8],[1]],[[0,6,0],[1]],[[0,6,1],[1]],[[0,6,2],[1]],[[0,6,3],[1]],[[0,6,4],[1]],[[0,6,5],[1]],[[0,6,6],[1]],[[0,6,7],[1]],[[0,6,8],[1]],[[0,7,0],[1]],[[0,7,1],[1]],[[0,7,2],[1]],[[0,7,3],[1]],[[0,7,4],[1]],[[0,7,5],[1]],[[0,7,6],[1]],[[0,7,7],[1]],[[0,7,8],[1]],[[0,8,0],[1]],[[0,8,1],[1]],[[0,8,2],[1]],[[0,8,3],[1]],[[0,8,4],[1]],[[0,8,5],[1]],[[0,8,6],[1]],[[0,8,7],[1]],[[0,8,8],[1]],[[1,0,0],[1]],[[1,0,1],[1]],[[1,0,2],[1]],[[1,0,3],[1]],[[1,0,4],[1]],[[1,0,5],[1]],[[1,0,6],[1]],[[1,0,7],[1]],[[1,0,8],[1]],[[1,1,0],[1]],[[1,1,1],[1]],[[1,1,2],[1]],[[1,1,3],[1]],[[1,1,4],[1]],[[1,1,5],[1]],[[1,1,6],[1]],[[1,1,7],[1]],[[1,1,8],[1]],[[1,2,0],[1]],[[1,2,1],[1]],[[1,2,2],[1]],[[1,2,3],[1]],[[1,2,4],[1]],[[1,2,5],[1]],[[1,2,6],[1]],[[1,2,7],[1]],[[1,2,8],[1]],[[1,3,0],[1]],[[1,3,1],[1]],[[1,3,2],[1]],[[1,3,3],[1]],[[1,3,4],[1]],[[1,3,5],[1]],[[1,3,6],[1]],[[1,3,7],[1]],[[1,3,8],[1]],[[1,4,0],[1]],[[1,4,1],[1]],[[1,4,2],[1]],[[1,4,3],[1]],[[1,4,4],[1]],[[1,4,5],[1]],[[1,4,6],[1]],[[1,4,7],[1]],[[1,4,8],[1]],[[1,5,0],[1]],[[1,5,1],[1]],[[1,5,2],[1]],[[1,5,3],[1]],[[1,5,4],[1]],[[1,5,5],[1]],[[1,5,6],[1]],[[1,5,7],[1]],[[1,5,8],[1]],[[1,6,0],[1]],[[1,6,1],[1]],[[1,6,2],[1]],[[1,6,3],[1]],[[1,6,4],[1]],[[1,6,5],[1]],[[1,6,6],[1]],[[1,6,7],[1]],[[1,6,8],[1]],[[1,7,0],[1]],[[1,7,1],[1]],[[1,7,2],[1]],[[1,7,3],[1]],[[1,7,4],[1]],[[1,7,5],[1]],[[1,7,6],[1]],[[1,7,7],[1]],[[1,7,8],[1]],[[1,8,0],[1]],[[1,8,1],[1]],[[1,8,2],[1]],[[1,8,3],[1]],[[1,8,4],[1]],[[1,8,5],[1]],[[1,8,6],[1]],[[1,8,7],[1]],[[1,8,8],[1]],[[2,0,0],[1]],[[2,0,1],[1]],[[2,0,2],[1]],[[2,0,3],[1]],[[2,0,4],[1]],[[2,0,5],[1]],[[2,0,6],[1]],[[2,0,7],[1]],[[2,0,8],[1]],[[2,1,0],[1]],[[2,1,1],[1]],[[2,1,2],[1]],[[2,1,3],[1]],[[2,1,4],[1]],[[2,1,5],[1]],[[2,1,6],[1]],[[2,1,7],[1]],[[2,1,8],[1]],[[2,2,0],[1]],[[2,2,1],[1]],[[2,2,2],[1]],[[2,2,3],[1]],[[2,2,4],[1]],[[2,2,5],[1]],[[2,2,6],[1]],[[2,2,7],[1]],[[2,2,8],[1]],[[2,3,0],[1]],[[2,3,1],[1]],[[2,3,2],[1]],[[2,3,3],[1]],[[2,3,4],[1]],[[2,3,5],[1]],[[2,3,6],[1]],[[2,3,7],[1]],[[2,3,8],[1]],[[2,4,0],[1]],[[2,4,1],[1]],[[2,4,2],[1]],[[2,4,3],[1]],[[2,4,4],[1]],[[2,4,5],[1]],[[2,4,6],[1]],[[2,4,7],[1]],[[2,4,8],[1]],[[2,5,0],[1]],[[2,5,1],[1]],[[2,5,2],[1]],[[2,5,3],[1]],[[2,5,4],[1]],[[2,5,5],[1]],[[2,5,6],[1]],[[2,5,7],[1]],[[2,5,8],[1]],[[2,6,0],[1]],[[2,6,1],[1]],[[2,6,2],[1]],[[2,6,3],[1]],[[2,6,4],[1]],[[2,6,5],[1]],[[2,6,6],[1]],[[2,6,7],[1]],[[2,6,8],[1]],[[2,7,0],[1]],[[2,7,1],[1]],[[2,7,2],[1]],[[2,7,3],[1]],[[2,7,4],[1]],[[2,7,5],[1]],[[2,7,6],[1]],[[2,7,7],[1]],[[2,7,8],[1]],[[2,8,0],[1]],[[2,8,1],[1]],[[2,8,2],[1]],[[2,8,3],[1]],[[2,8,4],[1]],[[2,8,5],[1]],[[2,8,6],[1]],[[2,8,7],[1]],[[2,8,8],[1]],[[3,0,0],[1]],[[3,0,1],[1]],[[3,0,2],[1]],[[3,0,3],[1]],[[3,0,4],[1]],[[3,0,5],[1]],[[3,0,6],[1]],[[3,0,7],[1]],[[3,0,8],[1]],[[3,1,0],[1]],[[3,1,1],[1]],[[3,1,2],[1]],[[3,1,3],[1]],[[3,1,4],[1]],[[3,1,5],[1]],[[3,1,6],[1]],[[3,1,7],[1]],[[3,1,8],[1]],[[3,2,0],[1]],[[3,2,1],[1]],[[3,2,2],[1]],[[3,2,3],[1]],[[3,2,4],[1]],[[3,2,5],[1]],[[3,2,6],[1]],[[3,2,7],[1]],[[3,2,8],[1]],[[3,3,0],[1]],[[3,3,1],[1]],[[3,3,2],[1]],[[3,3,3],[1]],[[3,3,4],[1]],[[3,3,5],[1]],[[3,3,6],[1]],[[3,3,7],[1]],[[3,3,8],[1]],[[3,4,0],[1]],[[3,4,1],[1]],[[3,4,2],[1]],[[3,4,3],[1]],[[3,4,4],[1]],[[3,4,5],[1]],[[3,4,6],[1]],[[3,4,7],[1]],[[3,4,8],[1]],[[3,5,0],[1]],[[3,5,1],[1]],[[3,5,2],[1]],[[3,5,3],[1]],[[3,5,4],[1]],[[3,5,5],[1]],[[3,5,6],[1]],[[3,5,7],[1]],[[3,5,8],[1]],[[3,6,0],[1]],[[3,6,1],[1]],[[3,6,2],[1]],[[3,6,3],[1]],[[3,6,4],[1]],[[3,6,5],[1]],[[3,6,6],[1]],[[3,6,7],[1]],[[3,6,8],[1]],[[3,7,0],[1]],[[3,7,1],[1]],[[3,7,2],[1]],[[3,7,3],[1]],[[3,7,4],[1]],[[3,7,5],[1]],[[3,7,6],[1]],[[3,7,7],[1]],[[3,7,8],[1]],[[3,8,0],[1]],[[3,8,1],[1]],[[3,8,2],[1]],[[3,8,3],[1]],[[3,8,4],[1]],[[3,8,5],[1]],[[3,8,6],[1]],[[3,8,7],[1]],[[3,8,8],[1]],[[4,0,0],[1]],[[4,0,1],[1]],[[4,0,2],[1]],[[4,0,3],[1]],[[4,0,4],[1]],[[4,0,5],[1]],[[4,0,6],[1]],[[4,0,7],[1]],[[4,0,8],[1]],[[4,1,0],[1]],[[4,1,1],[1]],[[4,1,2],[1]],[[4,1,3],[1]],[[4,1,4],[1]],[[4,1,5],[1]],[[4,1,6],[1]],[[4,1,7],[1]],[[4,1,8],[1]],[[4,2,0],[1]],[[4,2,1],[1]],[[4,2,2],[1]],[[4,2,3],[1]],[[4,2,4],[1]],[[4,2,5],[1]],[[4,2,6],[1]],[[4,2,7],[1]],[[4,2,8],[1]],[[4,3,0],[1]],[[4,3,1],[1]],[[4,3,2],[1]],[[4,3,3],[1]],[[4,3,4],[1]],[[4,3,5],[1]],[[4,3,6],[1]],[[4,3,7],[1]],[[4,3,8],[1]],[[4,4,0],[1]],[[4,4,1],[1]],[[4,4,2],[1]],[[4,4,3],[1]],[[4,4,4],[1]],[[4,4,5],[1]],[[4,4,6],[1]],[[4,4,7],[1]],[[4,4,8],[1]],[[4,5,0],[1]],[[4,5,1],[1]],[[4,5,2],[1]],[[4,5,3],[1]],[[4,5,4],[1]],[[4,5,5],[1]],[[4,5,6],[1]],[[4,5,7],[1]],[[4,5,8],[1]],[[4,6,0],[1]],[[4,6,1],[1]],[[4,6,2],[1]],[[4,6,3],[1]],[[4,6,4],[1]],[[4,6,5],[1]],[[4,6,6],[1]],[[4,6,7],[1]],[[4,6,8],[1]],[[4,7,0],[1]],[[4,7,1],[1]],[[4,7,2],[1]],[[4,7,3],[1]],[[4,7,4],[1]],[[4,7,5],[1]],[[4,7,6],[1]],[[4,7,7],[1]],[[4,7,8],[1]],[[4,8,0],[1]],[[4,8,1],[1]],[[4,8,2],[1]],[[4,8,3],[1]],[[4,8,4],[1]],[[4,8,5],[1]],[[4,8,6],[1]],[[4,8,7],[1]],[[4,8,8],[1]],[[5,0,0],[1]],[[5,0,1],[1]],[[5,0,2],[1]],[[5,0,3],[1]],[[5,0,4],[1]],[[5,0,5],[1]],[[5,0,6],[1]],[[5,0,7],[1]],[[5,0,8],[1]],[[5,1,0],[1]],[[5,1,1],[1]],[[5,1,2],[1]],[[5,1,3],[1]],[[5,1,4],[1]],[[5,1,5],[1]],[[5,1,6],[1]],[[5,1,7],[1]],[[5,1,8],[1]],[[5,2,0],[1]],[[5,2,1],[1]],[[5,2,2],[1]],[[5,2,3],[1]],[[5,2,4],[1]],[[5,2,5],[1]],[[5,2,6],[1]],[[5,2,7],[1]],[[5,2,8],[1]],[[5,3,0],[1]],[[5,3,1],[1]],[[5,3,2],[1]],[[5,3,3],[1]],[[5,3,4],[1]],[[5,3,5],[1]],[[5,3,6],[1]],[[5,3,7],[1]],[[5,3,8],[1]],[[5,4,0],[1]],[[5,4,1],[1]],[[5,4,2],[1]],[[5,4,3],[1]],[[5,4,4],[1]],[[5,4,5],[1]],[[5,4,6],[1]],[[5,4,7],[1]],[[5,4,8],[1]],[[5,5,0],[1]],[[5,5,1],[1]],[[5,5,2],[1]],[[5,5,3],[1]],[[5,5,4],[1]],[[5,5,5],[1]],[[5,5,6],[1]],[[5,5,7],[1]],[[5,5,8],[1]],[[5,6,0],[1]],[[5,6,1],[1]],[[5,6,2],[1]],[[5,6,3],[1]],[[5,6,4],[1]],[[5,6,5],[1]],[[5,6,6],[1]],[[5,6,7],[1]],[[5,6,8],[1]],[[5,7,0],[1]],[[5,7,1],[1]],[[5,7,2],[1]],[[5,7,3],[1]],[[5,7,4],[1]],[[5,7,5],[1]],[[5,7,6],[1]],[[5,7,7],[1]],[[5,7,8],[1]],[[5,8,0],[1]],[[5,8,1],[1]],[[5,8,2],[1]],[[5,8,3],[1]],[[5,8,4],[1]],[[5,8,5],[1]],[[5,8,6],[1]],[[5,8,7],[1]],[[5,8,8],[1]],[[6,0,0],[1]],[[6,0,1],[1]],[[6,0,2],[1]],[[6,0,3],[1]],[[6,0,4],[1]],[[6,0,5],[1]],[[6,0,6],[1]],[[6,0,7],[1]],[[6,0,8],[1]],[[6,1,0],[1]],[[6,1,1],[1]],[[6,1,2],[1]],[[6,1,3],[1]],[[6,1,4],[1]],[[6,1,5],[1]],[[6,1,6],[1]],[[6,1,7],[1]],[[6,1,8],[1]],[[6,2,0],[1]],[[6,2,1],[1]],[[6,2,2],[1]],[[6,2,3],[1]],[[6,2,4],[1]],[[6,2,5],[1]],[[6,2,6],[1]],[[6,2,7],[1]],[[6,2,8],[1]],[[6,3,0],[1]],[[6,3,1],[1]],[[6,3,2],[1]],[[6,3,3],[1]],[[6,3,4],[1]],[[6,3,5],[1]],[[6,3,6],[1]],[[6,3,7],[1]],[[6,3,8],[1]],[[6,4,0],[1]],[[6,4,1],[1]],[[6,4,2],[1]],[[6,4,3],[1]],[[6,4,4],[1]],[[6,4,5],[1]],[[6,4,6],[1]],[[6,4,7],[1]],[[6,4,8],[1]],[[6,5,0],[1]],[[6,5,1],[1]],[[6,5,2],[1]],[[6,5,3],[1]],[[6,5,4],[1]],[[6,5,5],[1]],[[6,5,6],[1]],[[6,5,7],[1]],[[6,5,8],[1]],[[6,6,0],[1]],[[6,6,1],[1]],[[6,6,2],[1]],[[6,6,3],[1]],[[6,6,4],[1]],[[6,6,5],[1]],[[6,6,6],[1]],[[6,6,7],[1]],[[6,6,8],[1]],[[6,7,0],[1]],[[6,7,1],[1]],[[6,7,2],[1]],[[6,7,3],[1]],[[6,7,4],[1]],[[6,7,5],[1]],[[6,7,6],[1]],[[6,7,7],[1]],[[6,7,8],[1]],[[6,8,0],[1]],[[6,8,1],[1]],[[6,8,2],[1]],[[6,8,3],[1]],[[6,8,4],[1]],[[6,8,5],[1]],[[6,8,6],[1]],[[6,8,7],[1]],[[6,8,8],[1]],[[7,0,0],[1]],[[7,0,1],[1]],[[7,0,2],[1]],[[7,0,3],[1]],[[7,0,4],[1]],[[7,0,5],[1]],[[7,0,6],[1]],[[7,0,7],[1]],[[7,0,8],[1]],[[7,1,0],[1]],[[7,1,1],[1]],[[7,1,2],[1]],[[7,1,3],[1]],[[7,1,4],[1]],[[7,1,5],[1]],[[7,1,6],[1]],[[7,1,7],[1]],[[7,1,8],[1]],[[7,2,0],[1]],[[7,2,1],[1]],[[7,2,2],[1]],[[7,2,3],[1]],[[7,2,4],[1]],[[7,2,5],[1]],[[7,2,6],[1]],[[7,2,7],[1]],[[7,2,8],[1]],[[7,3,0],[1]],[[7,3,1],[1]],[[7,3,2],[1]],[[7,3,3],[1]],[[7,3,4],[1]],[[7,3,5],[1]],[[7,3,6],[1]],[[7,3,7],[1]],[[7,3,8],[1]],[[7,4,0],[1]],[[7,4,1],[1]],[[7,4,2],[1]],[[7,4,3],[1]],[[7,4,4],[1]],[[7,4,5],[1]],[[7,4,6],[1]],[[7,4,7],[1]],[[7,4,8],[1]],[[7,5,0],[1]],[[7,5,1],[1]],[[7,5,2],[1]],[[7,5,3],[1]],[[7,5,4],[1]],[[7,5,5],[1]],[[7,5,6],[1]],[[7,5,7],[1]],[[7,5,8],[1]],[[7,6,0],[1]],[[7,6,1],[1]],[[7,6,2],[1]],[[7,6,3],[1]],[[7,6,4],[1]],[[7,6,5],[1]],[[7,6,6],[1]],[[7,6,7],[1]],[[7,6,8],[1]],[[7,7,0],[1]],[[7,7,1],[1]],[[7,7,2],[1]],[[7,7,3],[1]],[[7,7,4],[1]],[[7,7,5],[1]],[[7,7,6],[1]],[[7,7,7],[1]],[[7,7,8],[1]],[[7,8,0],[1]],[[7,8,1],[1]],[[7,8,2],[1]],[[7,8,3],[1]],[[7,8,4],[1]],[[7,8,5],[1]],[[7,8,6],[1]],[[7,8,7],[1]],[[7,8,8],[1]],[[8,0,0],[1]],[[8,0,1],[1]],[[8,0,2],[1]],[[8,0,3],[1]],[[8,0,4],[1]],[[8,0,5],[1]],[[8,0,6],[1]],[[8,0,7],[1]],[[8,0,8],[1]],[[8,1,0],[1]],[[8,1,1],[1]],[[8,1,2],[1]],[[8,1,3],[1]],[[8,1,4],[1]],[[8,1,5],[1]],[[8,1,6],[1]],[[8,1,7],[1]],[[8,1,8],[1]],[[8,2,0],[1]],[[8,2,1],[1]],[[8,2,2],[1]],[[8,2,3],[1]],[[8,2,4],[1]],[[8,2,5],[1]],[[8,2,6],[1]],[[8,2,7],[1]],[[8,2,8],[1]],[[8,3,0],[1]],[[8,3,1],[1]],[[8,3,2],[1]],[[8,3,3],[1]],[[8,3,4],[1]],[[8,3,5],[1]],[[8,3,6],[1]],[[8,3,7],[1]],[[8,3,8],[1]],[[8,4,0],[1]],[[8,4,1],[1]],[[8,4,2],[1]],[[8,4,3],[1]],[[8,4,4],[1]],[[8,4,5],[1]],[[8,4,6],[1]],[[8,4,7],[1]],[[8,4,8],[1]],[[8,5,0],[1]],[[8,5,1],[1]],[[8,5,2],[1]],[[8,5,3],[1]],[[8,5,4],[1]],[[8,5,5],[1]],[[8,5,6],[1]],[[8,5,7],[1]],[[8,5,8],[1]],[[8,6,0],[1]],[[8,6,1],[1]],[[8,6,2],[1]],[[8,6,3],[1]],[[8,6,4],[1]],[[8,6,5],[1]],[[8,6,6],[1]],[[8,6,7],[1]],[[8,6,8],[1]],[[8,7,0],[1]],[[8,7,1],[1]],[[8,7,2],[1]],[[8,7,3],[1]],[[8,7,4],[1]],[[8,7,5],[1]],[[8,7,6],[1]],[[8,7,7],[1]],[[8,7,8],[1]],[[8,8,0],[1]],[[8,8,1],[1]],[[8,8,2],[1]],[[8,8,3],[1]],[[8,8,4],[1]],[[8,8,5],[1]],[[8,8,6],[1]],[[8,8,7],[1]],[[8,8,8],[1]]]]]"


basicMiniBoard : String
basicMiniBoard =
    "[1,[5,5,5,[[[0,0,0],[1]],[[0,0,1],[1]],[[0,0,2],[2]],[[0,0,3],[1]],[[0,0,4],[1]],[[0,1,0],[1]],[[0,1,1],[1]],[[0,1,2],[3,false]],[[0,1,3],[1]],[[0,1,4],[1]],[[0,2,0],[2]],[[0,2,1],[3,false]],[[0,2,2],[3,false]],[[0,2,3],[3,false]],[[0,2,4],[2]],[[0,3,0],[1]],[[0,3,1],[1]],[[0,3,2],[3,false]],[[0,3,3],[1]],[[0,3,4],[1]],[[0,4,0],[1]],[[0,4,1],[1]],[[0,4,2],[2]],[[0,4,3],[1]],[[0,4,4],[1]],[[1,0,0],[1]],[[1,0,1],[1]],[[1,0,2],[3,false]],[[1,0,3],[1]],[[1,0,4],[1]],[[1,1,0],[1]],[[1,1,1],[1]],[[1,1,2],[1]],[[1,1,3],[1]],[[1,1,4],[1]],[[1,2,0],[3,false]],[[1,2,1],[1]],[[1,2,2],[1]],[[1,2,3],[1]],[[1,2,4],[3,false]],[[1,3,0],[1]],[[1,3,1],[1]],[[1,3,2],[1]],[[1,3,3],[1]],[[1,3,4],[1]],[[1,4,0],[1]],[[1,4,1],[1]],[[1,4,2],[3,false]],[[1,4,3],[1]],[[1,4,4],[1]],[[2,0,0],[2]],[[2,0,1],[3,false]],[[2,0,2],[3,false]],[[2,0,3],[3,false]],[[2,0,4],[2]],[[2,1,0],[3,false]],[[2,1,1],[1]],[[2,1,2],[1]],[[2,1,3],[1]],[[2,1,4],[3,false]],[[2,2,0],[3,false]],[[2,2,1],[1]],[[2,2,2],[1]],[[2,2,3],[1]],[[2,2,4],[4,[[0],[2]]]],[[2,3,0],[3,false]],[[2,3,1],[1]],[[2,3,2],[1]],[[2,3,3],[1]],[[2,3,4],[3,false]],[[2,4,0],[2]],[[2,4,1],[3,false]],[[2,4,2],[3,false]],[[2,4,3],[3,false]],[[2,4,4],[2]],[[3,0,0],[1]],[[3,0,1],[1]],[[3,0,2],[3,false]],[[3,0,3],[1]],[[3,0,4],[1]],[[3,1,0],[1]],[[3,1,1],[1]],[[3,1,2],[1]],[[3,1,3],[1]],[[3,1,4],[1]],[[3,2,0],[3,false]],[[3,2,1],[1]],[[3,2,2],[1]],[[3,2,3],[1]],[[3,2,4],[3,false]],[[3,3,0],[1]],[[3,3,1],[1]],[[3,3,2],[1]],[[3,3,3],[1]],[[3,3,4],[1]],[[3,4,0],[1]],[[3,4,1],[1]],[[3,4,2],[3,false]],[[3,4,3],[1]],[[3,4,4],[1]],[[4,0,0],[1]],[[4,0,1],[1]],[[4,0,2],[2]],[[4,0,3],[1]],[[4,0,4],[1]],[[4,1,0],[1]],[[4,1,1],[1]],[[4,1,2],[3,false]],[[4,1,3],[1]],[[4,1,4],[1]],[[4,2,0],[2]],[[4,2,1],[3,false]],[[4,2,2],[3,false]],[[4,2,3],[3,false]],[[4,2,4],[2]],[[4,3,0],[1]],[[4,3,1],[1]],[[4,3,2],[3,false]],[[4,3,3],[1]],[[4,3,4],[1]],[[4,4,0],[1]],[[4,4,1],[1]],[[4,4,2],[2]],[[4,4,3],[1]],[[4,4,4],[1]]]]]"


zigZagBoard : String
zigZagBoard =
    "[1,[9,9,9,[[[0,0,0],[1]],[[0,0,1],[1]],[[0,0,2],[1]],[[0,0,3],[1]],[[0,0,4],[1]],[[0,0,5],[1]],[[0,0,6],[1]],[[0,0,7],[1]],[[0,0,8],[1]],[[0,1,0],[2]],[[0,1,1],[0]],[[0,1,2],[3,false]],[[0,1,3],[1]],[[0,1,4],[1]],[[0,1,5],[1]],[[0,1,6],[3,false]],[[0,1,7],[0]],[[0,1,8],[2]],[[0,2,0],[1]],[[0,2,1],[1]],[[0,2,2],[3,false]],[[0,2,3],[1]],[[0,2,4],[1]],[[0,2,5],[1]],[[0,2,6],[3,false]],[[0,2,7],[1]],[[0,2,8],[1]],[[0,3,0],[1]],[[0,3,1],[1]],[[0,3,2],[3,false]],[[0,3,3],[1]],[[0,3,4],[1]],[[0,3,5],[1]],[[0,3,6],[3,false]],[[0,3,7],[1]],[[0,3,8],[1]],[[0,4,0],[1]],[[0,4,1],[1]],[[0,4,2],[3,false]],[[0,4,3],[3,false]],[[0,4,4],[3,false]],[[0,4,5],[3,false]],[[0,4,6],[3,false]],[[0,4,7],[1]],[[0,4,8],[1]],[[0,5,0],[1]],[[0,5,1],[1]],[[0,5,2],[3,false]],[[0,5,3],[1]],[[0,5,4],[1]],[[0,5,5],[1]],[[0,5,6],[3,false]],[[0,5,7],[1]],[[0,5,8],[1]],[[0,6,0],[1]],[[0,6,1],[1]],[[0,6,2],[3,false]],[[0,6,3],[1]],[[0,6,4],[1]],[[0,6,5],[1]],[[0,6,6],[3,false]],[[0,6,7],[1]],[[0,6,8],[1]],[[0,7,0],[2]],[[0,7,1],[0]],[[0,7,2],[3,false]],[[0,7,3],[1]],[[0,7,4],[1]],[[0,7,5],[1]],[[0,7,6],[3,false]],[[0,7,7],[0]],[[0,7,8],[2]],[[0,8,0],[1]],[[0,8,1],[1]],[[0,8,2],[1]],[[0,8,3],[1]],[[0,8,4],[1]],[[0,8,5],[1]],[[0,8,6],[1]],[[0,8,7],[1]],[[0,8,8],[1]],[[1,0,0],[2]],[[1,0,1],[0]],[[1,0,2],[3,false]],[[1,0,3],[1]],[[1,0,4],[1]],[[1,0,5],[1]],[[1,0,6],[3,false]],[[1,0,7],[0]],[[1,0,8],[2]],[[1,1,0],[0]],[[1,1,1],[1]],[[1,1,2],[1]],[[1,1,3],[1]],[[1,1,4],[1]],[[1,1,5],[1]],[[1,1,6],[1]],[[1,1,7],[1]],[[1,1,8],[0]],[[1,2,0],[3,false]],[[1,2,1],[1]],[[1,2,2],[1]],[[1,2,3],[1]],[[1,2,4],[1]],[[1,2,5],[1]],[[1,2,6],[1]],[[1,2,7],[1]],[[1,2,8],[1]],[[1,3,0],[3,false]],[[1,3,1],[1]],[[1,3,2],[1]],[[1,3,3],[1]],[[1,3,4],[1]],[[1,3,5],[1]],[[1,3,6],[1]],[[1,3,7],[1]],[[1,3,8],[1]],[[1,4,0],[3,false]],[[1,4,1],[1]],[[1,4,2],[1]],[[1,4,3],[1]],[[1,4,4],[1]],[[1,4,5],[1]],[[1,4,6],[1]],[[1,4,7],[1]],[[1,4,8],[1]],[[1,5,0],[3,false]],[[1,5,1],[1]],[[1,5,2],[1]],[[1,5,3],[1]],[[1,5,4],[1]],[[1,5,5],[1]],[[1,5,6],[1]],[[1,5,7],[1]],[[1,5,8],[1]],[[1,6,0],[3,false]],[[1,6,1],[1]],[[1,6,2],[1]],[[1,6,3],[1]],[[1,6,4],[1]],[[1,6,5],[1]],[[1,6,6],[1]],[[1,6,7],[1]],[[1,6,8],[1]],[[1,7,0],[0]],[[1,7,1],[1]],[[1,7,2],[1]],[[1,7,3],[1]],[[1,7,4],[1]],[[1,7,5],[1]],[[1,7,6],[1]],[[1,7,7],[1]],[[1,7,8],[0]],[[1,8,0],[2]],[[1,8,1],[0]],[[1,8,2],[3,false]],[[1,8,3],[1]],[[1,8,4],[1]],[[1,8,5],[1]],[[1,8,6],[3,false]],[[1,8,7],[0]],[[1,8,8],[2]],[[2,0,0],[1]],[[2,0,1],[1]],[[2,0,2],[3,false]],[[2,0,3],[1]],[[2,0,4],[1]],[[2,0,5],[1]],[[2,0,6],[3,false]],[[2,0,7],[1]],[[2,0,8],[1]],[[2,1,0],[1]],[[2,1,1],[1]],[[2,1,2],[1]],[[2,1,3],[1]],[[2,1,4],[1]],[[2,1,5],[1]],[[2,1,6],[1]],[[2,1,7],[1]],[[2,1,8],[3,false]],[[2,2,0],[1]],[[2,2,1],[1]],[[2,2,2],[1]],[[2,2,3],[1]],[[2,2,4],[1]],[[2,2,5],[1]],[[2,2,6],[1]],[[2,2,7],[1]],[[2,2,8],[1]],[[2,3,0],[1]],[[2,3,1],[1]],[[2,3,2],[1]],[[2,3,3],[1]],[[2,3,4],[1]],[[2,3,5],[1]],[[2,3,6],[1]],[[2,3,7],[1]],[[2,3,8],[1]],[[2,4,0],[3,false]],[[2,4,1],[1]],[[2,4,2],[1]],[[2,4,3],[1]],[[2,4,4],[1]],[[2,4,5],[1]],[[2,4,6],[1]],[[2,4,7],[1]],[[2,4,8],[1]],[[2,5,0],[1]],[[2,5,1],[1]],[[2,5,2],[1]],[[2,5,3],[1]],[[2,5,4],[1]],[[2,5,5],[1]],[[2,5,6],[1]],[[2,5,7],[1]],[[2,5,8],[1]],[[2,6,0],[1]],[[2,6,1],[1]],[[2,6,2],[1]],[[2,6,3],[1]],[[2,6,4],[1]],[[2,6,5],[1]],[[2,6,6],[1]],[[2,6,7],[1]],[[2,6,8],[1]],[[2,7,0],[1]],[[2,7,1],[1]],[[2,7,2],[1]],[[2,7,3],[1]],[[2,7,4],[1]],[[2,7,5],[1]],[[2,7,6],[1]],[[2,7,7],[1]],[[2,7,8],[3,false]],[[2,8,0],[1]],[[2,8,1],[1]],[[2,8,2],[3,false]],[[2,8,3],[1]],[[2,8,4],[1]],[[2,8,5],[1]],[[2,8,6],[3,false]],[[2,8,7],[1]],[[2,8,8],[1]],[[3,0,0],[1]],[[3,0,1],[1]],[[3,0,2],[3,false]],[[3,0,3],[1]],[[3,0,4],[1]],[[3,0,5],[1]],[[3,0,6],[3,false]],[[3,0,7],[1]],[[3,0,8],[1]],[[3,1,0],[1]],[[3,1,1],[1]],[[3,1,2],[1]],[[3,1,3],[1]],[[3,1,4],[1]],[[3,1,5],[1]],[[3,1,6],[1]],[[3,1,7],[1]],[[3,1,8],[3,false]],[[3,2,0],[1]],[[3,2,1],[1]],[[3,2,2],[1]],[[3,2,3],[1]],[[3,2,4],[1]],[[3,2,5],[1]],[[3,2,6],[1]],[[3,2,7],[1]],[[3,2,8],[1]],[[3,3,0],[1]],[[3,3,1],[1]],[[3,3,2],[1]],[[3,3,3],[1]],[[3,3,4],[1]],[[3,3,5],[1]],[[3,3,6],[1]],[[3,3,7],[1]],[[3,3,8],[1]],[[3,4,0],[3,false]],[[3,4,1],[1]],[[3,4,2],[1]],[[3,4,3],[1]],[[3,4,4],[1]],[[3,4,5],[1]],[[3,4,6],[1]],[[3,4,7],[1]],[[3,4,8],[1]],[[3,5,0],[1]],[[3,5,1],[1]],[[3,5,2],[1]],[[3,5,3],[1]],[[3,5,4],[1]],[[3,5,5],[1]],[[3,5,6],[1]],[[3,5,7],[1]],[[3,5,8],[1]],[[3,6,0],[1]],[[3,6,1],[1]],[[3,6,2],[1]],[[3,6,3],[1]],[[3,6,4],[1]],[[3,6,5],[1]],[[3,6,6],[1]],[[3,6,7],[1]],[[3,6,8],[1]],[[3,7,0],[1]],[[3,7,1],[1]],[[3,7,2],[1]],[[3,7,3],[1]],[[3,7,4],[1]],[[3,7,5],[1]],[[3,7,6],[1]],[[3,7,7],[1]],[[3,7,8],[3,false]],[[3,8,0],[1]],[[3,8,1],[1]],[[3,8,2],[3,false]],[[3,8,3],[1]],[[3,8,4],[1]],[[3,8,5],[1]],[[3,8,6],[3,false]],[[3,8,7],[1]],[[3,8,8],[1]],[[4,0,0],[1]],[[4,0,1],[1]],[[4,0,2],[3,false]],[[4,0,3],[3,false]],[[4,0,4],[3,false]],[[4,0,5],[3,false]],[[4,0,6],[3,false]],[[4,0,7],[1]],[[4,0,8],[1]],[[4,1,0],[1]],[[4,1,1],[1]],[[4,1,2],[1]],[[4,1,3],[1]],[[4,1,4],[1]],[[4,1,5],[1]],[[4,1,6],[1]],[[4,1,7],[1]],[[4,1,8],[3,false]],[[4,2,0],[1]],[[4,2,1],[1]],[[4,2,2],[1]],[[4,2,3],[1]],[[4,2,4],[1]],[[4,2,5],[1]],[[4,2,6],[1]],[[4,2,7],[1]],[[4,2,8],[3,false]],[[4,3,0],[1]],[[4,3,1],[1]],[[4,3,2],[1]],[[4,3,3],[1]],[[4,3,4],[1]],[[4,3,5],[1]],[[4,3,6],[1]],[[4,3,7],[1]],[[4,3,8],[3,false]],[[4,4,0],[3,false]],[[4,4,1],[1]],[[4,4,2],[1]],[[4,4,3],[1]],[[4,4,4],[1]],[[4,4,5],[1]],[[4,4,6],[1]],[[4,4,7],[1]],[[4,4,8],[3,false]],[[4,5,0],[1]],[[4,5,1],[1]],[[4,5,2],[1]],[[4,5,3],[1]],[[4,5,4],[1]],[[4,5,5],[1]],[[4,5,6],[1]],[[4,5,7],[1]],[[4,5,8],[3,false]],[[4,6,0],[1]],[[4,6,1],[1]],[[4,6,2],[1]],[[4,6,3],[1]],[[4,6,4],[1]],[[4,6,5],[1]],[[4,6,6],[1]],[[4,6,7],[1]],[[4,6,8],[3,false]],[[4,7,0],[1]],[[4,7,1],[1]],[[4,7,2],[1]],[[4,7,3],[1]],[[4,7,4],[1]],[[4,7,5],[1]],[[4,7,6],[1]],[[4,7,7],[1]],[[4,7,8],[3,false]],[[4,8,0],[1]],[[4,8,1],[1]],[[4,8,2],[3,false]],[[4,8,3],[0]],[[4,8,4],[0]],[[4,8,5],[4,[[5],[1]]]],[[4,8,6],[3,false]],[[4,8,7],[1]],[[4,8,8],[1]],[[5,0,0],[1]],[[5,0,1],[1]],[[5,0,2],[3,false]],[[5,0,3],[1]],[[5,0,4],[1]],[[5,0,5],[1]],[[5,0,6],[3,false]],[[5,0,7],[1]],[[5,0,8],[1]],[[5,1,0],[1]],[[5,1,1],[1]],[[5,1,2],[1]],[[5,1,3],[1]],[[5,1,4],[1]],[[5,1,5],[1]],[[5,1,6],[1]],[[5,1,7],[1]],[[5,1,8],[3,false]],[[5,2,0],[1]],[[5,2,1],[1]],[[5,2,2],[1]],[[5,2,3],[1]],[[5,2,4],[1]],[[5,2,5],[1]],[[5,2,6],[1]],[[5,2,7],[1]],[[5,2,8],[1]],[[5,3,0],[1]],[[5,3,1],[1]],[[5,3,2],[1]],[[5,3,3],[1]],[[5,3,4],[1]],[[5,3,5],[1]],[[5,3,6],[1]],[[5,3,7],[1]],[[5,3,8],[1]],[[5,4,0],[3,false]],[[5,4,1],[1]],[[5,4,2],[1]],[[5,4,3],[1]],[[5,4,4],[1]],[[5,4,5],[1]],[[5,4,6],[1]],[[5,4,7],[1]],[[5,4,8],[1]],[[5,5,0],[1]],[[5,5,1],[1]],[[5,5,2],[1]],[[5,5,3],[1]],[[5,5,4],[1]],[[5,5,5],[1]],[[5,5,6],[1]],[[5,5,7],[1]],[[5,5,8],[1]],[[5,6,0],[1]],[[5,6,1],[1]],[[5,6,2],[1]],[[5,6,3],[1]],[[5,6,4],[1]],[[5,6,5],[1]],[[5,6,6],[1]],[[5,6,7],[1]],[[5,6,8],[1]],[[5,7,0],[1]],[[5,7,1],[1]],[[5,7,2],[1]],[[5,7,3],[1]],[[5,7,4],[1]],[[5,7,5],[1]],[[5,7,6],[1]],[[5,7,7],[1]],[[5,7,8],[3,false]],[[5,8,0],[1]],[[5,8,1],[1]],[[5,8,2],[3,false]],[[5,8,3],[1]],[[5,8,4],[1]],[[5,8,5],[1]],[[5,8,6],[3,false]],[[5,8,7],[1]],[[5,8,8],[1]],[[6,0,0],[1]],[[6,0,1],[1]],[[6,0,2],[3,false]],[[6,0,3],[1]],[[6,0,4],[1]],[[6,0,5],[1]],[[6,0,6],[3,false]],[[6,0,7],[1]],[[6,0,8],[1]],[[6,1,0],[1]],[[6,1,1],[1]],[[6,1,2],[1]],[[6,1,3],[1]],[[6,1,4],[1]],[[6,1,5],[1]],[[6,1,6],[1]],[[6,1,7],[1]],[[6,1,8],[3,false]],[[6,2,0],[1]],[[6,2,1],[1]],[[6,2,2],[1]],[[6,2,3],[1]],[[6,2,4],[1]],[[6,2,5],[1]],[[6,2,6],[1]],[[6,2,7],[1]],[[6,2,8],[1]],[[6,3,0],[1]],[[6,3,1],[1]],[[6,3,2],[1]],[[6,3,3],[1]],[[6,3,4],[1]],[[6,3,5],[1]],[[6,3,6],[1]],[[6,3,7],[1]],[[6,3,8],[1]],[[6,4,0],[3,false]],[[6,4,1],[1]],[[6,4,2],[1]],[[6,4,3],[1]],[[6,4,4],[1]],[[6,4,5],[1]],[[6,4,6],[1]],[[6,4,7],[1]],[[6,4,8],[1]],[[6,5,0],[1]],[[6,5,1],[1]],[[6,5,2],[1]],[[6,5,3],[1]],[[6,5,4],[1]],[[6,5,5],[1]],[[6,5,6],[1]],[[6,5,7],[1]],[[6,5,8],[1]],[[6,6,0],[1]],[[6,6,1],[1]],[[6,6,2],[1]],[[6,6,3],[1]],[[6,6,4],[1]],[[6,6,5],[1]],[[6,6,6],[1]],[[6,6,7],[1]],[[6,6,8],[1]],[[6,7,0],[1]],[[6,7,1],[1]],[[6,7,2],[1]],[[6,7,3],[1]],[[6,7,4],[1]],[[6,7,5],[1]],[[6,7,6],[1]],[[6,7,7],[1]],[[6,7,8],[3,false]],[[6,8,0],[1]],[[6,8,1],[1]],[[6,8,2],[3,false]],[[6,8,3],[1]],[[6,8,4],[1]],[[6,8,5],[1]],[[6,8,6],[3,false]],[[6,8,7],[1]],[[6,8,8],[1]],[[7,0,0],[2]],[[7,0,1],[0]],[[7,0,2],[3,false]],[[7,0,3],[1]],[[7,0,4],[1]],[[7,0,5],[1]],[[7,0,6],[3,false]],[[7,0,7],[0]],[[7,0,8],[2]],[[7,1,0],[0]],[[7,1,1],[1]],[[7,1,2],[1]],[[7,1,3],[1]],[[7,1,4],[1]],[[7,1,5],[1]],[[7,1,6],[1]],[[7,1,7],[1]],[[7,1,8],[0]],[[7,2,0],[3,false]],[[7,2,1],[1]],[[7,2,2],[1]],[[7,2,3],[1]],[[7,2,4],[1]],[[7,2,5],[1]],[[7,2,6],[1]],[[7,2,7],[1]],[[7,2,8],[1]],[[7,3,0],[3,false]],[[7,3,1],[1]],[[7,3,2],[1]],[[7,3,3],[1]],[[7,3,4],[1]],[[7,3,5],[1]],[[7,3,6],[1]],[[7,3,7],[1]],[[7,3,8],[1]],[[7,4,0],[3,false]],[[7,4,1],[1]],[[7,4,2],[1]],[[7,4,3],[1]],[[7,4,4],[1]],[[7,4,5],[1]],[[7,4,6],[1]],[[7,4,7],[1]],[[7,4,8],[1]],[[7,5,0],[3,false]],[[7,5,1],[1]],[[7,5,2],[1]],[[7,5,3],[1]],[[7,5,4],[1]],[[7,5,5],[1]],[[7,5,6],[1]],[[7,5,7],[1]],[[7,5,8],[1]],[[7,6,0],[3,false]],[[7,6,1],[1]],[[7,6,2],[1]],[[7,6,3],[1]],[[7,6,4],[1]],[[7,6,5],[1]],[[7,6,6],[1]],[[7,6,7],[1]],[[7,6,8],[1]],[[7,7,0],[0]],[[7,7,1],[1]],[[7,7,2],[1]],[[7,7,3],[1]],[[7,7,4],[1]],[[7,7,5],[1]],[[7,7,6],[1]],[[7,7,7],[1]],[[7,7,8],[0]],[[7,8,0],[2]],[[7,8,1],[0]],[[7,8,2],[3,false]],[[7,8,3],[1]],[[7,8,4],[1]],[[7,8,5],[1]],[[7,8,6],[3,false]],[[7,8,7],[0]],[[7,8,8],[2]],[[8,0,0],[1]],[[8,0,1],[1]],[[8,0,2],[1]],[[8,0,3],[1]],[[8,0,4],[1]],[[8,0,5],[1]],[[8,0,6],[1]],[[8,0,7],[1]],[[8,0,8],[1]],[[8,1,0],[2]],[[8,1,1],[0]],[[8,1,2],[3,false]],[[8,1,3],[1]],[[8,1,4],[1]],[[8,1,5],[1]],[[8,1,6],[3,false]],[[8,1,7],[0]],[[8,1,8],[2]],[[8,2,0],[1]],[[8,2,1],[1]],[[8,2,2],[3,false]],[[8,2,3],[1]],[[8,2,4],[1]],[[8,2,5],[1]],[[8,2,6],[3,false]],[[8,2,7],[1]],[[8,2,8],[1]],[[8,3,0],[1]],[[8,3,1],[1]],[[8,3,2],[3,false]],[[8,3,3],[1]],[[8,3,4],[1]],[[8,3,5],[1]],[[8,3,6],[3,false]],[[8,3,7],[1]],[[8,3,8],[1]],[[8,4,0],[1]],[[8,4,1],[1]],[[8,4,2],[3,false]],[[8,4,3],[3,false]],[[8,4,4],[3,false]],[[8,4,5],[3,false]],[[8,4,6],[3,false]],[[8,4,7],[1]],[[8,4,8],[1]],[[8,5,0],[1]],[[8,5,1],[1]],[[8,5,2],[3,false]],[[8,5,3],[1]],[[8,5,4],[1]],[[8,5,5],[1]],[[8,5,6],[3,false]],[[8,5,7],[1]],[[8,5,8],[1]],[[8,6,0],[1]],[[8,6,1],[1]],[[8,6,2],[3,false]],[[8,6,3],[1]],[[8,6,4],[1]],[[8,6,5],[1]],[[8,6,6],[3,false]],[[8,6,7],[1]],[[8,6,8],[1]],[[8,7,0],[2]],[[8,7,1],[0]],[[8,7,2],[3,false]],[[8,7,3],[1]],[[8,7,4],[1]],[[8,7,5],[1]],[[8,7,6],[3,false]],[[8,7,7],[0]],[[8,7,8],[2]],[[8,8,0],[1]],[[8,8,1],[1]],[[8,8,2],[1]],[[8,8,3],[1]],[[8,8,4],[1]],[[8,8,5],[1]],[[8,8,6],[1]],[[8,8,7],[1]],[[8,8,8],[1]]]]]"
