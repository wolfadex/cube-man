module Board exposing
    ( Axis(..)
    , Block(..)
    , BlockPalette(..)
    , Board
    , BoardLoadError(..)
    , BoardPlayError(..)
    , Enemy
    , EnemySpawnerDetails
    , ExampleBoards(..)
    , Facing(..)
    , Level
    , Point
    , ScreenCoordinates(..)
    , Target
    , WorldCoordinates(..)
    , axisToDirection3d
    , axisToLabel
    , basicMiniBoard
    , boardCodec
    , defaultBoard
    , defaultEnemySpawnerDetails
    , empty
    , emptyLevel
    , findSpawn
    , gameLights
    , gamePlayCamera
    , handleGameKeyPressed
    , indexToPoint
    , init
    , initTarget
    , point3dToPoint
    , pointToPoint3d
    , tick
    , view3dScene
    , viewBlock
    , viewEnemy
    , viewGameOver
    , viewPlayer
    , viewStats
    , zigZagBoard
    )

import AStar.Generalised
import Angle exposing (Angle)
import Axis3d
import Block3d
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Color.Interpolate
import Cone3d
import Cylinder3d
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Ease
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes
import Input
import Length exposing (Length)
import Luminance
import LuminousFlux
import Math
import Phosphor
import Pixels
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))
import Scene3d
import Scene3d.Light
import Scene3d.Material
import Scene3d.Mesh
import Serialize
import Set exposing (Set)
import SketchPlane3d
import Sphere3d
import Units.Serialize
import Vector3d
import Viewpoint3d


type WorldCoordinates
    = WorldCoordinates Never


type ScreenCoordinates
    = ScreenCoordinates Never


type Target
    = NoTarget
    | MoveForward { from : Point, to : Point, duration : Duration }
    | TraverseEdge { from : Point, to : Point, duration : Duration }


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
    | EnemySpawner EnemySpawnerDetails


type alias EnemySpawnerDetails =
    { timeTillSpawn : Duration
    , timeBetweenSpawns : Duration
    }


defaultEnemySpawnerDetails : EnemySpawnerDetails
defaultEnemySpawnerDetails =
    { timeTillSpawn = Duration.seconds 3
    , timeBetweenSpawns = Duration.seconds 3
    }


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


enemySpawnerDetailsCodec : Serialize.Codec e EnemySpawnerDetails
enemySpawnerDetailsCodec =
    Serialize.record
        (\timeBetweenSpawns ->
            { timeTillSpawn = timeBetweenSpawns
            , timeBetweenSpawns = timeBetweenSpawns
            }
        )
        |> Serialize.field .timeBetweenSpawns Units.Serialize.durationCodec
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
        (\emptyEncoder wallEncoder edgeEncoder pointPickupEncoder playerSpawnEncoder enemySpawnerEncoder value ->
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

                EnemySpawner details ->
                    enemySpawnerEncoder details
        )
        |> Serialize.variant0 Empty
        |> Serialize.variant0 Wall
        |> Serialize.variant0 Edge
        |> Serialize.variant1 PointPickup Serialize.bool
        |> Serialize.variant1 PlayerSpawn playerSpawnDetailsCodec
        |> Serialize.variant1 EnemySpawner enemySpawnerDetailsCodec
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
    ( Math.betterRound parts.x
    , Math.betterRound parts.y
    , Math.betterRound parts.z
    )


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


gameLights : Board -> Point3d Length.Meters WorldCoordinates -> Scene3d.Lights WorldCoordinates
gameLights board playerPoint =
    let
        playerLight =
            Scene3d.Light.point
                (Scene3d.Light.castsShadows True)
                { position = playerPoint
                , chromaticity = Scene3d.Light.daylight
                , intensity = LuminousFlux.lumens 640000
                }

        nearestPoints =
            board.blocks
                |> Dict.foldl
                    (\point block acc ->
                        case block of
                            PointPickup False ->
                                let
                                    p3 =
                                        pointToPoint3d point
                                in
                                ( p3
                                , p3
                                    |> Point3d.distanceFrom playerPoint
                                    |> Length.inMeters
                                )
                                    :: acc

                            _ ->
                                acc
                    )
                    []
                |> List.sortBy Tuple.second
                |> List.take 7

        withShadow p =
            Scene3d.Light.point
                (Scene3d.Light.castsShadows True)
                { position = p
                , chromaticity = Scene3d.Light.fluorescent
                , intensity = LuminousFlux.lumens 160000
                }

        withoutShadow p =
            Scene3d.Light.point
                Scene3d.Light.neverCastsShadows
                { position = p
                , chromaticity = Scene3d.Light.fluorescent
                , intensity = LuminousFlux.lumens 160000
                }
    in
    case nearestPoints of
        [ ( one, _ ), ( two, _ ), ( three, _ ), ( four, _ ), ( five, _ ), ( six, _ ), ( seven, _ ) ] ->
            Scene3d.eightLights playerLight (withShadow one) (withShadow two) (withShadow three) (withoutShadow four) (withoutShadow five) (withoutShadow six) (withoutShadow seven)

        [ ( one, _ ), ( two, _ ), ( three, _ ), ( four, _ ), ( five, _ ), ( six, _ ) ] ->
            Scene3d.sevenLights playerLight (withShadow one) (withShadow two) (withShadow three) (withoutShadow four) (withoutShadow five) (withoutShadow six)

        [ ( one, _ ), ( two, _ ), ( three, _ ), ( four, _ ), ( five, _ ) ] ->
            Scene3d.sixLights playerLight (withShadow one) (withShadow two) (withShadow three) (withoutShadow four) (withoutShadow five)

        [ ( one, _ ), ( two, _ ), ( three, _ ), ( four, _ ) ] ->
            Scene3d.fiveLights playerLight (withShadow one) (withShadow two) (withShadow three) (withoutShadow four)

        [ ( one, _ ), ( two, _ ), ( three, _ ) ] ->
            Scene3d.fourLights playerLight (withShadow one) (withShadow two) (withShadow three)

        [ ( one, _ ), ( two, _ ) ] ->
            Scene3d.threeLights playerLight (withShadow one) (withShadow two)

        [ ( one, _ ) ] ->
            Scene3d.twoLights playerLight (withShadow one)

        _ ->
            Scene3d.oneLight playerLight


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
        , background = Scene3d.backgroundColor (Color.rgb 0.25 0.25 0.25) -- Color.gray
        , exposure = Scene3d.exposureValue 15
        , lights = lights
        , toneMapping = Scene3d.noToneMapping
        , whiteBalance = Scene3d.Light.daylight
        , antialiasing = Scene3d.multisampling
        , dimensions = ( Pixels.int screenSize.width, Pixels.int screenSize.height )
        , camera = camera
        , entities = entities
        }


enemyToVisualPoint : Enemy -> Point3d Length.Meters WorldCoordinates
enemyToVisualPoint enemy =
    case enemy.movingTo of
        [] ->
            pointToPoint3d enemy.movingFrom

        movingTo :: _ ->
            Point3d.interpolateFrom
                (pointToPoint3d enemy.movingFrom)
                (pointToPoint3d movingTo)
                (Quantity.ratio
                    enemy.durationBetweenMoves
                    durationEnemyMovement
                    |> (\f -> 1 - f)
                )


viewEnemy : Enemy -> Scene3d.Entity WorldCoordinates
viewEnemy enemy =
    let
        material =
            Scene3d.Material.emissive
                (Scene3d.Light.color Color.red)
                (Luminance.nits 10000)

        dimensions =
            { radius = Length.meters 0.1
            , length = Length.meters 0.45
            }

        visualPoint =
            enemyToVisualPoint enemy
    in
    Scene3d.group
        [ Scene3d.cone
            material
            (Cone3d.startingAt
                (visualPoint
                    |> Point3d.translateIn Direction3d.negativeX (Length.meters 0.15)
                )
                Direction3d.positiveX
                dimensions
            )
        , Scene3d.cone
            material
            (Cone3d.startingAt
                (visualPoint
                    |> Point3d.translateIn Direction3d.positiveX (Length.meters 0.15)
                )
                Direction3d.negativeX
                dimensions
            )
        , Scene3d.cone
            material
            (Cone3d.startingAt
                (visualPoint
                    |> Point3d.translateIn Direction3d.negativeY (Length.meters 0.15)
                )
                Direction3d.positiveY
                dimensions
            )
        , Scene3d.cone
            material
            (Cone3d.startingAt
                (visualPoint
                    |> Point3d.translateIn Direction3d.positiveY (Length.meters 0.15)
                )
                Direction3d.negativeY
                dimensions
            )
        , Scene3d.cone
            material
            (Cone3d.startingAt
                (visualPoint
                    |> Point3d.translateIn Direction3d.negativeZ (Length.meters 0.15)
                )
                Direction3d.positiveZ
                dimensions
            )
        , Scene3d.cone
            material
            (Cone3d.startingAt
                (visualPoint
                    |> Point3d.translateIn Direction3d.positiveZ (Length.meters 0.15)
                )
                Direction3d.negativeZ
                dimensions
            )
        ]


type alias TexturedMesh =
    ( Scene3d.Mesh.Textured WorldCoordinates
    , Scene3d.Material.Texture Color
    )


playerRadius : Length
playerRadius =
    Length.meters 0.5


enemyRadius : Length
enemyRadius =
    Length.meters 0.1


{-| Frequency in HZ
-}
pulse : Float -> Float -> Float
pulse time frequency =
    0.5 * (1 + sin (2 * pi * frequency * time))


viewPlayer : Level -> Scene3d.Entity WorldCoordinates
viewPlayer level =
    let
        frame =
            level.playerFrame

        color =
            if level.invincibleFrames |> Quantity.greaterThan (Quantity 0) then
                Color.Interpolate.interpolate
                    Color.Interpolate.RGB
                    Color.gray
                    Color.blue
                    (pulse (Duration.inSeconds level.invincibleFrames) 2.5)

            else
                Color.gray
    in
    Scene3d.group
        [ Scene3d.sphere
            (Scene3d.Material.emissive
                (Scene3d.Light.color color)
                (Luminance.nits 20000)
            )
            (Sphere3d.atPoint Point3d.origin
                playerRadius
                |> Sphere3d.placeIn frame
            )
        , Scene3d.cone
            (Scene3d.Material.emissive
                -- Color.red
                (Scene3d.Light.color Color.red)
                (Luminance.nits 2500)
            )
            (Cone3d.startingAt Point3d.origin
                Direction3d.positiveX
                { radius = Length.meters 0.5
                , length = Length.meters 0.75
                }
                |> Cone3d.placeIn frame
            )
        ]
        -- Scene3d.meshWithShadow
        --     (Scene3d.Material.color Color.gray)
        --     playerMesh
        --     (Scene3d.Mesh.shadow playerMesh)
        -- |> Scene3d.scaleAbout Point3d.origin 0.3
        -- |> Scene3d.translateBy (Vector3d.from Point3d.origin (Frame3d.originPoint frame))
        |> Scene3d.rotateAround (Axis3d.through (Frame3d.originPoint frame) (Frame3d.zDirection frame))
            (Angle.degrees <|
                case level.playerFacing of
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
            -- let
            --     ( wallMesh, wallTexture ) =
            --         meshes.wallMesh
            -- in
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

        -- Scene3d.meshWithShadow
        --     (Scene3d.Material.texturedColor wallTexture)
        --     wallMesh
        --     (Scene3d.Mesh.shadow wallMesh)
        --     |> Scene3d.scaleAbout Point3d.origin 0.5
        --     |> Scene3d.translateBy
        --         (Vector3d.from Point3d.origin
        --             (Point3d.meters
        --                 (toFloat x)
        --                 (toFloat y)
        --                 (toFloat z)
        --             )
        --         )
        PointPickup collected ->
            if collected then
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

        PlayerSpawn _ ->
            Scene3d.nothing

        Empty ->
            Scene3d.nothing

        Edge ->
            Scene3d.nothing

        EnemySpawner details ->
            let
                center =
                    Point3d.meters
                        (toFloat x)
                        (toFloat y)
                        (toFloat z)

                length =
                    Length.meters 1

                q =
                    Quantity.ratio
                        details.timeTillSpawn
                        defaultEnemySpawnerDetails.timeBetweenSpawns
                        |> Ease.reverse Ease.inOutElastic
                        |> max 0
                        |> (\f -> f / 4)
            in
            Scene3d.group
                [ Scene3d.sphere
                    (Scene3d.Material.emissive
                        (Scene3d.Light.color Color.red)
                        (Luminance.nits 30000)
                    )
                    (Sphere3d.atPoint
                        (Point3d.meters
                            (toFloat x)
                            (toFloat y)
                            (toFloat z)
                        )
                        (Length.meters q)
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


setPlayerFacing : Level -> Level
setPlayerFacing model =
    if model.playerFacing == model.playerWantFacing then
        model

    else
        case model.playerTarget of
            NoTarget ->
                { model
                    | playerFacing = model.playerWantFacing
                    , playerTarget =
                        findNextTarget model.board model.playerWantFacing (model.playerFrame |> Frame3d.originPoint |> point3dToPoint) model.playerFrame
                }

            TraverseEdge _ ->
                model

            MoveForward moveDetails ->
                if oppositeFacings model.playerFacing model.playerWantFacing then
                    { model
                        | playerFacing = model.playerWantFacing
                        , playerTarget =
                            MoveForward
                                { from = moveDetails.to
                                , to = moveDetails.from
                                , duration = durationForForwardMovement |> Quantity.minus moveDetails.duration
                                }
                    }

                else
                    let
                        playerPoint =
                            model.playerFrame
                                |> Frame3d.originPoint

                        playerBoardPoint =
                            playerPoint
                                |> point3dToPoint

                        playerBoardPoint3d =
                            playerBoardPoint
                                |> pointToPoint3d
                    in
                    if
                        Quantity.equalWithin (Length.meters 0.1)
                            (Point3d.distanceFrom playerBoardPoint3d playerPoint)
                            (Length.meters 0)
                    then
                        let
                            targetBoardPoint =
                                playerBoardPoint3d
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
                                            { originPoint = playerBoardPoint3d
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
                                            |> correctPlayerFrame
                                    , playerFacing = model.playerWantFacing
                                    , playerTarget =
                                        MoveForward
                                            { from = playerBoardPoint
                                            , to = targetBoardPoint
                                            , duration = durationForForwardMovement
                                            }
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

                                    EnemySpawner _ ->
                                        model

                                    Edge ->
                                        { model
                                            | playerFrame =
                                                Frame3d.unsafe
                                                    { originPoint = playerBoardPoint3d
                                                    , xDirection = Frame3d.xDirection model.playerFrame
                                                    , yDirection = Frame3d.yDirection model.playerFrame
                                                    , zDirection = Frame3d.zDirection model.playerFrame
                                                    }
                                            , playerFacing = model.playerWantFacing
                                            , playerTarget =
                                                TraverseEdge
                                                    { from = playerBoardPoint
                                                    , to =
                                                        targetBoardPoint
                                                            |> pointToPoint3d
                                                            |> Point3d.translateIn (Frame3d.zDirection model.playerFrame) (Length.meters -1)
                                                            |> point3dToPoint
                                                    , duration = durationForEdgeMovement
                                                    }
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


initTarget : Board -> Facing -> Frame3d Length.Meters WorldCoordinates { defines : WorldCoordinates } -> Target
initTarget board facing playerFrame =
    findNextTarget board facing (playerFrame |> Frame3d.originPoint |> point3dToPoint) playerFrame


findNextTarget : Board -> Facing -> Point -> Frame3d Length.Meters WorldCoordinates { defines : WorldCoordinates } -> Target
findNextTarget board playerFacing fromPoint playerFrame =
    let
        toPoint =
            playerFrame
                |> Frame3d.translateIn
                    (case playerFacing of
                        Forward ->
                            Frame3d.xDirection playerFrame

                        Backward ->
                            Frame3d.xDirection playerFrame
                                |> Direction3d.reverse

                        Right ->
                            Frame3d.yDirection playerFrame
                                |> Direction3d.reverse

                        Left ->
                            Frame3d.yDirection playerFrame
                    )
                    (Length.meters 1)
                |> Frame3d.originPoint
                |> point3dToPoint
    in
    case Dict.get toPoint board.blocks of
        Nothing ->
            NoTarget

        Just Wall ->
            NoTarget

        Just (EnemySpawner _) ->
            NoTarget

        Just Empty ->
            MoveForward { from = fromPoint, to = toPoint, duration = durationForForwardMovement }

        Just (PlayerSpawn _) ->
            MoveForward { from = fromPoint, to = toPoint, duration = durationForForwardMovement }

        Just (PointPickup _) ->
            MoveForward { from = fromPoint, to = toPoint, duration = durationForForwardMovement }

        Just Edge ->
            TraverseEdge
                { from = fromPoint
                , to =
                    toPoint
                        |> pointToPoint3d
                        |> Point3d.translateIn (Frame3d.zDirection playerFrame) (Length.meters -1)
                        |> point3dToPoint
                , duration = durationForEdgeMovement
                }


durationForForwardMovement : Duration
durationForForwardMovement =
    Duration.seconds 0.25


durationForEdgeMovement : Duration
durationForEdgeMovement =
    Duration.seconds 0.75


durationEnemyMovement : Duration
durationEnemyMovement =
    Duration.seconds 1


tick : Duration -> Level -> Level
tick deltaDuration level =
    if level.hearts > 0 then
        level
            |> tickPlayer deltaDuration
            |> tickEnemies deltaDuration

    else
        level


tickEnemies : Duration -> Level -> Level
tickEnemies deltaDuration level =
    level
        |> moveEnemies deltaDuration
        |> tickEnemySpawners deltaDuration


moveEnemies : Duration -> Level -> Level
moveEnemies deltaDuration level =
    moveEnemiesHelper deltaDuration [] level.enemies level


moveEnemiesHelper : Duration -> List Enemy -> List Enemy -> Level -> Level
moveEnemiesHelper deltaDuration movedEnemies toMoveEnemies level =
    case toMoveEnemies of
        [] ->
            { level | enemies = movedEnemies }

        nextEnemy :: restEnemies ->
            let
                movedEnemy =
                    moveEnemy deltaDuration level nextEnemy
            in
            if level.invincibleFrames |> Quantity.greaterThan (Quantity 0) then
                moveEnemiesHelper deltaDuration (movedEnemy :: movedEnemies) restEnemies level

            else if enemyPlayerCollision (enemyToVisualPoint movedEnemy) level then
                moveEnemiesHelper deltaDuration
                    movedEnemies
                    restEnemies
                    { level
                        | hearts = level.hearts - 1
                        , invincibleFrames = initialInvincibleFrames
                    }

            else
                moveEnemiesHelper deltaDuration (movedEnemy :: movedEnemies) restEnemies level


moveEnemy : Duration -> Level -> Enemy -> Enemy
moveEnemy deltaDuration level enemy =
    case level.playerTarget of
        TraverseEdge _ ->
            enemy

        _ ->
            let
                enemyWithUpdatedPath =
                    if
                        Quantity.equalWithin
                            (Quantity 0.1)
                            enemy.durationBetweenMoves
                            durationEnemyMovement
                    then
                        { enemy
                            | movingTo =
                                level.playerFrame
                                    |> Frame3d.originPoint
                                    |> point3dToPoint
                                    |> findEnemyPath level.board.blocks enemy.movingFrom
                                    |> Maybe.withDefault []
                        }

                    else
                        enemy
            in
            case enemyWithUpdatedPath.movingTo of
                [] ->
                    { enemyWithUpdatedPath
                        | movingTo =
                            level.playerFrame
                                |> Frame3d.originPoint
                                |> point3dToPoint
                                |> findEnemyPath level.board.blocks enemyWithUpdatedPath.movingFrom
                                |> Maybe.withDefault []
                    }

                movingTo :: movingToRest ->
                    let
                        remainingDuration =
                            enemyWithUpdatedPath.durationBetweenMoves
                                |> Quantity.minus deltaDuration
                    in
                    if Quantity.compare remainingDuration (Quantity 0) == EQ then
                        { enemyWithUpdatedPath
                            | movingTo = movingToRest
                            , movingFrom = movingTo
                            , durationBetweenMoves = durationEnemyMovement
                        }

                    else if remainingDuration |> Quantity.lessThan (Quantity 0) then
                        { enemyWithUpdatedPath
                            | movingTo = movingToRest
                            , movingFrom = movingTo
                            , durationBetweenMoves = durationEnemyMovement
                        }
                            |> moveEnemy (Quantity 0 |> Quantity.minus remainingDuration) level

                    else
                        { enemyWithUpdatedPath
                            | durationBetweenMoves = remainingDuration
                        }


findEnemyPath : Dict Point Block -> Point -> Point -> Maybe (List Point)
findEnemyPath blocks =
    AStar.Generalised.findPath
        aStarCost
        (enemyMovementNeighbors blocks)


aStarCost : Point -> Point -> Float
aStarCost ( x1, y1, z1 ) ( x2, y2, z2 ) =
    toFloat (abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1))


enemyMovementNeighbors : Dict Point Block -> Point -> Set Point
enemyMovementNeighbors blocks point =
    neighborBlocks point blocks
        |> List.filterMap
            (\( p, block ) ->
                case block of
                    Empty ->
                        Just p

                    Edge ->
                        Just p

                    PointPickup _ ->
                        Just p

                    PlayerSpawn _ ->
                        Just p

                    EnemySpawner _ ->
                        Just p

                    Wall ->
                        Nothing
            )
        |> Set.fromList


tickEnemySpawners : Duration -> Level -> Level
tickEnemySpawners deltaDuration level =
    let
        board =
            level.board

        ( updatedBlocks, enemySpawned ) =
            Dict.foldl
                (\point block ( blocks, possibleEnemy ) ->
                    case block of
                        EnemySpawner details ->
                            if List.length level.enemies < 3 then
                                let
                                    timeTillSpawn =
                                        details.timeTillSpawn
                                            |> Quantity.minus deltaDuration
                                in
                                if timeTillSpawn |> Quantity.lessThan (Quantity 0) then
                                    ( Dict.insert point
                                        (EnemySpawner
                                            { details
                                                | timeTillSpawn = timeTillSpawn |> Quantity.plus details.timeBetweenSpawns
                                            }
                                        )
                                        blocks
                                    , let
                                        movingTo =
                                            level.playerFrame
                                                |> Frame3d.originPoint
                                                |> point3dToPoint
                                                |> findEnemyPath board.blocks point
                                                |> Maybe.withDefault []
                                      in
                                      Just
                                        { targetPoint = point
                                        , movingFrom = point
                                        , movingTo = movingTo
                                        , durationBetweenMoves = durationEnemyMovement
                                        }
                                    )

                                else
                                    ( Dict.insert point
                                        (EnemySpawner
                                            { details
                                                | timeTillSpawn = timeTillSpawn
                                            }
                                        )
                                        blocks
                                    , possibleEnemy
                                    )

                            else
                                ( Dict.insert point
                                    (EnemySpawner { details | timeTillSpawn = details.timeBetweenSpawns })
                                    blocks
                                , possibleEnemy
                                )

                        _ ->
                            ( Dict.insert point block blocks, possibleEnemy )
                )
                ( Dict.empty, Nothing )
                board.blocks
    in
    { level
        | board =
            { board
                | blocks = updatedBlocks
            }
        , enemies =
            case enemySpawned of
                Nothing ->
                    level.enemies

                Just enemy ->
                    enemy :: level.enemies
    }


tickPlayer : Duration -> Level -> Level
tickPlayer deltaDuration model =
    model
        |> setPlayerFacing
        |> tickPlayerOther deltaDuration
        |> movePlayer deltaDuration


tickPlayerOther : Duration -> Level -> Level
tickPlayerOther deltaDuration level =
    { level
        | invincibleFrames =
            level.invincibleFrames
                |> Quantity.minus deltaDuration
                |> Quantity.max (Quantity 0)
    }


type alias Level =
    { playerFacing : Facing
    , playerWantFacing : Facing
    , playerTarget : Target
    , playerFrame : Frame3d Length.Meters WorldCoordinates { defines : WorldCoordinates }
    , board : Board
    , score : Int
    , enemies : List Enemy
    , hearts : Int
    , invincibleFrames : Duration
    }


initialInvincibleFrames : Duration
initialInvincibleFrames =
    Duration.seconds 4


type alias Enemy =
    { targetPoint : Point
    , movingFrom : Point
    , movingTo : List Point
    , durationBetweenMoves : Duration
    }


emptyLevel : Level
emptyLevel =
    { playerFacing = Forward
    , playerWantFacing = Forward
    , playerTarget = NoTarget
    , playerFrame = Frame3d.atOrigin
    , board = empty
    , score = 0
    , enemies = []
    , hearts = 6
    , invincibleFrames = Duration.seconds 0
    }


init : Board -> Maybe Level
init board =
    case findSpawn board of
        Nothing ->
            Nothing

        Just spawnFrame ->
            Just
                { emptyLevel
                    | board = optimize board
                    , playerFrame = spawnFrame
                    , playerTarget = initTarget board Forward spawnFrame
                }


enemyPlayerCollision : Point3d Length.Meters WorldCoordinates -> Level -> Bool
enemyPlayerCollision enemyPoint level =
    Point3d.distanceFrom
        (Frame3d.originPoint level.playerFrame)
        enemyPoint
        |> Quantity.greaterThan (playerRadius |> Quantity.plus enemyRadius)
        |> not


handlePlayerCollisions : Level -> Level
handlePlayerCollisions level =
    if level.invincibleFrames |> Quantity.greaterThan (Quantity 0) then
        level

    else
        handlePlayerCollisionsHelper [] level.enemies level


handlePlayerCollisionsHelper : List Enemy -> List Enemy -> Level -> Level
handlePlayerCollisionsHelper checkedEnemies toCheckEnemies level =
    case toCheckEnemies of
        [] ->
            level

        nextEnemy :: restEnemies ->
            if enemyPlayerCollision (enemyToVisualPoint nextEnemy) level then
                { level
                    | hearts = level.hearts - 1
                    , enemies = checkedEnemies ++ restEnemies
                    , invincibleFrames = initialInvincibleFrames
                }

            else
                handlePlayerCollisionsHelper
                    (nextEnemy :: checkedEnemies)
                    restEnemies
                    level


movePlayer : Duration -> Level -> Level
movePlayer deltaDuration level =
    case level.playerTarget of
        NoTarget ->
            level

        MoveForward moveDetails ->
            let
                toPoint =
                    pointToPoint3d moveDetails.to

                remainingDuration =
                    moveDetails.duration
                        |> Quantity.minus deltaDuration
            in
            if Quantity.compare remainingDuration (Quantity 0) == EQ then
                let
                    playerFrame =
                        level.playerFrame
                            |> Frame3d.moveTo toPoint
                in
                { level
                    | playerFrame = playerFrame
                    , playerTarget = findNextTarget level.board level.playerFacing moveDetails.to playerFrame
                }
                    |> scorePoints
                    |> handlePlayerCollisions

            else if remainingDuration |> Quantity.lessThan (Quantity 0) then
                let
                    playerFrame =
                        level.playerFrame
                            |> Frame3d.moveTo toPoint

                    nextLevel =
                        { level
                            | playerFrame = playerFrame
                            , playerTarget = findNextTarget level.board level.playerFacing moveDetails.to playerFrame
                        }
                            |> scorePoints
                            |> handlePlayerCollisions
                in
                if nextLevel.hearts < 1 then
                    nextLevel

                else
                    nextLevel
                        |> tickPlayer (Quantity 0 |> Quantity.minus remainingDuration)

            else
                let
                    fromPoint =
                        pointToPoint3d moveDetails.from
                in
                { level
                    | playerFrame =
                        level.playerFrame
                            |> Frame3d.moveTo
                                (Point3d.translateBy
                                    (Vector3d.from fromPoint toPoint
                                        |> Vector3d.scaleBy
                                            (let
                                                (Quantity durFordMove) =
                                                    durationForForwardMovement

                                                (Quantity remDur) =
                                                    remainingDuration
                                             in
                                             (durFordMove - remDur) / durFordMove
                                            )
                                    )
                                    fromPoint
                                )
                    , playerTarget = MoveForward { moveDetails | duration = remainingDuration }
                }
                    |> scorePoints
                    |> handlePlayerCollisions

        TraverseEdge edgeDetails ->
            let
                remainingDuration : Duration
                remainingDuration =
                    edgeDetails.duration
                        |> Quantity.minus deltaDuration

                targetAngle : Angle
                targetAngle =
                    Angle.degrees 90

                targetSpeed : Quantity Float (Quantity.Rate Angle.Radians Duration.Seconds)
                targetSpeed =
                    targetAngle
                        |> Quantity.per durationForEdgeMovement
            in
            if remainingDuration |> Quantity.lessThan (Quantity 0) then
                let
                    traverseDurationDuration : Duration
                    traverseDurationDuration =
                        deltaDuration
                            |> Quantity.plus remainingDuration

                    edgeMovement : Angle
                    edgeMovement =
                        targetSpeed
                            |> Quantity.for traverseDurationDuration

                    playerFrame =
                        level.playerFrame
                            |> Frame3d.rotateAround
                                (Axis3d.through
                                    (level.playerFrame
                                        |> Frame3d.originPoint
                                        |> point3dToPoint
                                        |> pointToPoint3d
                                        |> Point3d.translateIn
                                            (Frame3d.zDirection level.playerFrame)
                                            (Length.meters -1)
                                    )
                                    (case level.playerFacing of
                                        Forward ->
                                            Frame3d.yDirection level.playerFrame

                                        Backward ->
                                            Frame3d.yDirection level.playerFrame
                                                |> Direction3d.reverse

                                        Left ->
                                            Frame3d.xDirection level.playerFrame
                                                |> Direction3d.reverse

                                        Right ->
                                            Frame3d.xDirection level.playerFrame
                                    )
                                )
                                edgeMovement

                    correctedPlayerFrame =
                        correctPlayerFrame playerFrame

                    nextLevel =
                        { level
                            | playerFrame = correctedPlayerFrame
                            , playerTarget = findNextTarget level.board level.playerFacing edgeDetails.to correctedPlayerFrame
                        }
                            |> scorePoints
                            |> handlePlayerCollisions
                in
                if nextLevel.hearts < 1 then
                    nextLevel

                else
                    nextLevel
                        |> tickPlayer (Quantity 0 |> Quantity.minus remainingDuration)

            else
                let
                    edgeMovement : Angle
                    edgeMovement =
                        targetSpeed
                            |> Quantity.for deltaDuration
                in
                if Quantity.compare remainingDuration (Quantity 0) == EQ then
                    let
                        playerFrame =
                            level.playerFrame
                                |> Frame3d.rotateAround
                                    (Axis3d.through
                                        (level.playerFrame
                                            |> Frame3d.originPoint
                                            |> point3dToPoint
                                            |> pointToPoint3d
                                            |> Point3d.translateIn
                                                (Frame3d.zDirection level.playerFrame)
                                                (Length.meters -1)
                                        )
                                        (case level.playerFacing of
                                            Forward ->
                                                Frame3d.yDirection level.playerFrame

                                            Backward ->
                                                Frame3d.yDirection level.playerFrame
                                                    |> Direction3d.reverse

                                            Left ->
                                                Frame3d.xDirection level.playerFrame
                                                    |> Direction3d.reverse

                                            Right ->
                                                Frame3d.xDirection level.playerFrame
                                        )
                                    )
                                    edgeMovement

                        correctedPlayerFrame =
                            correctPlayerFrame playerFrame
                    in
                    { level
                        | playerFrame = correctedPlayerFrame
                        , playerTarget = findNextTarget level.board level.playerFacing edgeDetails.to correctedPlayerFrame
                    }
                        |> scorePoints
                        |> handlePlayerCollisions

                else
                    { level
                        | playerFrame =
                            level.playerFrame
                                |> Frame3d.rotateAround
                                    (Axis3d.through
                                        (level.playerFrame
                                            |> Frame3d.originPoint
                                            |> point3dToPoint
                                            |> pointToPoint3d
                                            |> Point3d.translateIn
                                                (Frame3d.zDirection level.playerFrame)
                                                (Length.meters -1)
                                        )
                                        (case level.playerFacing of
                                            Forward ->
                                                Frame3d.yDirection level.playerFrame

                                            Backward ->
                                                Frame3d.yDirection level.playerFrame
                                                    |> Direction3d.reverse

                                            Left ->
                                                Frame3d.xDirection level.playerFrame
                                                    |> Direction3d.reverse

                                            Right ->
                                                Frame3d.xDirection level.playerFrame
                                        )
                                    )
                                    edgeMovement
                        , playerTarget = TraverseEdge { edgeDetails | duration = remainingDuration }
                    }
                        |> scorePoints
                        |> handlePlayerCollisions


viewStats : Level -> Html msg
viewStats level =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "padding" "0.5rem"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "gap" "1rem"
        , Html.Attributes.style "color" "white"
        ]
        [ Html.h3
            [ Html.Attributes.style "margin" "0"
            ]
            [ Html.text ("Score: " ++ String.fromInt level.score) ]
        , let
            fullHearts =
                level.hearts // 2

            brokenHearts =
                level.hearts |> remainderBy 2
          in
          List.range 0 (fullHearts - 1)
            |> List.map
                (\_ ->
                    Phosphor.heart Phosphor.Fill
                        |> Phosphor.toHtml []
                )
            |> (\hearts ->
                    if brokenHearts == 1 then
                        (Phosphor.heartBreak Phosphor.Fill
                            |> Phosphor.toHtml []
                        )
                            :: hearts

                    else
                        hearts
               )
            |> Html.div
                [ Html.Attributes.style "color" "red"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-direction" "row-reverse"
                , Html.Attributes.style "gap" "0.5rem"
                ]
        ]


scorePoints : Level -> Level
scorePoints boardLike =
    let
        playerActualPoint =
            Frame3d.originPoint boardLike.playerFrame

        playerBoardPoint =
            point3dToPoint playerActualPoint

        blockAtPoint =
            Dict.get playerBoardPoint boardLike.board.blocks
    in
    if blockAtPoint == Just (PointPickup False) then
        let
            boardPoint3d =
                pointToPoint3d playerBoardPoint

            distFromBoardPointCenter =
                Point3d.distanceFrom playerActualPoint boardPoint3d
        in
        if Quantity.equalWithin (Length.meters 0.25) distFromBoardPointCenter (Length.meters 0) then
            let
                board =
                    boardLike.board
            in
            { boardLike
                | board =
                    { board
                        | blocks =
                            Dict.insert playerBoardPoint
                                (PointPickup True)
                                board.blocks
                    }
                , score = boardLike.score + 50
            }

        else
            boardLike

    else
        boardLike


correctPlayerFrame : Frame3d Length.Meters WorldCoordinates { defines : WorldCoordinates } -> Frame3d Length.Meters WorldCoordinates { defines : WorldCoordinates }
correctPlayerFrame playerFrame =
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


optimize : Board -> Board
optimize board =
    { board
        | blocks =
            Dict.foldl
                (\point block blocks ->
                    let
                        neighbors =
                            neighborBlocks point board.blocks
                    in
                    if List.length neighbors == 6 && List.all (\( _, b ) -> b == Wall) neighbors then
                        blocks

                    else
                        Dict.insert point block blocks
                )
                Dict.empty
                board.blocks
    }


neighborBlocks : Point -> Dict Point Block -> List ( Point, Block )
neighborBlocks ( x, y, z ) blocks =
    List.filterMap identity
        [ let
            p =
                ( x + 1, y, z )
          in
          Dict.get p blocks |> Maybe.map (Tuple.pair p)
        , let
            p =
                ( x - 1, y, z )
          in
          Dict.get p blocks |> Maybe.map (Tuple.pair p)
        , let
            p =
                ( x, y + 1, z )
          in
          Dict.get p blocks |> Maybe.map (Tuple.pair p)
        , let
            p =
                ( x, y - 1, z )
          in
          Dict.get p blocks |> Maybe.map (Tuple.pair p)
        , let
            p =
                ( x, y, z + 1 )
          in
          Dict.get p blocks |> Maybe.map (Tuple.pair p)
        , let
            p =
                ( x, y, z - 1 )
          in
          Dict.get p blocks |> Maybe.map (Tuple.pair p)
        ]


handleGameKeyPressed : ({ m | level : Level } -> { m | level : Level }) -> Input.Mapping -> String -> { m | level : Level } -> { m | level : Level }
handleGameKeyPressed onOpenMenu inputMapping key model =
    let
        level =
            model.level
    in
    if Input.isInputKey inputMapping.moveUp key then
        { model | level = { level | playerWantFacing = Forward } }

    else if Input.isInputKey inputMapping.moveDown key then
        { model | level = { level | playerWantFacing = Backward } }

    else if Input.isInputKey inputMapping.moveLeft key then
        { model | level = { level | playerWantFacing = Left } }

    else if Input.isInputKey inputMapping.moveRight key then
        { model | level = { level | playerWantFacing = Right } }

    else if key == "Escape" then
        onOpenMenu model

    else
        model


viewGameOver : Level -> Html msg -> Html msg
viewGameOver level nextActions =
    if level.hearts > 0 then
        Html.text ""

    else
        Html.div
            [ Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "top" "50%"
            , Html.Attributes.style "left" "50%"
            , Html.Attributes.style "transform" "translate(-50%, -50%)"
            , Html.Attributes.style "background-color" "rgba(255, 0, 0, 0.5)"
            , Html.Attributes.style "font-size" "4rem"
            , Html.Attributes.style "backdrop-filter" "blur(5px)"
            , Html.Attributes.style "padding" "4rem 8rem"
            , Html.Attributes.style "border-radius" "1rem"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "gap" "1rem"
            ]
            [ Html.span [ Html.Attributes.style "color" "white" ] [ Html.text "Game Over" ]
            , nextActions
            ]



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
    "[1,[5,5,5,[[[0,0,0],[1]],[[0,0,1],[1]],[[0,0,2],[2]],[[0,0,3],[1]],[[0,0,4],[1]],[[0,1,0],[1]],[[0,1,1],[1]],[[0,1,2],[3,false]],[[0,1,3],[1]],[[0,1,4],[1]],[[0,2,0],[2]],[[0,2,1],[3,false]],[[0,2,2],[3,false]],[[0,2,3],[3,false]],[[0,2,4],[2]],[[0,3,0],[1]],[[0,3,1],[1]],[[0,3,2],[3,false]],[[0,3,3],[1]],[[0,3,4],[1]],[[0,4,0],[1]],[[0,4,1],[1]],[[0,4,2],[2]],[[0,4,3],[1]],[[0,4,4],[1]],[[1,0,0],[1]],[[1,0,1],[1]],[[1,0,2],[3,false]],[[1,0,3],[1]],[[1,0,4],[1]],[[1,1,0],[1]],[[1,1,1],[1]],[[1,1,2],[1]],[[1,1,3],[1]],[[1,1,4],[1]],[[1,2,0],[3,false]],[[1,2,1],[1]],[[1,2,2],[1]],[[1,2,3],[1]],[[1,2,4],[3,false]],[[1,3,0],[1]],[[1,3,1],[1]],[[1,3,2],[1]],[[1,3,3],[1]],[[1,3,4],[1]],[[1,4,0],[1]],[[1,4,1],[1]],[[1,4,2],[3,false]],[[1,4,3],[1]],[[1,4,4],[1]],[[2,0,0],[2]],[[2,0,1],[3,false]],[[2,0,2],[3,false]],[[2,0,3],[3,false]],[[2,0,4],[2]],[[2,1,0],[3,false]],[[2,1,1],[1]],[[2,1,2],[1]],[[2,1,3],[1]],[[2,1,4],[3,false]],[[2,2,0],[5,[[0,3]]]],[[2,2,1],[1]],[[2,2,2],[1]],[[2,2,3],[1]],[[2,2,4],[4,[[0],[2]]]],[[2,3,0],[3,false]],[[2,3,1],[1]],[[2,3,2],[1]],[[2,3,3],[1]],[[2,3,4],[3,false]],[[2,4,0],[2]],[[2,4,1],[3,false]],[[2,4,2],[3,false]],[[2,4,3],[3,false]],[[2,4,4],[2]],[[3,0,0],[1]],[[3,0,1],[1]],[[3,0,2],[3,false]],[[3,0,3],[1]],[[3,0,4],[1]],[[3,1,0],[1]],[[3,1,1],[1]],[[3,1,2],[1]],[[3,1,3],[1]],[[3,1,4],[1]],[[3,2,0],[3,false]],[[3,2,1],[1]],[[3,2,2],[1]],[[3,2,3],[1]],[[3,2,4],[3,false]],[[3,3,0],[1]],[[3,3,1],[1]],[[3,3,2],[1]],[[3,3,3],[1]],[[3,3,4],[1]],[[3,4,0],[1]],[[3,4,1],[1]],[[3,4,2],[3,false]],[[3,4,3],[1]],[[3,4,4],[1]],[[4,0,0],[1]],[[4,0,1],[1]],[[4,0,2],[2]],[[4,0,3],[1]],[[4,0,4],[1]],[[4,1,0],[1]],[[4,1,1],[1]],[[4,1,2],[3,false]],[[4,1,3],[1]],[[4,1,4],[1]],[[4,2,0],[2]],[[4,2,1],[3,false]],[[4,2,2],[3,false]],[[4,2,3],[3,false]],[[4,2,4],[2]],[[4,3,0],[1]],[[4,3,1],[1]],[[4,3,2],[3,false]],[[4,3,3],[1]],[[4,3,4],[1]],[[4,4,0],[1]],[[4,4,1],[1]],[[4,4,2],[2]],[[4,4,3],[1]],[[4,4,4],[1]]]]]"


zigZagBoard : String
zigZagBoard =
    "[1,[9,9,9,[[[0,0,0],[1]],[[0,0,1],[1]],[[0,0,2],[1]],[[0,0,3],[1]],[[0,0,4],[2]],[[0,0,5],[1]],[[0,0,6],[1]],[[0,0,7],[1]],[[0,0,8],[1]],[[0,1,0],[2]],[[0,1,1],[3,false]],[[0,1,2],[1]],[[0,1,3],[3,false]],[[0,1,4],[3,false]],[[0,1,5],[3,false]],[[0,1,6],[1]],[[0,1,7],[3,false]],[[0,1,8],[2]],[[0,2,0],[1]],[[0,2,1],[3,false]],[[0,2,2],[1]],[[0,2,3],[3,false]],[[0,2,4],[1]],[[0,2,5],[3,false]],[[0,2,6],[1]],[[0,2,7],[3,false]],[[0,2,8],[1]],[[0,3,0],[1]],[[0,3,1],[3,false]],[[0,3,2],[1]],[[0,3,3],[3,false]],[[0,3,4],[3,false]],[[0,3,5],[3,false]],[[0,3,6],[1]],[[0,3,7],[3,false]],[[0,3,8],[1]],[[0,4,0],[1]],[[0,4,1],[3,false]],[[0,4,2],[1]],[[0,4,3],[3,false]],[[0,4,4],[1]],[[0,4,5],[3,false]],[[0,4,6],[1]],[[0,4,7],[3,false]],[[0,4,8],[1]],[[0,5,0],[1]],[[0,5,1],[3,false]],[[0,5,2],[3,false]],[[0,5,3],[3,false]],[[0,5,4],[1]],[[0,5,5],[3,false]],[[0,5,6],[3,false]],[[0,5,7],[3,false]],[[0,5,8],[1]],[[0,6,0],[1]],[[0,6,1],[3,false]],[[0,6,2],[1]],[[0,6,3],[3,false]],[[0,6,4],[1]],[[0,6,5],[3,false]],[[0,6,6],[1]],[[0,6,7],[3,false]],[[0,6,8],[1]],[[0,7,0],[1]],[[0,7,1],[3,false]],[[0,7,2],[3,false]],[[0,7,3],[3,false]],[[0,7,4],[1]],[[0,7,5],[3,false]],[[0,7,6],[3,false]],[[0,7,7],[3,false]],[[0,7,8],[1]],[[0,8,0],[1]],[[0,8,1],[1]],[[0,8,2],[1]],[[0,8,3],[1]],[[0,8,4],[1]],[[0,8,5],[1]],[[0,8,6],[1]],[[0,8,7],[1]],[[0,8,8],[1]],[[1,0,0],[2]],[[1,0,1],[3,false]],[[1,0,2],[1]],[[1,0,3],[3,false]],[[1,0,4],[3,false]],[[1,0,5],[3,false]],[[1,0,6],[1]],[[1,0,7],[3,false]],[[1,0,8],[2]],[[1,1,0],[3,false]],[[1,1,1],[1]],[[1,1,2],[1]],[[1,1,3],[1]],[[1,1,4],[1]],[[1,1,5],[1]],[[1,1,6],[1]],[[1,1,7],[1]],[[1,1,8],[3,false]],[[1,2,0],[1]],[[1,2,1],[1]],[[1,2,2],[1]],[[1,2,3],[1]],[[1,2,4],[1]],[[1,2,5],[1]],[[1,2,6],[1]],[[1,2,7],[1]],[[1,2,8],[3,false]],[[1,3,0],[3,false]],[[1,3,1],[1]],[[1,3,2],[1]],[[1,3,3],[1]],[[1,3,4],[1]],[[1,3,5],[1]],[[1,3,6],[1]],[[1,3,7],[1]],[[1,3,8],[3,false]],[[1,4,0],[3,false]],[[1,4,1],[1]],[[1,4,2],[1]],[[1,4,3],[1]],[[1,4,4],[1]],[[1,4,5],[1]],[[1,4,6],[1]],[[1,4,7],[1]],[[1,4,8],[3,false]],[[1,5,0],[3,false]],[[1,5,1],[1]],[[1,5,2],[1]],[[1,5,3],[1]],[[1,5,4],[1]],[[1,5,5],[1]],[[1,5,6],[1]],[[1,5,7],[1]],[[1,5,8],[3,false]],[[1,6,0],[3,false]],[[1,6,1],[1]],[[1,6,2],[1]],[[1,6,3],[1]],[[1,6,4],[1]],[[1,6,5],[1]],[[1,6,6],[1]],[[1,6,7],[1]],[[1,6,8],[3,false]],[[1,7,0],[3,false]],[[1,7,1],[1]],[[1,7,2],[1]],[[1,7,3],[1]],[[1,7,4],[1]],[[1,7,5],[1]],[[1,7,6],[1]],[[1,7,7],[1]],[[1,7,8],[3,false]],[[1,8,0],[1]],[[1,8,1],[3,false]],[[1,8,2],[3,false]],[[1,8,3],[3,false]],[[1,8,4],[1]],[[1,8,5],[3,false]],[[1,8,6],[3,false]],[[1,8,7],[3,false]],[[1,8,8],[1]],[[2,0,0],[1]],[[2,0,1],[3,false]],[[2,0,2],[1]],[[2,0,3],[3,false]],[[2,0,4],[1]],[[2,0,5],[3,false]],[[2,0,6],[1]],[[2,0,7],[3,false]],[[2,0,8],[1]],[[2,1,0],[3,false]],[[2,1,1],[1]],[[2,1,2],[1]],[[2,1,3],[1]],[[2,1,4],[1]],[[2,1,5],[1]],[[2,1,6],[1]],[[2,1,7],[1]],[[2,1,8],[1]],[[2,2,0],[1]],[[2,2,1],[1]],[[2,2,2],[1]],[[2,2,3],[1]],[[2,2,4],[1]],[[2,2,5],[1]],[[2,2,6],[1]],[[2,2,7],[1]],[[2,2,8],[1]],[[2,3,0],[3,false]],[[2,3,1],[1]],[[2,3,2],[1]],[[2,3,3],[1]],[[2,3,4],[1]],[[2,3,5],[1]],[[2,3,6],[1]],[[2,3,7],[1]],[[2,3,8],[1]],[[2,4,0],[1]],[[2,4,1],[1]],[[2,4,2],[1]],[[2,4,3],[1]],[[2,4,4],[1]],[[2,4,5],[1]],[[2,4,6],[1]],[[2,4,7],[1]],[[2,4,8],[1]],[[2,5,0],[3,false]],[[2,5,1],[1]],[[2,5,2],[1]],[[2,5,3],[1]],[[2,5,4],[1]],[[2,5,5],[1]],[[2,5,6],[1]],[[2,5,7],[1]],[[2,5,8],[1]],[[2,6,0],[1]],[[2,6,1],[1]],[[2,6,2],[1]],[[2,6,3],[1]],[[2,6,4],[1]],[[2,6,5],[1]],[[2,6,6],[1]],[[2,6,7],[1]],[[2,6,8],[1]],[[2,7,0],[3,false]],[[2,7,1],[1]],[[2,7,2],[1]],[[2,7,3],[1]],[[2,7,4],[1]],[[2,7,5],[1]],[[2,7,6],[1]],[[2,7,7],[1]],[[2,7,8],[3,false]],[[2,8,0],[1]],[[2,8,1],[3,false]],[[2,8,2],[1]],[[2,8,3],[3,false]],[[2,8,4],[1]],[[2,8,5],[3,false]],[[2,8,6],[1]],[[2,8,7],[3,false]],[[2,8,8],[1]],[[3,0,0],[1]],[[3,0,1],[3,false]],[[3,0,2],[1]],[[3,0,3],[3,false]],[[3,0,4],[3,false]],[[3,0,5],[3,false]],[[3,0,6],[1]],[[3,0,7],[3,false]],[[3,0,8],[1]],[[3,1,0],[3,false]],[[3,1,1],[1]],[[3,1,2],[1]],[[3,1,3],[1]],[[3,1,4],[1]],[[3,1,5],[1]],[[3,1,6],[1]],[[3,1,7],[1]],[[3,1,8],[3,false]],[[3,2,0],[1]],[[3,2,1],[1]],[[3,2,2],[1]],[[3,2,3],[1]],[[3,2,4],[1]],[[3,2,5],[1]],[[3,2,6],[1]],[[3,2,7],[1]],[[3,2,8],[3,false]],[[3,3,0],[0]],[[3,3,1],[1]],[[3,3,2],[1]],[[3,3,3],[1]],[[3,3,4],[1]],[[3,3,5],[1]],[[3,3,6],[1]],[[3,3,7],[1]],[[3,3,8],[0]],[[3,4,0],[1]],[[3,4,1],[1]],[[3,4,2],[1]],[[3,4,3],[1]],[[3,4,4],[1]],[[3,4,5],[1]],[[3,4,6],[1]],[[3,4,7],[1]],[[3,4,8],[4,[[3],[0]]]],[[3,5,0],[0]],[[3,5,1],[1]],[[3,5,2],[1]],[[3,5,3],[1]],[[3,5,4],[1]],[[3,5,5],[1]],[[3,5,6],[1]],[[3,5,7],[1]],[[3,5,8],[0]],[[3,6,0],[1]],[[3,6,1],[1]],[[3,6,2],[1]],[[3,6,3],[1]],[[3,6,4],[1]],[[3,6,5],[1]],[[3,6,6],[1]],[[3,6,7],[1]],[[3,6,8],[3,false]],[[3,7,0],[3,false]],[[3,7,1],[1]],[[3,7,2],[1]],[[3,7,3],[1]],[[3,7,4],[1]],[[3,7,5],[1]],[[3,7,6],[1]],[[3,7,7],[1]],[[3,7,8],[3,false]],[[3,8,0],[1]],[[3,8,1],[3,false]],[[3,8,2],[3,false]],[[3,8,3],[3,false]],[[3,8,4],[1]],[[3,8,5],[3,false]],[[3,8,6],[3,false]],[[3,8,7],[3,false]],[[3,8,8],[1]],[[4,0,0],[1]],[[4,0,1],[3,false]],[[4,0,2],[1]],[[4,0,3],[3,false]],[[4,0,4],[1]],[[4,0,5],[3,false]],[[4,0,6],[1]],[[4,0,7],[3,false]],[[4,0,8],[1]],[[4,1,0],[3,false]],[[4,1,1],[1]],[[4,1,2],[1]],[[4,1,3],[1]],[[4,1,4],[1]],[[4,1,5],[1]],[[4,1,6],[1]],[[4,1,7],[1]],[[4,1,8],[3,false]],[[4,2,0],[1]],[[4,2,1],[1]],[[4,2,2],[1]],[[4,2,3],[1]],[[4,2,4],[1]],[[4,2,5],[1]],[[4,2,6],[1]],[[4,2,7],[1]],[[4,2,8],[1]],[[4,3,0],[0]],[[4,3,1],[1]],[[4,3,2],[1]],[[4,3,3],[1]],[[4,3,4],[1]],[[4,3,5],[1]],[[4,3,6],[1]],[[4,3,7],[1]],[[4,3,8],[1]],[[4,4,0],[5,[[0,3]]]],[[4,4,1],[1]],[[4,4,2],[1]],[[4,4,3],[1]],[[4,4,4],[1]],[[4,4,5],[1]],[[4,4,6],[1]],[[4,4,7],[1]],[[4,4,8],[1]],[[4,5,0],[0]],[[4,5,1],[1]],[[4,5,2],[1]],[[4,5,3],[1]],[[4,5,4],[1]],[[4,5,5],[1]],[[4,5,6],[1]],[[4,5,7],[1]],[[4,5,8],[1]],[[4,6,0],[1]],[[4,6,1],[1]],[[4,6,2],[1]],[[4,6,3],[1]],[[4,6,4],[1]],[[4,6,5],[1]],[[4,6,6],[1]],[[4,6,7],[1]],[[4,6,8],[1]],[[4,7,0],[3,false]],[[4,7,1],[1]],[[4,7,2],[1]],[[4,7,3],[1]],[[4,7,4],[1]],[[4,7,5],[1]],[[4,7,6],[1]],[[4,7,7],[1]],[[4,7,8],[3,false]],[[4,8,0],[1]],[[4,8,1],[3,false]],[[4,8,2],[1]],[[4,8,3],[3,false]],[[4,8,4],[1]],[[4,8,5],[3,false]],[[4,8,6],[1]],[[4,8,7],[3,false]],[[4,8,8],[1]],[[5,0,0],[1]],[[5,0,1],[3,false]],[[5,0,2],[3,false]],[[5,0,3],[3,false]],[[5,0,4],[1]],[[5,0,5],[3,false]],[[5,0,6],[3,false]],[[5,0,7],[3,false]],[[5,0,8],[1]],[[5,1,0],[3,false]],[[5,1,1],[1]],[[5,1,2],[1]],[[5,1,3],[1]],[[5,1,4],[1]],[[5,1,5],[1]],[[5,1,6],[1]],[[5,1,7],[1]],[[5,1,8],[3,false]],[[5,2,0],[1]],[[5,2,1],[1]],[[5,2,2],[1]],[[5,2,3],[1]],[[5,2,4],[1]],[[5,2,5],[1]],[[5,2,6],[1]],[[5,2,7],[1]],[[5,2,8],[3,false]],[[5,3,0],[0]],[[5,3,1],[1]],[[5,3,2],[1]],[[5,3,3],[1]],[[5,3,4],[1]],[[5,3,5],[1]],[[5,3,6],[1]],[[5,3,7],[1]],[[5,3,8],[3,false]],[[5,4,0],[1]],[[5,4,1],[1]],[[5,4,2],[1]],[[5,4,3],[1]],[[5,4,4],[1]],[[5,4,5],[1]],[[5,4,6],[1]],[[5,4,7],[1]],[[5,4,8],[3,false]],[[5,5,0],[0]],[[5,5,1],[1]],[[5,5,2],[1]],[[5,5,3],[1]],[[5,5,4],[1]],[[5,5,5],[1]],[[5,5,6],[1]],[[5,5,7],[1]],[[5,5,8],[3,false]],[[5,6,0],[1]],[[5,6,1],[1]],[[5,6,2],[1]],[[5,6,3],[1]],[[5,6,4],[1]],[[5,6,5],[1]],[[5,6,6],[1]],[[5,6,7],[1]],[[5,6,8],[3,false]],[[5,7,0],[3,false]],[[5,7,1],[1]],[[5,7,2],[1]],[[5,7,3],[1]],[[5,7,4],[1]],[[5,7,5],[1]],[[5,7,6],[1]],[[5,7,7],[1]],[[5,7,8],[3,false]],[[5,8,0],[1]],[[5,8,1],[3,false]],[[5,8,2],[1]],[[5,8,3],[3,false]],[[5,8,4],[3,false]],[[5,8,5],[3,false]],[[5,8,6],[1]],[[5,8,7],[3,false]],[[5,8,8],[1]],[[6,0,0],[1]],[[6,0,1],[3,false]],[[6,0,2],[1]],[[6,0,3],[3,false]],[[6,0,4],[1]],[[6,0,5],[3,false]],[[6,0,6],[1]],[[6,0,7],[3,false]],[[6,0,8],[1]],[[6,1,0],[3,false]],[[6,1,1],[1]],[[6,1,2],[1]],[[6,1,3],[1]],[[6,1,4],[1]],[[6,1,5],[1]],[[6,1,6],[1]],[[6,1,7],[1]],[[6,1,8],[3,false]],[[6,2,0],[1]],[[6,2,1],[1]],[[6,2,2],[1]],[[6,2,3],[1]],[[6,2,4],[1]],[[6,2,5],[1]],[[6,2,6],[1]],[[6,2,7],[1]],[[6,2,8],[1]],[[6,3,0],[3,false]],[[6,3,1],[1]],[[6,3,2],[1]],[[6,3,3],[1]],[[6,3,4],[1]],[[6,3,5],[1]],[[6,3,6],[1]],[[6,3,7],[1]],[[6,3,8],[1]],[[6,4,0],[1]],[[6,4,1],[1]],[[6,4,2],[1]],[[6,4,3],[1]],[[6,4,4],[1]],[[6,4,5],[1]],[[6,4,6],[1]],[[6,4,7],[1]],[[6,4,8],[1]],[[6,5,0],[3,false]],[[6,5,1],[1]],[[6,5,2],[1]],[[6,5,3],[1]],[[6,5,4],[1]],[[6,5,5],[1]],[[6,5,6],[1]],[[6,5,7],[1]],[[6,5,8],[1]],[[6,6,0],[1]],[[6,6,1],[1]],[[6,6,2],[1]],[[6,6,3],[1]],[[6,6,4],[1]],[[6,6,5],[1]],[[6,6,6],[1]],[[6,6,7],[1]],[[6,6,8],[1]],[[6,7,0],[3,false]],[[6,7,1],[1]],[[6,7,2],[1]],[[6,7,3],[1]],[[6,7,4],[1]],[[6,7,5],[1]],[[6,7,6],[1]],[[6,7,7],[1]],[[6,7,8],[1]],[[6,8,0],[1]],[[6,8,1],[3,false]],[[6,8,2],[1]],[[6,8,3],[3,false]],[[6,8,4],[1]],[[6,8,5],[3,false]],[[6,8,6],[1]],[[6,8,7],[3,false]],[[6,8,8],[1]],[[7,0,0],[1]],[[7,0,1],[3,false]],[[7,0,2],[3,false]],[[7,0,3],[3,false]],[[7,0,4],[1]],[[7,0,5],[3,false]],[[7,0,6],[3,false]],[[7,0,7],[3,false]],[[7,0,8],[1]],[[7,1,0],[3,false]],[[7,1,1],[1]],[[7,1,2],[1]],[[7,1,3],[1]],[[7,1,4],[1]],[[7,1,5],[1]],[[7,1,6],[1]],[[7,1,7],[1]],[[7,1,8],[3,false]],[[7,2,0],[3,false]],[[7,2,1],[1]],[[7,2,2],[1]],[[7,2,3],[1]],[[7,2,4],[1]],[[7,2,5],[1]],[[7,2,6],[1]],[[7,2,7],[1]],[[7,2,8],[3,false]],[[7,3,0],[3,false]],[[7,3,1],[1]],[[7,3,2],[1]],[[7,3,3],[1]],[[7,3,4],[1]],[[7,3,5],[1]],[[7,3,6],[1]],[[7,3,7],[1]],[[7,3,8],[3,false]],[[7,4,0],[3,false]],[[7,4,1],[1]],[[7,4,2],[1]],[[7,4,3],[1]],[[7,4,4],[1]],[[7,4,5],[1]],[[7,4,6],[1]],[[7,4,7],[1]],[[7,4,8],[3,false]],[[7,5,0],[3,false]],[[7,5,1],[1]],[[7,5,2],[1]],[[7,5,3],[1]],[[7,5,4],[1]],[[7,5,5],[1]],[[7,5,6],[1]],[[7,5,7],[1]],[[7,5,8],[3,false]],[[7,6,0],[1]],[[7,6,1],[1]],[[7,6,2],[1]],[[7,6,3],[1]],[[7,6,4],[1]],[[7,6,5],[1]],[[7,6,6],[1]],[[7,6,7],[1]],[[7,6,8],[3,false]],[[7,7,0],[3,false]],[[7,7,1],[1]],[[7,7,2],[1]],[[7,7,3],[1]],[[7,7,4],[1]],[[7,7,5],[1]],[[7,7,6],[1]],[[7,7,7],[1]],[[7,7,8],[3,false]],[[7,8,0],[2]],[[7,8,1],[3,false]],[[7,8,2],[1]],[[7,8,3],[3,false]],[[7,8,4],[3,false]],[[7,8,5],[3,false]],[[7,8,6],[1]],[[7,8,7],[3,false]],[[7,8,8],[2]],[[8,0,0],[1]],[[8,0,1],[1]],[[8,0,2],[1]],[[8,0,3],[1]],[[8,0,4],[1]],[[8,0,5],[1]],[[8,0,6],[1]],[[8,0,7],[1]],[[8,0,8],[1]],[[8,1,0],[1]],[[8,1,1],[3,false]],[[8,1,2],[3,false]],[[8,1,3],[3,false]],[[8,1,4],[1]],[[8,1,5],[3,false]],[[8,1,6],[3,false]],[[8,1,7],[3,false]],[[8,1,8],[1]],[[8,2,0],[1]],[[8,2,1],[3,false]],[[8,2,2],[1]],[[8,2,3],[3,false]],[[8,2,4],[1]],[[8,2,5],[3,false]],[[8,2,6],[1]],[[8,2,7],[3,false]],[[8,2,8],[1]],[[8,3,0],[1]],[[8,3,1],[3,false]],[[8,3,2],[3,false]],[[8,3,3],[3,false]],[[8,3,4],[1]],[[8,3,5],[3,false]],[[8,3,6],[3,false]],[[8,3,7],[3,false]],[[8,3,8],[1]],[[8,4,0],[1]],[[8,4,1],[3,false]],[[8,4,2],[1]],[[8,4,3],[3,false]],[[8,4,4],[1]],[[8,4,5],[3,false]],[[8,4,6],[1]],[[8,4,7],[3,false]],[[8,4,8],[1]],[[8,5,0],[1]],[[8,5,1],[3,false]],[[8,5,2],[1]],[[8,5,3],[3,false]],[[8,5,4],[3,false]],[[8,5,5],[3,false]],[[8,5,6],[1]],[[8,5,7],[3,false]],[[8,5,8],[1]],[[8,6,0],[1]],[[8,6,1],[3,false]],[[8,6,2],[1]],[[8,6,3],[3,false]],[[8,6,4],[1]],[[8,6,5],[3,false]],[[8,6,6],[1]],[[8,6,7],[3,false]],[[8,6,8],[1]],[[8,7,0],[2]],[[8,7,1],[3,false]],[[8,7,2],[1]],[[8,7,3],[3,false]],[[8,7,4],[3,false]],[[8,7,5],[3,false]],[[8,7,6],[1]],[[8,7,7],[3,false]],[[8,7,8],[2]],[[8,8,0],[1]],[[8,8,1],[1]],[[8,8,2],[1]],[[8,8,3],[1]],[[8,8,4],[2]],[[8,8,5],[1]],[[8,8,6],[1]],[[8,8,7],[1]],[[8,8,8],[1]]]]]"
