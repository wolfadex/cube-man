module Main exposing (main)

import Angle exposing (Angle)
import Animation exposing (Animation)
import Array exposing (Array)
import Array.Extra
import Axis3d
import Block3d
import Browser
import Browser.Events
import Camera3d
import Color
import Cone3d
import Direction3d
import Duration
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Length
import Pixels
import Point3d exposing (Point3d)
import Quantity
import Scene3d
import Scene3d.Material
import Sphere3d
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
    { maxX : Int
    , maxY : Int
    , maxZ : Int
    , board : Board
    , editorCursor : Point
    , cameraRotation : Angle
    , mouseDragging : Bool
    , cursorBounce : Animation Float
    , boardEncoding : String
    , mode : Mode
    , playerFrame : Frame3d Length.Meters WorldCoordinates { defines : {} }
    , playerFacing : Facing
    , playerWantFacing : Facing
    , playerMovingAcrossEdge : Maybe Angle
    }


type Mode
    = Editor
    | Game


type Facing
    = Forward
    | Backward
    | Left
    | Right


type WorldCoordinates
    = WorldCoordinates Never


type alias Point =
    ( Int, Int, Int )


type alias Board =
    Array Block


type Block
    = Empty
    | Wall
    | Edge


encodeBoard : Board -> String
encodeBoard board =
    Array.foldr
        (\block enc ->
            (case block of
                Empty ->
                    "0"

                Wall ->
                    "1"

                Edge ->
                    "2"
            )
                ++ enc
        )
        ""
        board


decodeBoard : String -> Board
decodeBoard enc =
    String.foldl
        (\char board ->
            case char of
                '0' ->
                    Array.push Empty board

                '1' ->
                    Array.push Wall board

                '2' ->
                    Array.push Edge board

                _ ->
                    board
        )
        Array.empty
        enc


pointToIndex : { m | maxY : Int, maxZ : Int } -> Point -> Int
pointToIndex { maxY, maxZ } ( x, y, z ) =
    x * maxY * maxZ + y * maxZ + z


indexToPoint : { m | maxX : Int, maxY : Int, maxZ : Int } -> Int -> Point
indexToPoint { maxX, maxY, maxZ } index =
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

        board =
            -- Array.repeat (maxX * maxY * maxZ) True
            decodeBoard borgCube
    in
    ( { maxX = maxX
      , maxY = maxY
      , maxZ = maxZ
      , board = board
      , editorCursor = ( 1, 4, 7 )
      , cameraRotation = Angle.degrees 135
      , mouseDragging = False
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
      , mode = Game
      , boardEncoding = encodeBoard board
      , playerFrame = Frame3d.atPoint (Point3d.meters 1 4 7)
      , playerFacing = Forward
      , playerWantFacing = Forward
      , playerMovingAcrossEdge = Nothing
      }
    , Cmd.none
    )



-- Borg Cube board


borgCube =
    "11121111111011111110111111101111200000021110111111101111111211111110111111111111111111111111111101111110111111111111111111101111111011111111111111111111111111110111111011111111111111111110111120000002011111100111111001111110011111100111111001111110200000021110111111111111111111111111111101111110111111111111111111101111111011111111111111111111111111110111111011111111111111111110111111101111111111111111111111111111011111101111111111111111111011111112111111101111111011111110111120000002111011111110111111121111"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyPress decodeKeyPressed
        , if model.mouseDragging then
            Browser.Events.onMouseUp decodeMouseUp

          else
            Browser.Events.onMouseDown deocdeMouseDown
        , if model.mouseDragging then
            Browser.Events.onMouseMove decodeMouseMove

          else
            Sub.none
        ]


deocdeMouseDown : Json.Decode.Decoder Msg
deocdeMouseDown =
    Json.Decode.field "button" Json.Decode.int
        |> Json.Decode.andThen
            (\button ->
                if button == 0 then
                    Json.Decode.succeed MouseDown

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


decodeMouseMove : Json.Decode.Decoder Msg
decodeMouseMove =
    Json.Decode.map MouseMove
        (Json.Decode.field "movementX" Json.Decode.float)


decodeKeyPressed : Json.Decode.Decoder Msg
decodeKeyPressed =
    Json.Decode.map KeyPressed
        (Json.Decode.field "key" Json.Decode.string)


type Msg
    = Tick Float
    | KeyPressed String
    | MouseDown
    | MouseUp
    | MouseMove Float
    | EncodingChanged String
    | LoadBoard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick deltaMs ->
            ( tickPlayer deltaMs
                { model
                    | cursorBounce =
                        case model.mode of
                            Editor ->
                                let
                                    ( _, anim ) =
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
                                            deltaMs
                                            model.cursorBounce
                                in
                                anim

                            Game ->
                                model.cursorBounce
                }
            , Cmd.none
            )

        EncodingChanged boardEncoding ->
            ( { model | boardEncoding = boardEncoding }, Cmd.none )

        LoadBoard ->
            ( { model | board = decodeBoard model.boardEncoding }, Cmd.none )

        MouseDown ->
            ( { model | mouseDragging = True }, Cmd.none )

        MouseUp ->
            ( { model | mouseDragging = False }, Cmd.none )

        MouseMove delta ->
            ( { model
                | cameraRotation =
                    model.cameraRotation
                        |> Quantity.minus
                            (Angle.degrees delta)
              }
            , Cmd.none
            )

        KeyPressed key ->
            case model.mode of
                Editor ->
                    handleEditorKeyPressed key model

                Game ->
                    handleGameKeyPressed key model


tickPlayer : Float -> Model -> Model
tickPlayer deltaMs model =
    model
        |> setPlayerFacing
        |> movePlayer deltaMs


movePlayer : Float -> Model -> Model
movePlayer deltaMs model =
    let
        doEdgeMovement () =
            let
                edgeMovement =
                    Angle.degrees 45
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
    in
    case model.playerMovingAcrossEdge of
        Nothing ->
            let
                nearBlock =
                    Array.get
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
                            |> pointToIndex model
                        )
                        model.board
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

                Just Empty ->
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
                                    (Length.meters 2
                                        |> Quantity.per (Duration.seconds 1)
                                        |> Quantity.for (Duration.milliseconds deltaMs)
                                    )
                    }

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
                            , yDirection =
                                playerFrame
                                    |> Frame3d.yDirection
                                    |> Direction3d.toVector
                                    |> Vector3d.normalize
                                    |> Vector3d.direction
                                    |> Maybe.withDefault Direction3d.positiveX
                            , zDirection =
                                playerFrame
                                    |> Frame3d.zDirection
                                    |> Direction3d.toVector
                                    |> Vector3d.normalize
                                    |> Vector3d.direction
                                    |> Maybe.withDefault Direction3d.positiveX
                            }

                    else
                        playerFrame
                , playerMovingAcrossEdge =
                    if edgeTravelComplete then
                        Nothing

                    else
                        Just totalMovement
            }


setPlayerFacing : Model -> Model
setPlayerFacing model =
    if model.playerFacing == model.playerWantFacing then
        model

    else
        let
            playerBoardPoint =
                model.playerFrame
                    |> Frame3d.originPoint
                    |> point3dToPoint

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
        in
        if oppositeFacings model.playerFacing model.playerWantFacing then
            { model | playerFacing = model.playerWantFacing }

        else if
            Quantity.equalWithin (Length.meters 0.1)
                (Point3d.distanceFrom (pointToPoint3d playerBoardPoint) (Frame3d.originPoint model.playerFrame))
                (Length.meters 0)
        then
            case Array.get (pointToIndex model targetBoardPoint) model.board of
                Nothing ->
                    model

                Just black ->
                    case black of
                        Wall ->
                            model

                        Empty ->
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
                                        , yDirection =
                                            model.playerFrame
                                                |> Frame3d.yDirection
                                                |> Direction3d.toVector
                                                |> Vector3d.normalize
                                                |> Vector3d.direction
                                                |> Maybe.withDefault Direction3d.positiveX
                                        , zDirection =
                                            model.playerFrame
                                                |> Frame3d.zDirection
                                                |> Direction3d.toVector
                                                |> Vector3d.normalize
                                                |> Vector3d.direction
                                                |> Maybe.withDefault Direction3d.positiveX
                                        }
                                , playerFacing = model.playerWantFacing
                            }

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


handleEditorKeyPressed : String -> Model -> ( Model, Cmd Msg )
handleEditorKeyPressed key model =
    case key of
        " " ->
            let
                board =
                    Array.Extra.update
                        (pointToIndex model model.editorCursor)
                        cycleBlockType
                        model.board

                cycleBlockType block =
                    case block of
                        Empty ->
                            Edge

                        Wall ->
                            Empty

                        Edge ->
                            Wall
            in
            ( { model
                | board = board
                , boardEncoding = encodeBoard board
              }
            , Cmd.none
            )

        "w" ->
            let
                ( x, y, z ) =
                    model.editorCursor
            in
            ( { model
                | editorCursor =
                    ( min (model.maxX - 1) (x + 1)
                    , y
                    , z
                    )
              }
            , Cmd.none
            )

        "s" ->
            let
                ( x, y, z ) =
                    model.editorCursor
            in
            ( { model
                | editorCursor =
                    ( max 0 (x - 1)
                    , y
                    , z
                    )
              }
            , Cmd.none
            )

        "a" ->
            let
                ( x, y, z ) =
                    model.editorCursor
            in
            ( { model
                | editorCursor =
                    ( x
                    , min (model.maxY - 1) (y + 1)
                    , z
                    )
              }
            , Cmd.none
            )

        "d" ->
            let
                ( x, y, z ) =
                    model.editorCursor
            in
            ( { model
                | editorCursor =
                    ( x
                    , max 0 (y - 1)
                    , z
                    )
              }
            , Cmd.none
            )

        "e" ->
            let
                ( x, y, z ) =
                    model.editorCursor
            in
            ( { model
                | editorCursor =
                    ( x
                    , y
                    , min (model.maxZ - 1) (z + 1)
                    )
              }
            , Cmd.none
            )

        "q" ->
            let
                ( x, y, z ) =
                    model.editorCursor
            in
            ( { model
                | editorCursor =
                    ( x
                    , y
                    , max 0 (z - 1)
                    )
              }
            , Cmd.none
            )

        _ ->
            let
                _ =
                    Debug.log "unhandled key" key
            in
            ( model
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Cube-Man"
    , body =
        [ Html.div
            [ Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "display" "inline-flex"
            ]
            [ Scene3d.sunny
                { clipDepth = Length.meters 1
                , background = Scene3d.transparentBackground
                , shadows = True
                , dimensions = ( Pixels.int 800, Pixels.int 600 )
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
                            Camera3d.perspective
                                { viewpoint =
                                    Viewpoint3d.orbitZ
                                        { focalPoint = Point3d.meters 3.5 3.5 2
                                        , azimuth = model.cameraRotation
                                        , elevation = Angle.degrees 15
                                        , distance = Length.meters 30
                                        }
                                , verticalFieldOfView = Angle.degrees 30
                                }
                , sunlightDirection =
                    Direction3d.positiveZ
                        |> Direction3d.rotateAround Axis3d.x (Angle.degrees 60)
                        |> Direction3d.rotateAround Axis3d.z (Angle.degrees 60)
                , entities =
                    List.concat
                        [ model.board
                            |> Array.toList
                            |> List.indexedMap (viewBlock model)
                        , case model.mode of
                            Editor ->
                                [ viewCursor model.cursorBounce model.editorCursor ]

                            Game ->
                                []
                        , case model.mode of
                            Editor ->
                                []

                            Game ->
                                [ viewPlayer model.playerFacing model.playerFrame ]
                        ]
                }
            ]
        , Html.br [] []
        , Html.form
            [ Html.Events.onSubmit LoadBoard ]
            [ Html.button
                [ Html.Attributes.type_ "submit" ]
                [ Html.text "Load" ]
            , Html.input
                [ Html.Attributes.value model.boardEncoding
                , Html.Events.onInput EncodingChanged
                ]
                []
            ]
        ]
    }


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
        -- |> Scene3d.translateBy (Vector3d.from Point3d.origin pos)
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


viewBlock : Model -> Int -> Block -> Scene3d.Entity WorldCoordinates
viewBlock model index block =
    case block of
        Wall ->
            let
                ( x, y, z ) =
                    indexToPoint
                        model
                        index
            in
            Scene3d.blockWithShadow
                (Scene3d.Material.matte
                    (Color.rgb
                        (toFloat x * 1.2 / toFloat model.maxX)
                        (toFloat y * 1.2 / toFloat model.maxY)
                        (toFloat z * 1.2 / toFloat model.maxZ)
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

        Empty ->
            Scene3d.nothing

        Edge ->
            case model.mode of
                Game ->
                    Scene3d.nothing

                Editor ->
                    let
                        ( x, y, z ) =
                            indexToPoint
                                model
                                index
                    in
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
                            ( Length.meters 1, Length.meters 1, Length.meters 1 )
                        )


viewCursor : Animation Float -> Point -> Scene3d.Entity WorldCoordinates
viewCursor bounceAnim ( x, y, z ) =
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
            (Scene3d.Material.color Color.white)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.positiveY offset
                    |> Frame3d.translateIn Direction3d.positiveZ offset
                )
                ( length, Length.meters 0.1, Length.meters 0.1 )
            )
        , Scene3d.block
            (Scene3d.Material.color Color.white)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.positiveY offset
                    |> Frame3d.translateIn Direction3d.negativeZ offset
                )
                ( length, Length.meters 0.1, Length.meters 0.1 )
            )
        , Scene3d.block
            (Scene3d.Material.color Color.white)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.negativeY offset
                    |> Frame3d.translateIn Direction3d.positiveZ offset
                )
                ( length, Length.meters 0.1, Length.meters 0.1 )
            )
        , Scene3d.block
            (Scene3d.Material.color Color.white)
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
            (Scene3d.Material.color Color.white)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.negativeY offset
                    |> Frame3d.translateIn Direction3d.negativeX offset
                )
                ( Length.meters 0.1, Length.meters 0.1, length )
            )
        , Scene3d.block
            (Scene3d.Material.color Color.white)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.positiveY offset
                    |> Frame3d.translateIn Direction3d.negativeX offset
                )
                ( Length.meters 0.1, Length.meters 0.1, length )
            )
        , Scene3d.block
            (Scene3d.Material.color Color.white)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.negativeY offset
                    |> Frame3d.translateIn Direction3d.positiveX offset
                )
                ( Length.meters 0.1, Length.meters 0.1, length )
            )
        , Scene3d.block
            (Scene3d.Material.color Color.white)
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
            (Scene3d.Material.color Color.white)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.positiveZ offset
                    |> Frame3d.translateIn Direction3d.negativeX offset
                )
                ( Length.meters 0.1, length, Length.meters 0.1 )
            )
        , Scene3d.block
            (Scene3d.Material.color Color.white)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.negativeZ offset
                    |> Frame3d.translateIn Direction3d.negativeX offset
                )
                ( Length.meters 0.1, length, Length.meters 0.1 )
            )
        , Scene3d.block
            (Scene3d.Material.color Color.white)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.positiveZ offset
                    |> Frame3d.translateIn Direction3d.positiveX offset
                )
                ( Length.meters 0.1, length, Length.meters 0.1 )
            )
        , Scene3d.block
            (Scene3d.Material.color Color.white)
            (Block3d.centeredOn
                (center
                    |> Frame3d.atPoint
                    |> Frame3d.translateIn Direction3d.negativeZ offset
                    |> Frame3d.translateIn Direction3d.positiveX offset
                )
                ( Length.meters 0.1, length, Length.meters 0.1 )
            )
        ]
