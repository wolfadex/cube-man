module Main exposing (main)

import Angle exposing (Angle)
import Animation exposing (Animation)
import Array exposing (Array)
import Array.Extra
import Axis3d exposing (Axis3d)
import Axis3d.Extra
import Block3d
import BoundingBox3d
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Cone3d
import Direction3d exposing (Direction3d)
import Duration
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Length
import LineSegment3d
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity
import Rectangle2d
import Scene3d
import Scene3d.Material
import SketchPlane3d
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
    , screenSize : { width : Int, height : Int }
    , xLowerVisible : Int
    , xUpperVisible : Int
    , yLowerVisible : Int
    , yUpperVisible : Int
    , zLowerVisible : Int
    , zUpperVisible : Int
    , selectedBlockType : Block
    , editorCursor : Point
    , cameraRotation : Angle
    , cameraElevation : Angle
    , mouseDragging : EditorMouseInteraction
    , cursorBounce : Animation Float
    , boardEncoding : String
    , mode : Mode
    , playerFrame : Frame3d Length.Meters WorldCoordinates { defines : {} }
    , playerFacing : Facing
    , playerWantFacing : Facing
    , playerMovingAcrossEdge : Maybe Angle
    }


type EditorMouseInteraction
    = NoInteraction
    | InteractionStart Json.Decode.Value
    | InteractionMoving Json.Decode.Value


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
      , xLowerVisible = 0
      , xUpperVisible = maxX - 1
      , yLowerVisible = 0
      , yUpperVisible = maxY - 1
      , zLowerVisible = 0
      , zUpperVisible = maxZ - 1
      , selectedBlockType = Empty
      , board = board
      , screenSize = { width = 800, height = 600 }
      , editorCursor = ( 0, 0, 0 )
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
        ]


decodeKeyPressed : Json.Decode.Decoder Msg
decodeKeyPressed =
    Json.Decode.map KeyPressed
        (Json.Decode.field "key" Json.Decode.string)


type ScreenCoordinates
    = ScreenCoordinates Never


type Msg
    = Tick Float
    | KeyPressed String
    | MouseDown Json.Decode.Value
    | MouseUp (Point2d Pixels ScreenCoordinates)
    | MouseMove Json.Decode.Value (Point2d Pixels ScreenCoordinates) (Point2d Pixels ScreenCoordinates)
    | EncodingChanged String
    | LoadBoard
    | ChangeMode
    | XLowerVisibleChanged Int
    | XUpperVisibleChanged Int
    | YLowerVisibleChanged Int
    | YUpperVisibleChanged Int
    | ZLowerVisibleChanged Int
    | ZUpperVisibleChanged Int
    | BlockTypeSelected Block


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick deltaMs ->
            ( model
                -- |> animateCursor deltaMs
                |> tickPlayer deltaMs
            , Cmd.none
            )

        EncodingChanged boardEncoding ->
            ( { model | boardEncoding = boardEncoding }, Cmd.none )

        LoadBoard ->
            ( { model | board = decodeBoard model.boardEncoding }, Cmd.none )

        ChangeMode ->
            ( { model
                | mode =
                    case model.mode of
                        Editor ->
                            Game

                        Game ->
                            Editor
              }
            , Cmd.none
            )

        MouseDown pointerId ->
            ( { model | mouseDragging = InteractionStart pointerId }, Cmd.none )

        MouseUp point ->
            case model.mouseDragging of
                NoInteraction ->
                    ( model, Cmd.none )

                InteractionStart _ ->
                    ( { model
                        | mouseDragging = NoInteraction
                        , board =
                            Array.set (pointToIndex model model.editorCursor)
                                model.selectedBlockType
                                model.board
                      }
                    , Cmd.none
                    )

                InteractionMoving _ ->
                    ( { model | mouseDragging = NoInteraction }, Cmd.none )

        MouseMove pointerId offset movement ->
            case model.mouseDragging of
                NoInteraction ->
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

                        maybeIntersection =
                            Array.foldl
                                (\block ( index, maybeInter ) ->
                                    ( index + 1
                                    , case block of
                                        Wall ->
                                            let
                                                boundingBox =
                                                    BoundingBox3d.withDimensions
                                                        ( Length.meters 1, Length.meters 1, Length.meters 1 )
                                                        (pointToPoint3d (indexToPoint model index))
                                            in
                                            case Axis3d.Extra.intersectionAxisAlignedBoundingBox3d ray boundingBox of
                                                Nothing ->
                                                    maybeInter

                                                Just intersection ->
                                                    Just intersection

                                        _ ->
                                            maybeInter
                                    )
                                )
                                ( 0, Nothing )
                                model.board
                                |> Tuple.second
                    in
                    case maybeIntersection of
                        Nothing ->
                            ( model, Cmd.none )

                        Just intersection ->
                            ( { model
                                | editorCursor =
                                    case model.selectedBlockType of
                                        Empty ->
                                            Point3d.along (Axis3d.reverse intersection) (Length.meters 0.5)
                                                |> point3dToPoint

                                        Wall ->
                                            Point3d.along intersection (Length.meters 0.5)
                                                |> point3dToPoint

                                        Edge ->
                                            Point3d.along intersection (Length.meters 0.5)
                                                |> point3dToPoint
                              }
                            , Cmd.none
                            )

                InteractionStart _ ->
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
                                |> Quantity.minus
                                    (Point2d.yCoordinate (Debug.log "move" movement)
                                        |> Pixels.toFloat
                                        |> Angle.degrees
                                    )
                      }
                    , Cmd.none
                    )

                InteractionMoving _ ->
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
                                    (Point2d.yCoordinate (Debug.log "move" movement)
                                        |> Pixels.toFloat
                                        |> Angle.degrees
                                    )
                      }
                    , Cmd.none
                    )

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

        KeyPressed key ->
            case model.mode of
                Editor ->
                    -- handleEditorKeyPressed key model
                    ( model, Cmd.none )

                Game ->
                    handleGameKeyPressed key model


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


animateCursor : Float -> Model -> Model
animateCursor deltaMs model =
    case model.mode of
        Game ->
            model

        Editor ->
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
                                    (Length.meters 4
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
                    Json.Decode.map2 (\x y -> MouseUp (Point2d.pixels x y))
                        (Json.Decode.field "offsetX" Json.Decode.float)
                        (Json.Decode.field "offsetX" Json.Decode.float)

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
            ]
            [ Html.div
                ([ Html.Attributes.style "grid-column" <|
                    case model.mode of
                        Editor ->
                            "1"

                        Game ->
                            "1 / 2"
                 , Html.Attributes.style "grid-row" "1"
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
                                let
                                    cam : Camera3d Length.Meters WorldCoordinates
                                    cam =
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
                                in
                                cam

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
                                List.concat
                                    [ model.board
                                        |> Array.toList
                                        |> List.indexedMap (viewBlock model)
                                    , [ viewCursor model.cursorBounce model.editorCursor ]
                                    , [ viewOrientationArrows ]
                                    ]

                            Game ->
                                List.concat
                                    [ model.board
                                        |> Array.toList
                                        |> List.indexedMap (viewBlock model)
                                    , [ viewPlayer model.playerFacing model.playerFrame ]
                                    ]
                    }
                ]
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
                    Html.div
                        [ Html.Attributes.style "grid-column" "2"
                        , Html.Attributes.style "padding" "0.5rem"
                        , Html.Attributes.style "height" "100vh"
                        , Html.Attributes.style "overflow" "auto"
                        ]
                        [ Html.button
                            [ Html.Attributes.type_ "button"
                            , Html.Events.onClick ChangeMode
                            ]
                            [ Html.text "Play Level"
                            ]
                        , Html.hr [] []
                        , Html.div
                            [ Html.Attributes.attribute "role" "group"
                            ]
                            [ Html.button
                                [ Html.Attributes.attribute "aria-current" <|
                                    if model.selectedBlockType == Wall then
                                        "true"

                                    else
                                        "false"
                                , Html.Attributes.type_ "button"
                                , Html.Events.onClick (BlockTypeSelected Wall)
                                ]
                                [ Html.text "Wall"
                                ]
                            , Html.button
                                [ Html.Attributes.attribute "aria-current" <|
                                    if model.selectedBlockType == Edge then
                                        "true"

                                    else
                                        "false"
                                , Html.Attributes.type_ "button"
                                , Html.Events.onClick (BlockTypeSelected Edge)
                                ]
                                [ Html.text "Edge"
                                ]
                            , Html.button
                                [ Html.Attributes.attribute "aria-current" <|
                                    if model.selectedBlockType == Empty then
                                        "true"

                                    else
                                        "false"
                                , Html.Attributes.type_ "button"
                                , Html.Events.onClick (BlockTypeSelected Empty)
                                ]
                                [ Html.text "Empty"
                                ]
                            ]
                        , Html.form []
                            [ Html.fieldset
                                []
                                [ Html.label [] [ Html.span [] [ Html.text "X Visibility" ] ]
                                , Html.fieldset []
                                    [ Html.label [] [ Html.span [] [ Html.text "Lower bound" ] ]
                                    , Html.input
                                        [ Html.Attributes.type_ "range"
                                        , Html.Attributes.min "0"
                                        , Html.Attributes.max (String.fromInt (model.maxX - 1))
                                        , Html.Attributes.step "1"
                                        , Html.Attributes.value (String.fromInt model.xLowerVisible)
                                        , Html.Events.onInput (String.toInt >> Maybe.withDefault model.xLowerVisible >> XLowerVisibleChanged)
                                        ]
                                        []
                                    , Html.label [] [ Html.span [] [ Html.text "Upper bound" ] ]
                                    , Html.input
                                        [ Html.Attributes.type_ "range"
                                        , Html.Attributes.min "0"
                                        , Html.Attributes.max (String.fromInt (model.maxX - 1))
                                        , Html.Attributes.step "1"
                                        , Html.Attributes.value (String.fromInt model.xUpperVisible)
                                        , Html.Events.onInput (String.toInt >> Maybe.withDefault model.xLowerVisible >> XUpperVisibleChanged)
                                        ]
                                        []
                                    ]
                                ]
                            , Html.fieldset []
                                [ Html.label [] [ Html.span [] [ Html.text "Y Visibility" ] ]
                                , Html.fieldset []
                                    [ Html.label [] [ Html.span [] [ Html.text "Lower bound" ] ]
                                    , Html.input
                                        [ Html.Attributes.type_ "range"
                                        , Html.Attributes.min "0"
                                        , Html.Attributes.max (String.fromInt (model.maxY - 1))
                                        , Html.Attributes.step "1"
                                        , Html.Attributes.value (String.fromInt model.yLowerVisible)
                                        , Html.Events.onInput (String.toInt >> Maybe.withDefault model.yLowerVisible >> YLowerVisibleChanged)
                                        ]
                                        []
                                    , Html.label [] [ Html.span [] [ Html.text "Upper bound" ] ]
                                    , Html.input
                                        [ Html.Attributes.type_ "range"
                                        , Html.Attributes.min "0"
                                        , Html.Attributes.max (String.fromInt (model.maxY - 1))
                                        , Html.Attributes.step "1"
                                        , Html.Attributes.value (String.fromInt model.yUpperVisible)
                                        , Html.Events.onInput (String.toInt >> Maybe.withDefault model.yLowerVisible >> YUpperVisibleChanged)
                                        ]
                                        []
                                    ]
                                ]
                            , Html.fieldset []
                                [ Html.label [] [ Html.span [] [ Html.text "Z Visibility" ] ]
                                , Html.fieldset []
                                    [ Html.label [] [ Html.span [] [ Html.text "Lower bound" ] ]
                                    , Html.input
                                        [ Html.Attributes.type_ "range"
                                        , Html.Attributes.min "0"
                                        , Html.Attributes.max (String.fromInt (model.maxZ - 1))
                                        , Html.Attributes.step "1"
                                        , Html.Attributes.value (String.fromInt model.zLowerVisible)
                                        , Html.Events.onInput (String.toInt >> Maybe.withDefault model.zLowerVisible >> ZLowerVisibleChanged)
                                        ]
                                        []
                                    , Html.label [] [ Html.span [] [ Html.text "Upper bound" ] ]
                                    , Html.input
                                        [ Html.Attributes.type_ "range"
                                        , Html.Attributes.min "0"
                                        , Html.Attributes.max (String.fromInt (model.maxZ - 1))
                                        , Html.Attributes.step "1"
                                        , Html.Attributes.value (String.fromInt model.zUpperVisible)
                                        , Html.Events.onInput (String.toInt >> Maybe.withDefault model.zLowerVisible >> ZUpperVisibleChanged)
                                        ]
                                        []
                                    ]
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
                                ]
                            , Html.fieldset [ Html.Attributes.attribute "role" "group" ]
                                [ Html.input
                                    [ Html.Attributes.value model.boardEncoding
                                    , Html.Events.onInput EncodingChanged
                                    ]
                                    []
                                , Html.button
                                    [ Html.Attributes.type_ "submit" ]
                                    [ Html.text "Load" ]
                                ]
                            ]
                        ]
            ]
        ]
    }


viewOrientationArrows : Scene3d.Entity WorldCoordinates
viewOrientationArrows =
    Scene3d.group
        [ Scene3d.lineSegment
            (Scene3d.Material.color Color.red)
            (LineSegment3d.from
                (Point3d.meters -1 -1 -1)
                (Point3d.meters 1 -1 -1)
            )
        , Scene3d.cone
            (Scene3d.Material.matte Color.red)
            (Cone3d.startingAt (Point3d.meters 1 -1 -1)
                Direction3d.positiveX
                { radius = Length.meters 0.125
                , length = Length.meters 0.25
                }
            )
        , Scene3d.lineSegment
            (Scene3d.Material.color Color.green)
            (LineSegment3d.from
                (Point3d.meters -1 -1 -1)
                (Point3d.meters -1 1 -1)
            )
        , Scene3d.cone
            (Scene3d.Material.matte Color.green)
            (Cone3d.startingAt (Point3d.meters -1 1 -1)
                Direction3d.positiveY
                { radius = Length.meters 0.125
                , length = Length.meters 0.25
                }
            )
        , Scene3d.lineSegment
            (Scene3d.Material.color Color.blue)
            (LineSegment3d.from
                (Point3d.meters -1 -1 -1)
                (Point3d.meters -1 -1 1)
            )
        , Scene3d.cone
            (Scene3d.Material.matte Color.blue)
            (Cone3d.startingAt (Point3d.meters -1 -1 1)
                Direction3d.positiveZ
                { radius = Length.meters 0.125
                , length = Length.meters 0.25
                }
            )
        ]


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
            if x < model.xLowerVisible || x > model.xUpperVisible || y < model.yLowerVisible || y > model.yUpperVisible || z < model.zLowerVisible || z > model.zUpperVisible then
                Scene3d.nothing

            else
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
                    if x < model.xLowerVisible || x > model.xUpperVisible || y < model.yLowerVisible || y > model.yUpperVisible || z < model.zLowerVisible || z > model.zUpperVisible then
                        Scene3d.nothing

                    else
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
