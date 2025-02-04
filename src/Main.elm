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
import Direction3d
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Length
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Material
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
    }


type alias Point =
    ( Int, Int, Int )


type alias Board =
    Array Bool


pointToIndex : { maxY : Int, maxZ : Int } -> Point -> Int
pointToIndex { maxY, maxZ } ( x, y, z ) =
    x * maxY * maxZ + y * maxZ + z


indexToPoint : { maxX : Int, maxY : Int, maxZ : Int } -> Int -> Point
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


init : () -> ( Model, Cmd Msg )
init () =
    ( { maxX = 8
      , maxY = 8
      , maxZ = 8
      , board = Array.repeat (8 * 8 * 8) True
      , editorCursor = ( 0, 5, 0 )
      , cameraRotation = Angle.degrees 0
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
      }
    , Cmd.none
    )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick deltaMs ->
            ( { model
                | cursorBounce =
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
              }
            , Cmd.none
            )

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
            case key of
                " " ->
                    ( { model
                        | board =
                            Array.Extra.update
                                (pointToIndex { maxZ = model.maxZ, maxY = model.maxY } model.editorCursor)
                                not
                                model.board
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
            [ let
                camera =
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
              in
              Scene3d.sunny
                { clipDepth = Length.meters 1
                , background = Scene3d.transparentBackground
                , shadows = True
                , dimensions = ( Pixels.int 800, Pixels.int 600 )
                , upDirection = Direction3d.positiveZ
                , camera = camera
                , sunlightDirection =
                    Direction3d.positiveZ
                        |> Direction3d.rotateAround Axis3d.x (Angle.degrees 60)
                        |> Direction3d.rotateAround Axis3d.z (Angle.degrees 60)
                , entities =
                    List.concat
                        [ model.board
                            |> Array.toList
                            |> List.indexedMap (viewCell model)
                        , [ viewCursor model.cursorBounce model.editorCursor ]
                        ]
                }
            ]
        ]
    }


viewCell : Model -> Int -> Bool -> Scene3d.Entity coordinates
viewCell model index solid =
    if solid then
        let
            ( x, y, z ) =
                indexToPoint
                    { maxX = model.maxX
                    , maxY = model.maxY
                    , maxZ = model.maxZ
                    }
                    index
        in
        Scene3d.blockWithShadow
            (Scene3d.Material.matte
                (Color.rgb (toFloat x / toFloat model.maxX)
                    (toFloat y / toFloat model.maxY)
                    (toFloat z / toFloat model.maxZ)
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

    else
        Scene3d.nothing


viewCursor : Animation Float -> Point -> Scene3d.Entity coordinates
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
