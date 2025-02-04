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
    , playerFrame : Frame3d Length.Meters WorldCoordinates {}
    }


type WorldCoordinates
    = WorldCoordinates Never


type alias Point =
    ( Int, Int, Int )


type alias Board =
    Array Bool


encodeBoard : Board -> String
encodeBoard board =
    Array.foldr
        (\cell enc ->
            if cell then
                "1" ++ enc

            else
                "0" ++ enc
        )
        ""
        board


decodeBoard : String -> Board
decodeBoard enc =
    String.foldl
        (\char board ->
            case char of
                '0' ->
                    Array.push False board

                '1' ->
                    Array.push True board

                _ ->
                    board
        )
        Array.empty
        enc


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
      , boardEncoding = encodeBoard board
      , playerFrame = Frame3d.atPoint (Point3d.meters 1 4 7)
      }
    , Cmd.none
    )



-- Borg Cube board


borgCube =
    "11101111111011111110111111101111000000001110111111101111111011111110111111111111111111111111111101111110111111111111111111101111111011111111111111111111111111110111111011111111111111111110111100000000011111100111111001111110011111100111111001111110000000001110111111111111111111111111111101111110111111111111111111101111111011111111111111111111111111110111111011111111111111111110111111101111111111111111111111111111011111101111111111111111111011111110111111101111111011111110111100000000111011111110111111101111"


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
                , playerFrame =
                    model.playerFrame
                        |> Frame3d.translateIn
                            (Frame3d.xDirection model.playerFrame)
                            (Length.meters 2
                                |> Quantity.per (Duration.seconds 1)
                                |> Quantity.for (Duration.milliseconds deltaMs)
                            )
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
            case key of
                " " ->
                    let
                        board =
                            Array.Extra.update
                                (pointToIndex { maxZ = model.maxZ, maxY = model.maxY } model.editorCursor)
                                not
                                model.board
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
            [ let
                editorCamera =
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

                gameplayCamera =
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
              Scene3d.sunny
                { clipDepth = Length.meters 1
                , background = Scene3d.transparentBackground
                , shadows = True
                , dimensions = ( Pixels.int 800, Pixels.int 600 )
                , upDirection = Direction3d.positiveZ
                , camera = gameplayCamera
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
                        , [ viewPlayer model.playerFrame ]
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


viewPlayer : Frame3d Length.Meters WorldCoordinates {} -> Scene3d.Entity WorldCoordinates
viewPlayer frame =
    let
        pos =
            Frame3d.originPoint frame
    in
    Scene3d.group
        [ Scene3d.sphereWithShadow
            (Scene3d.Material.matte Color.gray)
            (Sphere3d.atOrigin
                (Length.meters 0.5)
            )
        , Scene3d.coneWithShadow
            (Scene3d.Material.matte Color.red)
            (Cone3d.startingAt Point3d.origin
                Direction3d.positiveX
                { radius = Length.meters 0.5
                , length = Length.meters 0.75
                }
            )
        , Scene3d.coneWithShadow
            (Scene3d.Material.matte Color.green)
            (Cone3d.startingAt Point3d.origin
                Direction3d.positiveY
                { radius = Length.meters 0.5
                , length = Length.meters 0.75
                }
            )
        , Scene3d.coneWithShadow
            (Scene3d.Material.matte Color.blue)
            (Cone3d.startingAt Point3d.origin
                Direction3d.positiveZ
                { radius = Length.meters 0.5
                , length = Length.meters 0.75
                }
            )
        ]
        |> Scene3d.translateBy
            (Vector3d.from Point3d.origin pos)



-- |> Scene3d.rotateAround
--     (Axis3d.through pos Direction3d)


viewCell : Model -> Int -> Bool -> Scene3d.Entity WorldCoordinates
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
                (Color.rgb
                    (toFloat x / toFloat model.maxX)
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
