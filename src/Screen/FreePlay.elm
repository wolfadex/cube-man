module Screen.FreePlay exposing
    ( BoardPreviewTile
    , FreePlayMode
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Board
import Browser.Dom
import Browser.Events
import Dict
import Duration exposing (Duration)
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra
import Input
import Json.Decode
import Json.Encode
import Phosphor
import Screen exposing (Screen)
import Serialize
import Shared
import Task


type alias Model =
    { level : Board.Level
    , boardLoadError : Maybe Board.BoardLoadError
    , boardPlayError : Maybe Board.BoardPlayError

    --
    , freePlayMode : FreePlayMode
    , showFreePlayMenu : Bool
    }


type FreePlayMode
    = FreePlayBoardLoaded
    | FreePlayBoardSelection


init : { toSharedMsg : Shared.Msg -> msg, toMsg : Msg -> msg } -> ( Model, Cmd msg )
init { toSharedMsg } =
    ( { level = Board.emptyLevel
      , boardLoadError = Nothing
      , boardPlayError = Nothing
      , freePlayMode = FreePlayBoardSelection
      , showFreePlayMenu = False
      }
    , Browser.Dom.getViewportOf "game-viewport"
        |> Task.attempt (Shared.ViewportResized >> toSharedMsg)
    )


subscriptions : { toSharedMsg : Shared.Msg -> msg, toMsg : Msg -> msg } -> Model -> Sub msg
subscriptions { toSharedMsg, toMsg } model =
    Sub.batch
        [ Browser.Events.onResize (\width height -> Shared.SetScreenSize { width = width, height = height } |> toSharedMsg)
        , case model.freePlayMode of
            FreePlayBoardSelection ->
                Sub.none

            FreePlayBoardLoaded ->
                if model.showFreePlayMenu then
                    Sub.none

                else
                    Sub.batch
                        [ Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick >> toMsg)
                        , Browser.Events.onKeyDown (decodeKeyDown toMsg)
                        ]
        ]


decodeKeyDown : (Msg -> msg) -> Json.Decode.Decoder msg
decodeKeyDown toMsg =
    Json.Decode.map (KeyDown >> toMsg)
        (Json.Decode.field "key" Json.Decode.string)


type Msg
    = Tick Duration
    | KeyDown String
    | LoadFreePlayBoard String
    | ExitFreePlayBoard
    | ShowFreePlayMenu Bool


update : Shared.LoadedModel -> Msg -> Model -> ( Model, Cmd Msg )
update sharedModel msg model =
    case msg of
        Tick deltaMs ->
            model
                |> tick sharedModel deltaMs

        ExitFreePlayBoard ->
            ( { model
                | freePlayMode = FreePlayBoardSelection
                , showFreePlayMenu = False
              }
            , Cmd.none
            )

        LoadFreePlayBoard encoding ->
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
                Ok board ->
                    case Board.init board of
                        Nothing ->
                            ( { model | boardPlayError = Just Board.MissingPlayerSpawn }, Cmd.none )

                        Just level ->
                            ( { model
                                | freePlayMode = FreePlayBoardLoaded
                                , level = level
                                , boardPlayError = Nothing
                              }
                            , Cmd.none
                            )

                Err error ->
                    ( { model
                        | boardLoadError = Just error
                      }
                    , Cmd.none
                    )

        ShowFreePlayMenu show ->
            ( { model | showFreePlayMenu = show }, Cmd.none )

        KeyDown key ->
            case model.freePlayMode of
                FreePlayBoardSelection ->
                    ( model, Cmd.none )

                FreePlayBoardLoaded ->
                    ( Board.handleGameKeyPressed
                        (\m -> { m | showFreePlayMenu = True })
                        sharedModel.inputMapping
                        key
                        model
                    , Cmd.none
                    )


tick : Shared.LoadedModel -> Duration -> Model -> ( Model, Cmd msg )
tick sharedModel deltaMs model =
    case model.freePlayMode of
        FreePlayBoardSelection ->
            ( model, Cmd.none )

        FreePlayBoardLoaded ->
            Board.tick sharedModel.audioMapping deltaMs model.level
                |> Tuple.mapFirst (\level -> { model | level = level })


view : { setScreen : Screen -> msg, toSharedMsg : Shared.Msg -> msg, sharedModel : Shared.LoadedModel, toMsg : Msg -> msg, model : Model } -> List (Html msg)
view { setScreen, toSharedMsg, sharedModel, toMsg, model } =
    [ Html.div
        [ Html.Attributes.id "game-viewport"
        , Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        ]
      <|
        case model.freePlayMode of
            FreePlayBoardSelection ->
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
                            , Html.Events.onClick (setScreen Screen.Menu)
                            ]
                            [ Html.text "Main Menu" ]
                        ]
                    , Html.div
                        [ Html.Attributes.style "width" "100vw"
                        , Html.Attributes.style "display" "grid"
                        , Html.Attributes.style "grid-template-columns" "repeat(5, 1fr)"
                        , Html.Attributes.style "gap" "2rem"
                        , Html.Attributes.style "padding" "2rem"
                        ]
                        (List.map (viewBoardPreviewTile toMsg)
                            [ { name = "Mini"
                              , boardEncoding = Board.basicMiniBoard
                              }
                            , { name = "Something Familiar"
                              , boardEncoding = Board.somethingFamiliarBoard
                              }
                            , { name = "Zig-Zag"
                              , boardEncoding = Board.zigZagBoard
                              }
                            , { name = "Layers"
                              , boardEncoding = Board.layersBoard
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

            FreePlayBoardLoaded ->
                [ Board.view3dScene
                    (Board.gameLights model.level.board (Frame3d.originPoint model.level.playerFrame))
                    sharedModel.screenSize
                    (Board.gamePlayCamera model.level.playerFrame)
                    (List.concat
                        [ model.level.board.blocks
                            |> Dict.toList
                            |> List.map
                                (Board.viewBlock
                                 -- { wallMesh = sharedModel.wallMesh
                                 -- }
                                )
                        , [ Board.viewPlayer model.level ]
                        , List.map Board.viewEnemy model.level.enemies
                        ]
                    )
                , Board.viewStats model.level
                , Board.viewGameOver model.level
                    (Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg ExitFreePlayBoard)
                        ]
                        [ Html.text "Select another board" ]
                    )
                , Board.viewAllPointsCollected model.level
                    (Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg ExitFreePlayBoard)
                        ]
                        [ Html.text "Select another board" ]
                    )
                , Html.div
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "padding" "0.5rem"
                    , Html.Attributes.style "top" "0"
                    , Html.Attributes.style "right" "0"
                    ]
                    [ Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg (ShowFreePlayMenu True))
                        ]
                        [ Html.text "Menu"
                        ]
                    ]
                , Html.Extra.modal { open = model.showFreePlayMenu, onClose = toMsg (ShowFreePlayMenu False) }
                    []
                    [ Html.h1
                        [ Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "margin-top" "0"
                        ]
                        [ Html.text "Free Play"
                        , Html.button
                            [ Html.Attributes.style "float" "right"
                            , Html.Attributes.style "background" "none"
                            , Html.Attributes.type_ "button"
                            , Html.Attributes.title "Close"
                            , Html.Events.onClick (toMsg (ShowFreePlayMenu False))
                            ]
                            [ Phosphor.xCircle Phosphor.Regular
                                |> Phosphor.toHtml []
                            ]
                        ]
                    , Html.button
                        [ Html.Attributes.type_ "button"
                        , Html.Events.onClick (toMsg ExitFreePlayBoard)
                        ]
                        [ Html.text "Select another board"
                        ]
                    , Html.br [] []
                    , Html.br [] []
                    , Html.h2 [] [ Html.text "Audio" ]
                    , Html.label
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "align-items" "center"
                        , Html.Attributes.style "gap" "1rem"
                        ]
                        [ Html.span [] [ Html.text "Sound Effects" ]
                        , Html.Extra.range
                            []
                            { step = 0.1
                            , min = 0.0
                            , max = 1.0
                            , value = sharedModel.audioMapping.effects
                            , onInput = Shared.AudioEffectsChanged >> toSharedMsg
                            }
                        ]
                    , Html.h2 [] [ Html.text "Input" ]
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
                            )
                        ]
                    ]
                ]
    ]


viewBoardPreviewTile : (Msg -> msg) -> BoardPreviewTile -> Html msg
viewBoardPreviewTile toMsg board =
    Html.button
        [ Html.Attributes.type_ "button"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "height" "8rem"
        , Html.Events.onClick (toMsg (LoadFreePlayBoard board.boardEncoding))
        ]
        [ Html.text board.name ]


type alias BoardPreviewTile =
    { name : String
    , boardEncoding : String
    }
