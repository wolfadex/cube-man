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
import Serialize
import Shared


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


init : ( Model, Cmd Msg )
init =
    ( { level = Board.emptyLevel
      , boardLoadError = Nothing
      , boardPlayError = Nothing
      , freePlayMode = FreePlayBoardSelection
      , showFreePlayMenu = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.freePlayMode of
        FreePlayBoardSelection ->
            Sub.none

        FreePlayBoardLoaded ->
            if model.showFreePlayMenu then
                Sub.none

            else
                Sub.batch
                    [ Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)
                    , Browser.Events.onKeyDown decodeKeyDown
                    ]


decodeKeyDown : Json.Decode.Decoder Msg
decodeKeyDown =
    Json.Decode.map KeyDown
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
            ( model
                |> tick deltaMs
            , Cmd.none
            )

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
                    handleGameKeyPressed sharedModel key model


tick : Duration -> Model -> Model
tick deltaMs model =
    case model.freePlayMode of
        FreePlayBoardSelection ->
            model

        FreePlayBoardLoaded ->
            { model
                | level =
                    model.level
                        |> Board.tickPlayer deltaMs
                        |> Board.tickEnemies deltaMs
            }


handleGameKeyPressed : Shared.LoadedModel -> String -> Model -> ( Model, Cmd Msg )
handleGameKeyPressed sharedModel key model =
    let
        level =
            model.level
    in
    if Input.isInputKey sharedModel.inputMapping.moveUp key then
        ( { model | level = { level | playerWantFacing = Board.Forward } }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.moveDown key then
        ( { model | level = { level | playerWantFacing = Board.Backward } }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.moveLeft key then
        ( { model | level = { level | playerWantFacing = Board.Left } }, Cmd.none )

    else if Input.isInputKey sharedModel.inputMapping.moveRight key then
        ( { model | level = { level | playerWantFacing = Board.Right } }, Cmd.none )

    else
        ( model, Cmd.none )


view : (Shared.Msg -> msg) -> Shared.LoadedModel -> (Msg -> msg) -> Model -> List (Html msg)
view toSharedMsg sharedModel toMsg model =
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
                        , Html.Events.onClick (toSharedMsg (Shared.SetScreen Shared.Menu))
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
                    (List.map (viewBoardPreviewTile toMsg)
                        [ { name = "Mini"
                          , boardEncoding = Board.basicMiniBoard
                          }
                        , { name = "Zig-Zag"
                          , boardEncoding = Board.zigZagBoard
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
                    , [ Board.viewPlayer model.level.playerFacing model.level.playerFrame ]
                    ]
                )
            , Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "padding" "0.5rem"
                , Html.Attributes.style "top" "0"
                , Html.Attributes.style "left" "0"
                ]
                [ Html.h3 [] [ Html.text ("Score: " ++ String.fromInt model.level.score) ]
                ]
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
                [ Html.h2
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
