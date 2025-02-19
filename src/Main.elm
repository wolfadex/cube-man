module Main exposing (main)

import Browser
import Html
import Screen exposing (Screen)
import Screen.Editor
import Screen.FreePlay
import Screen.Game
import Screen.Menu
import Screen.Model
import Shared


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { sharedModel : Shared.Model
    , screen : Screen.Model.Screen
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        ( sharedModel, sharedCmd ) =
            Shared.init

        ( menuModel, menuCmd ) =
            Screen.Menu.init
    in
    ( { sharedModel = sharedModel
      , screen = Screen.Model.Menu menuModel
      }
    , Cmd.batch
        [ Cmd.map SharedMsg sharedCmd
        , Cmd.map MenuMsg menuCmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.sharedModel of
        Shared.Loaded sharedModel ->
            case model.screen of
                Screen.Model.Menu menuModel ->
                    Screen.Menu.subscriptions menuModel
                        |> Sub.map MenuMsg

                Screen.Model.Game gameModel ->
                    Screen.Game.subscriptions gameModel
                        |> Sub.map GameMsg

                Screen.Model.FreePlay freePlayModel ->
                    Screen.FreePlay.subscriptions
                        { toSharedMsg = SharedMsg
                        , toMsg = FreePlayMsg
                        }
                        freePlayModel

                Screen.Model.Editor editorModel ->
                    Screen.Editor.subscriptions editorModel
                        |> Sub.map EditorMsg

        _ ->
            Sub.none


type Msg
    = SharedMsg Shared.Msg
    | SetScreen Screen
    | MenuMsg Screen.Menu.Msg
    | GameMsg Screen.Game.Msg
    | FreePlayMsg Screen.FreePlay.Msg
    | EditorMsg Screen.Editor.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SharedMsg message ->
            let
                ( sharedModel, sharedCmd ) =
                    Shared.update message model.sharedModel
            in
            ( { model | sharedModel = sharedModel }, Cmd.map SharedMsg sharedCmd )

        SetScreen screen ->
            case screen of
                Screen.Menu ->
                    case model.screen of
                        Screen.Model.Menu _ ->
                            ( model, Cmd.none )

                        _ ->
                            let
                                ( menuModel, menuCmd ) =
                                    Screen.Menu.init
                            in
                            ( { model | screen = Screen.Model.Menu menuModel }, Cmd.map MenuMsg menuCmd )

                Screen.Game ->
                    case model.screen of
                        Screen.Model.Game _ ->
                            ( model, Cmd.none )

                        _ ->
                            let
                                ( gameModel, gameCmd ) =
                                    Screen.Game.init
                            in
                            ( { model | screen = Screen.Model.Game gameModel }, Cmd.map GameMsg gameCmd )

                Screen.Editor ->
                    case model.screen of
                        Screen.Model.Editor _ ->
                            ( model, Cmd.none )

                        _ ->
                            let
                                ( editorModel, editorCmd ) =
                                    Screen.Editor.init
                            in
                            ( { model | screen = Screen.Model.Editor editorModel }, Cmd.map EditorMsg editorCmd )

                Screen.FreePlay ->
                    case model.screen of
                        Screen.Model.FreePlay _ ->
                            ( model, Cmd.none )

                        _ ->
                            let
                                ( freePlayModel, freePlayCmd ) =
                                    Screen.FreePlay.init { toSharedMsg = SharedMsg, toMsg = FreePlayMsg }
                            in
                            ( { model | screen = Screen.Model.FreePlay freePlayModel }, freePlayCmd )

        MenuMsg message ->
            case model.sharedModel of
                Shared.Loaded sharedModel ->
                    case model.screen of
                        Screen.Model.Menu menuModel ->
                            let
                                ( nextMenuModel, menuCmd ) =
                                    Screen.Menu.update sharedModel message menuModel
                            in
                            ( { model | screen = Screen.Model.Menu nextMenuModel }, Cmd.map MenuMsg menuCmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GameMsg message ->
            case model.sharedModel of
                Shared.Loaded sharedModel ->
                    case model.screen of
                        Screen.Model.Game gameModel ->
                            let
                                ( nextGameModel, gameCmd ) =
                                    Screen.Game.update sharedModel message gameModel
                            in
                            ( { model | screen = Screen.Model.Game nextGameModel }, Cmd.map GameMsg gameCmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FreePlayMsg message ->
            case model.sharedModel of
                Shared.Loaded sharedModel ->
                    case model.screen of
                        Screen.Model.FreePlay freePlayModel ->
                            let
                                ( nextFreePlayModel, freePlayCmd ) =
                                    Screen.FreePlay.update sharedModel message freePlayModel
                            in
                            ( { model | screen = Screen.Model.FreePlay nextFreePlayModel }, Cmd.map FreePlayMsg freePlayCmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditorMsg message ->
            case model.sharedModel of
                Shared.Loaded sharedModel ->
                    case model.screen of
                        Screen.Model.Editor editorModel ->
                            let
                                ( nextEditorModel, editorCmd ) =
                                    Screen.Editor.update
                                        SharedMsg
                                        sharedModel
                                        EditorMsg
                                        message
                                        editorModel
                            in
                            ( { model | screen = Screen.Model.Editor nextEditorModel }, editorCmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Cube-Dude"
    , body =
        case model.sharedModel of
            Shared.Loading _ ->
                [ Html.text "Loading" ]

            Shared.Failed _ ->
                [ Html.text "Failed to load" ]

            Shared.Loaded sharedModel ->
                case model.screen of
                    Screen.Model.Menu menuModel ->
                        Screen.Menu.view
                            { setScreen = SetScreen
                            , toSharedMsg = SharedMsg
                            , sharedModel = sharedModel
                            , toMsg = MenuMsg
                            , model = menuModel
                            }

                    Screen.Model.Game gameModel ->
                        Screen.Game.view
                            { setScreen = SetScreen
                            , toSharedMsg = SharedMsg
                            , sharedModel = sharedModel
                            , toMsg = GameMsg
                            , model = gameModel
                            }

                    Screen.Model.FreePlay freePlayModel ->
                        Screen.FreePlay.view
                            { setScreen = SetScreen
                            , toSharedMsg = SharedMsg
                            , sharedModel = sharedModel
                            , toMsg = FreePlayMsg
                            , model = freePlayModel
                            }

                    Screen.Model.Editor editorModel ->
                        Screen.Editor.view
                            { setScreen = SetScreen
                            , toSharedMsg = SharedMsg
                            , sharedModel = sharedModel
                            , toMsg = EditorMsg
                            , model = editorModel
                            }
    }
