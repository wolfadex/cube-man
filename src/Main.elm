module Main exposing (main)

import Browser
import Html
import Screen.Editor
import Screen.FreePlay
import Screen.Game
import Screen.Menu
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
    , menuModel : Screen.Menu.Model
    , gameModel : Screen.Game.Model
    , freePlayModel : Screen.FreePlay.Model
    , editorModel : Screen.Editor.Model
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        ( sharedModel, sharedCmd ) =
            Shared.init

        ( menuModel, menuCmd ) =
            Screen.Menu.init

        ( gameModel, gameCmd ) =
            Screen.Game.init

        ( freePlayModel, freePlayCmd ) =
            Screen.FreePlay.init

        ( editorModel, editorCmd ) =
            Screen.Editor.init
    in
    ( { sharedModel = sharedModel
      , menuModel = menuModel
      , gameModel = gameModel
      , freePlayModel = freePlayModel
      , editorModel = editorModel
      }
    , Cmd.batch
        [ Cmd.map SharedMsg sharedCmd
        , Cmd.map MenuMsg menuCmd
        , Cmd.map GameMsg gameCmd
        , Cmd.map FreePlayMsg freePlayCmd
        , Cmd.map EditorMsg editorCmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.sharedModel of
        Shared.Loaded sharedModel ->
            case sharedModel.screen of
                Shared.Menu ->
                    Screen.Menu.subscriptions model.menuModel
                        |> Sub.map MenuMsg

                Shared.Game ->
                    Screen.Game.subscriptions model.gameModel
                        |> Sub.map GameMsg

                Shared.FreePlay ->
                    Screen.FreePlay.subscriptions model.freePlayModel
                        |> Sub.map FreePlayMsg

                Shared.Editor ->
                    Screen.Editor.subscriptions model.editorModel
                        |> Sub.map EditorMsg

        _ ->
            Sub.none


type Msg
    = SharedMsg Shared.Msg
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

        MenuMsg message ->
            case model.sharedModel of
                Shared.Loaded sharedModel ->
                    let
                        ( menuModel, menuCmd ) =
                            Screen.Menu.update sharedModel message model.menuModel
                    in
                    ( { model | menuModel = menuModel }, Cmd.map MenuMsg menuCmd )

                _ ->
                    ( model, Cmd.none )

        GameMsg message ->
            case model.sharedModel of
                Shared.Loaded sharedModel ->
                    let
                        ( gameModel, gameCmd ) =
                            Screen.Game.update sharedModel message model.gameModel
                    in
                    ( { model | gameModel = gameModel }, Cmd.map GameMsg gameCmd )

                _ ->
                    ( model, Cmd.none )

        FreePlayMsg message ->
            case model.sharedModel of
                Shared.Loaded sharedModel ->
                    let
                        ( freePlayModel, freePlayCmd ) =
                            Screen.FreePlay.update sharedModel message model.freePlayModel
                    in
                    ( { model | freePlayModel = freePlayModel }, Cmd.map FreePlayMsg freePlayCmd )

                _ ->
                    ( model, Cmd.none )

        EditorMsg message ->
            case model.sharedModel of
                Shared.Loaded sharedModel ->
                    let
                        ( editorModel, editorCmd ) =
                            Screen.Editor.update
                                SharedMsg
                                sharedModel
                                EditorMsg
                                message
                                model.editorModel
                    in
                    ( { model | editorModel = editorModel }, editorCmd )

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
                case sharedModel.screen of
                    Shared.Menu ->
                        Screen.Menu.view SharedMsg sharedModel MenuMsg model.menuModel

                    Shared.Game ->
                        Screen.Game.view SharedMsg sharedModel GameMsg model.gameModel

                    Shared.FreePlay ->
                        Screen.FreePlay.view SharedMsg sharedModel FreePlayMsg model.freePlayModel

                    Shared.Editor ->
                        Screen.Editor.view SharedMsg sharedModel EditorMsg model.editorModel
    }
