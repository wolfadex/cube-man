module Shared exposing (Model, Msg(..), Screen(..), init, update)

import Board
import Input


type alias Model =
    { screenSize : { width : Int, height : Int }
    , blockPalette : Board.BlockPalette
    , inputMapping : Input.Mapping
    , screen : Screen
    }


type Screen
    = Editor
    | Game
    | FreePlay
    | Menu


init : ( Model, Cmd Msg )
init =
    ( { screenSize = { width = 800, height = 600 }
      , blockPalette = Board.SimpleBlocks
      , inputMapping = Input.defaultMapping
      , screen = Menu
      }
    , Cmd.none
    )


type Msg
    = SetMapping (Input.Mapping -> Input.Mapping)
    | SetScreen Screen
    | SetBlockPalette Board.BlockPalette


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMapping fn ->
            ( { model | inputMapping = fn model.inputMapping }, Cmd.none )

        SetScreen screen ->
            ( { model | screen = screen }, Cmd.none )

        SetBlockPalette blockPalette ->
            ( { model | blockPalette = blockPalette }, Cmd.none )
