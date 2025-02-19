module Screen.Model exposing (Screen(..))

import Screen.Editor
import Screen.FreePlay
import Screen.Game
import Screen.Menu


type Screen
    = Menu Screen.Menu.Model
    | Game Screen.Game.Model
    | FreePlay Screen.FreePlay.Model
    | Editor Screen.Editor.Model
