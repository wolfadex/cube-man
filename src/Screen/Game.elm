module Screen.Game exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Board exposing (Board)
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Length
import Shared


type alias Model =
    { board : Board
    , score : Int
    , playerFrame : Frame3d Length.Meters Board.WorldCoordinates { defines : Board.WorldCoordinates }
    , playerFacing : Board.Facing
    , playerWantFacing : Board.Facing
    , playerTarget : Board.Target
    }


init : ( Model, Cmd Msg )
init =
    ( { board = Board.empty
      , score = 0
      , playerFrame = Frame3d.atOrigin
      , playerFacing = Board.Forward
      , playerWantFacing = Board.Forward
      , playerTarget = Board.initTarget Board.empty Board.Forward Frame3d.atOrigin
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = NoOp


update : Shared.LoadedModel -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ model =
    ( model, Cmd.none )


view : (Shared.Msg -> msg) -> Shared.LoadedModel -> (Msg -> msg) -> Model -> List (Html msg)
view toSharedMsg _ _ _ =
    [ Html.div
        [ Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "display" "grid"
        , Html.Attributes.style "grid-template-columns" "auto auto auto"
        ]
        [ Html.div
            [ Html.Attributes.style "grid-column" "2"
            , Html.Attributes.style "margin-top" "5rem"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "gap" "1rem"
            , Html.Attributes.style "align-items" "center"
            ]
            [ Html.h1
                [ Html.Attributes.style "text-align" "center"
                ]
                [ Html.text "Cube-Man" ]
            , Html.h2
                [ Html.Attributes.style "text-align" "center"
                ]
                [ Html.text "Full game coming soon..." ]
            , Html.button
                [ Html.Attributes.type_ "button"
                , Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "padding" "0.5rem 2rem"
                , Html.Events.onClick (toSharedMsg (Shared.SetScreen Shared.Menu))
                ]
                [ Html.text "Main Menu" ]
            ]
        ]
    ]
