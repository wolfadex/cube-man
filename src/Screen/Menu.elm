module Screen.Menu exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Shared


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = NoOp


update : Shared.LoadedModel -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ model =
    ( model, Cmd.none )


view : (Shared.Msg -> msg) -> Shared.LoadedModel -> (Msg -> msg) -> Model -> List (Html msg)
view toSharedMsg sharedModel _ _ =
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
                , Html.Attributes.style "color" "white"
                ]
                [ Html.text "Cube-Man" ]
            , Html.div
                [ Html.Attributes.style "margin-top" "5rem"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-direction" "column"
                , Html.Attributes.style "gap" "1rem"
                ]
                [ Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "padding" "0.5rem 2rem"
                    , Html.Events.onClick (toSharedMsg (Shared.SetScreen Shared.Game))
                    ]
                    [ Html.span [ Html.Attributes.style "text-decoration" "line-through" ] [ Html.text "Play" ]
                    , Html.br [] []
                    , Html.small [] [ Html.text " Coming soon..." ]
                    ]
                , Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "padding" "0.5rem 2rem"
                    , Html.Events.onClick (toSharedMsg (Shared.SetScreen Shared.FreePlay))
                    ]
                    [ Html.text "Free Play" ]
                , Html.button
                    [ Html.Attributes.type_ "button"
                    , Html.Attributes.style "text-align" "center"
                    , Html.Attributes.style "padding" "0.5rem 2rem"
                    , Html.Events.onClick (toSharedMsg (Shared.SetScreen Shared.Editor))
                    ]
                    [ Html.text "Level Editor" ]
                ]
            ]

        -- , let
        --     -- Create a single rectangle from its color and four vertices
        --     -- (Scene3d.quad can be used to create any flat four-sided shape)
        --     square =
        --         Scene3d.quad (Scene3d.Material.color Color.blue)
        --             (Point3d.meters -1 -1 0)
        --             (Point3d.meters 1 -1 0)
        --             (Point3d.meters 1 1 0)
        --             (Point3d.meters -1 1 0)
        --     -- Create a camera using perspective projection
        --     camera =
        --         Camera3d.perspective
        --             { -- Camera is at the point (4, 2, 2), looking at the point
        --               -- (0, 0, 0), oriented so that positive Z appears up
        --               viewpoint =
        --                 Viewpoint3d.lookAt
        --                     { focalPoint = Point3d.origin
        --                     , eyePoint = Point3d.meters 4 2 2
        --                     , upDirection = Direction3d.positiveZ
        --                     }
        --             -- The image on the screen will have a total rendered 'height'
        --             -- of 30 degrees; small angles make the camera act more like a
        --             -- telescope and large numbers make it act more like a fisheye
        --             -- lens
        --             , verticalFieldOfView = Angle.degrees 30
        --             }
        --     ( wallMesh, wallTexture ) =
        --         sharedModel.wallMesh
        --   in
        --   -- Render a scene that doesn't involve any lighting (no lighting is needed
        --   -- here since we provided a material that will result in a constant color
        --   -- no matter what lighting is used)
        --   Scene3d.unlit
        --     { -- Our scene has a single 'entity' in it
        --       entities =
        --         [ square
        --         , Scene3d.meshWithShadow
        --             -- (Scene3d.Material.color Color.gray)
        --             (Scene3d.Material.texturedColor wallTexture)
        --             wallMesh
        --             (Scene3d.Mesh.shadow wallMesh)
        --             |> Scene3d.scaleAbout Point3d.origin 0.5
        --         -- (Scene3d.Mesh.shadow playerMesh)
        --         -- |> Scene3d.translateBy (Vector3d.from Point3d.origin (Frame3d.originPoint frame))
        --         , viewOrientationArrows
        --         ]
        --     -- Provide the camera to be used when rendering the scene
        --     , camera = camera
        --     -- Anything closer than 1 meter to the camera will be clipped away
        --     -- (this is necessary because of the internals of how WebGL works)
        --     , clipDepth = Length.meters 1
        --     -- Using a transparent background means that the HTML underneath the
        --     -- scene will show through
        --     , background = Scene3d.transparentBackground
        --     -- Size in pixels of the generated HTML element
        --     , dimensions = ( Pixels.int 400, Pixels.int 300 )
        --     }
        ]
    ]
