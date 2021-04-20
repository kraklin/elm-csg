module Scene exposing (main)

{-| This example shows how you can allow orbiting of a scene by listening for
mouse events and moving the camera accordingly.
-}

import Angle exposing (Angle)
import Browser
import Browser.Events
import BspTree
import Camera3d
import Color
import Csg
import Direction3d
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Length
import Pixels exposing (Pixels)
import Plane3d
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import Triangle3d
import Vector3d
import Viewpoint3d


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { azimuth : Angle -- Orbiting angle of the camera around the focal point
    , elevation : Angle -- Angle of the camera up from the XY plane
    , orbiting : Bool -- Whether the mouse button is currently down
    , mesh1 : Mesh.Plain WorldCoordinates -- Saved Mesh values for rendering
    , mesh2 : Mesh.Plain WorldCoordinates
    , clipPlanePosition : Float
    }


type Msg
    = MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | PlanePositionChanged String


init : () -> ( Model, Cmd Msg )
init () =
    -- Create a couple of Mesh values containing a single triangle each and
    -- store them in the model
    let
        mesh1 =
            Mesh.triangles
                [ Triangle3d.from
                    (Point3d.meters 0 0 0)
                    (Point3d.meters 1 0 0)
                    (Point3d.meters 1 1 0)
                ]

        mesh2 =
            Mesh.triangles
                [ Triangle3d.from
                    (Point3d.meters 0 0 0)
                    (Point3d.meters 1 1 0)
                    (Point3d.meters 0 1 0)
                ]
    in
    ( { azimuth = Angle.degrees 45
      , elevation = Angle.degrees 30
      , orbiting = False
      , clipPlanePosition = -0.5
      , mesh1 = mesh1
      , mesh2 = mesh2
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        -- Start orbiting when a mouse button is pressed
        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        -- Stop orbiting when a mouse button is released
        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        -- Orbit camera on mouse move (if a mouse button is down)
        MouseMove dx dy ->
            if model.orbiting then
                let
                    -- How fast we want to orbit the camera (orbiting the
                    -- camera by 1 degree per pixel of drag is a decent default
                    -- to start with)
                    rotationRate =
                        Angle.degrees 1 |> Quantity.per Pixels.pixel

                    -- Adjust azimuth based on horizontal mouse motion (one
                    -- degree per pixel)
                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    -- Adjust elevation based on vertical mouse motion (one
                    -- degree per pixel), and clamp to make sure camera cannot
                    -- go past vertical in either direction
                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model | azimuth = newAzimuth, elevation = newElevation }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        PlanePositionChanged input ->
            ( { model | clipPlanePosition = String.toFloat input |> Maybe.withDefault 0 }, Cmd.none )


{-| Use movementX and movementY for simplicity (don't need to store initial
mouse position in the model) - not supported in Internet Explorer though
-}
decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.orbiting then
        -- If we're currently orbiting, listen for mouse moves and mouse button
        -- up events (to stop orbiting); in a real app we'd probably also want
        -- to listen for page visibility changes to stop orbiting if the user
        -- switches to a different tab or something
        Sub.batch
            [ Browser.Events.onMouseMove decodeMouseMove
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            ]

    else
        -- If we're not currently orbiting, just listen for mouse down events
        -- to start orbiting
        Browser.Events.onMouseDown (Decode.succeed MouseDown)


renderCsg trianglesList =
    trianglesList
        |> List.map
            (\( v1, v2, v3 ) ->
                Triangle3d.from v1.position v2.position v3.position
                    |> Scene3d.facet (Material.matte v1.color)
            )
        |> Scene3d.group


cube1 =
    Csg.cube (Length.meters 1)
        |> Csg.translate (Vector3d.meters -0.5 -0.5 0.5)
        |> Csg.withColor Color.blue


cube2 =
    Csg.cube (Length.meters 1)
        |> Csg.withColor Color.red


split1 =
    Csg.subtract sphere cube2



-- |> Csg.withColor Color.blue


split2 =
    Csg.intersect cube1 sphere
        |> Csg.intersect cube2
        |> Csg.toMesh
        |> renderCsg


union =
    Csg.intersect cube1 cube2


pyramid =
    Csg.pyramid


sphere =
    Csg.sphere
        |> Csg.withColor Color.green


view : Model -> Browser.Document Msg
view model =
    let
        -- Create a viewpoint by orbiting around a Z axis through the given
        -- focal point, with azimuth measured from the positive X direction
        -- towards positive Y
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.meters 0.5 0.5 0
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 5
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                }

        --Csg.sphere
    in
    { title = "OrbitingCamera"
    , body =
        [ Scene3d.cloudy
            { camera = camera
            , clipDepth = Length.meters 0.1
            , dimensions = ( Pixels.int 400, Pixels.int 300 )
            , background = Scene3d.transparentBackground
            , entities =
                [ originCross

                --, renderCsg <| Csg.toMesh pyramid
                {--
                , cube1
                    |> Csg.toLines
                    |> Mesh.lineSegments
                    |> Scene3d.mesh (Material.color Color.purple)

                --}
                {--
                , cube2
                    |> Csg.toLines
                    |> Mesh.lineSegments
                    |> Scene3d.mesh (Material.color Color.red)

                --}
                --, renderCsg <| Csg.toMesh sphere
                {--, sphere
                    |> Csg.toLines
                    |> Mesh.lineSegments
                    |> Scene3d.mesh (Material.color Color.red)

                --}
                {--
                , renderCsg <| Csg.toMesh split1

                --}
                --{--
                , split1
                    |> Csg.toLines
                    |> Mesh.lineSegments
                    |> Scene3d.mesh (Material.color Color.green)

                ---}
                --, Scene3d.mesh (Material.color Color.blue) clippedCubeBottom --cubesWireframe
                ]
            , upDirection = Direction3d.positiveZ
            }
        , Html.input
            [ Attrs.value (model.clipPlanePosition |> String.fromFloat)
            , Attrs.min "-1.1"
            , Attrs.max "1.1"
            , Attrs.type_ "number"
            , Attrs.step "0.1"
            , Events.onInput PlanePositionChanged
            ]
            []
        ]
    }


originCross =
    let
        radius =
            Pixels.pixels 4

        distance =
            0.2
    in
    Scene3d.group
        [ Scene3d.point { radius = radius } (Material.color Color.black) Point3d.origin
        , Scene3d.point { radius = radius } (Material.color Color.red) (Point3d.meters distance 0 0)
        , Scene3d.point { radius = radius } (Material.color Color.green) (Point3d.meters 0 distance 0)
        , Scene3d.point { radius = radius } (Material.color Color.blue) (Point3d.meters 0 0 distance)
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
