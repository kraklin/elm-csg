module Scene exposing (main)

{-| This example shows how you can allow orbiting of a scene by listening for
mouse events and moving the camera accordingly.
-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Browser
import Browser.Events
import Camera3d
import Color
import Csg
import Csg.Shape3d as CsgShape
import Direction3d
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import LineSegment3d exposing (LineSegment3d)
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
    , clipPlanePosition : Float
    , mesh : Scene3d.Entity WorldCoordinates
    , showAxis : Bool
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
    ( { azimuth = Angle.degrees -60
      , elevation = Angle.degrees 30
      , orbiting = False
      , clipPlanePosition = -0.5
      , mesh =
            allShapes
                |> Csg.toTriangularMeshGroupedByColor
                |> List.map
                    (\( mesh, color ) ->
                        mesh
                            |> Mesh.indexedFaces
                            |> Scene3d.mesh (Material.metal { baseColor = color, roughness = 0 })
                    )
                |> Scene3d.group
      , showAxis = True
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


cube =
    --Csg.sphere (Length.meters 0.7)
    CsgShape.cube (Length.meters 1)
        |> CsgShape.translateBy (Vector3d.meters -0.5 -0.5 -0.5)
        |> CsgShape.withColor Color.red


cube2 =
    CsgShape.cube (Length.meters 1)
        |> CsgShape.withColor Color.red


cylinderY =
    CsgShape.cylinderFromTo (Length.centimeters 40) (Point3d.meters 0 -1 0) (Point3d.meters 0 1 0)
        |> CsgShape.withColor Color.purple


cylinderX =
    CsgShape.cylinderFromTo (Length.centimeters 40) (Point3d.meters -1 0 0) (Point3d.meters 1 0 0)
        |> CsgShape.withColor Color.purple


cylinderZ =
    CsgShape.cylinderFromTo (Length.centimeters 40) (Point3d.meters 0 0 -1) (Point3d.meters 0 0 1)


finalCsg =
    let
        cylinders =
            cylinderX
                |> CsgShape.unionWith cylinderY
                |> CsgShape.unionWith cylinderZ
                |> CsgShape.withColor Color.green
    in
    cylinders
        |> CsgShape.subtractFrom
            (cube
                |> CsgShape.intersectWith sphere
            )


allShapes =
    let
        smallCube =
            CsgShape.cube (Length.centimeters 10)
                |> CsgShape.withColor Color.red

        cuboid =
            CsgShape.cuboid
                { width = Length.centimeters 10
                , depth = Length.centimeters 20
                , height = Length.centimeters 30
                }
                |> CsgShape.withColor Color.blue

        cone =
            CsgShape.cone (Length.centimeters 5) (Length.centimeters 20)
                |> CsgShape.withColor Color.yellow

        coneWith =
            CsgShape.coneWith
                { slices = 6
                , bottomRadius = Length.centimeters 7
                , topRadius = Length.centimeters 3
                , bottomPoint = Point3d.origin
                , topPoint = Point3d.xyz Quantity.zero Quantity.zero (Length.centimeters 20)
                }
                |> CsgShape.withColor Color.darkGreen

        cylinder =
            CsgShape.cylinder (Length.centimeters 5) (Length.centimeters 20)
                |> CsgShape.withColor Color.green

        smallSphere =
            CsgShape.sphere (Length.centimeters 5)
                |> CsgShape.withColor Color.purple
    in
    CsgShape.group
        [ smallCube
        , cuboid |> CsgShape.moveBackward (Length.centimeters 15)
        , cone
            |> CsgShape.moveRight (Length.centimeters 20)
            |> CsgShape.moveBackward (Length.centimeters 5)
        , cylinder
            |> CsgShape.moveRight (Length.centimeters 20)
            |> CsgShape.moveBackward (Length.centimeters 25)
        , coneWith
            |> CsgShape.moveRight (Length.centimeters 20)
            |> CsgShape.moveBackward (Length.centimeters 47)
        , smallSphere
            |> CsgShape.moveLeft (Length.centimeters 10)
            |> CsgShape.moveBackward (Length.centimeters 5)
            |> CsgShape.moveUp (Length.centimeters 5)
        ]


finalCsg4 =
    CsgShape.cube (Length.meters 1)
        |> CsgShape.scaleBy (Vector3d.meters 0.4 1 1)
        |> CsgShape.rotateAround Axis3d.y (Angle.degrees -45)
        |> CsgShape.translateBy (Vector3d.meters 0 0 0.7)
        |> CsgShape.scaleBy (Vector3d.meters 2 1 1)
        |> CsgShape.rotateAround Axis3d.y (Angle.degrees 22.5)
        |> CsgShape.subtractFrom
            (CsgShape.sphere (Length.meters 1)
                |> CsgShape.scaleBy (Vector3d.meters 2 1 1)
            )
        |> CsgShape.scaleBy (Vector3d.meters 0.5 1 1)


finalCsg5 =
    let
        dotRadius =
            Length.centimeters 8

        cubeSize =
            Length.meters 1

        dotAt x y =
            CsgShape.sphereWith { slices = 8, stacks = 4, radius = dotRadius }
                |> CsgShape.translateBy
                    (Vector3d.meters
                        (0.3 + (x * 0.2))
                        (0.3 + (y * 0.2))
                        0
                    )

        one =
            dotAt 1 1

        two =
            CsgShape.group
                [ dotAt 2 2
                , dotAt 0 0
                ]

        three =
            CsgShape.group
                [ one
                , two
                ]

        four =
            CsgShape.group
                [ dotAt 0 2
                , dotAt 2 0
                , two
                ]

        five =
            CsgShape.group
                [ four
                , one
                ]

        six =
            CsgShape.group
                [ four
                , dotAt 1 2
                , dotAt 1 0
                ]

        base =
            CsgShape.cube cubeSize
                |> CsgShape.translateBy (Vector3d.meters -0.5 -0.5 0.5)
                |> CsgShape.intersectWith sphere
                |> CsgShape.translateBy (Vector3d.meters 0.5 0.5 -0.5)
                |> CsgShape.withColor Color.red

        dots =
            CsgShape.group
                [ one
                , six |> CsgShape.translateBy (Vector3d.meters 0 0 -1)
                , two |> CsgShape.rotateAround Axis3d.x (Angle.degrees -90)
                , five
                    |> CsgShape.rotateAround Axis3d.x (Angle.degrees -90)
                    |> CsgShape.translateBy (Vector3d.meters 0 1 0)
                , three
                    |> CsgShape.rotateAround Axis3d.y (Angle.degrees -90)
                    |> CsgShape.rotateAround Axis3d.x (Angle.degrees -90)
                , four
                    |> CsgShape.rotateAround Axis3d.y (Angle.degrees -90)
                    |> CsgShape.rotateAround Axis3d.x (Angle.degrees -90)
                    |> CsgShape.translateBy (Vector3d.meters 1 0 0)
                ]
                |> CsgShape.withColor Color.white
    in
    dots
        |> CsgShape.subtractFrom base
        |> CsgShape.translateBy (Vector3d.meters -0.5 -0.5 0.5)
        |> CsgShape.scaleBy (Vector3d.meters 2 1 1)
        |> CsgShape.rotateAround Axis3d.x (Angle.degrees -45)


finalCsg7 =
    --|> Csg.scaleBy (Vector3d.meters 1.5 0.5 1)
    (CsgShape.cube (Length.meters 1)
        |> CsgShape.translateBy (Vector3d.meters -0.5 0 0.5)
    )
        |> CsgShape.unionWith cube


finalCsg6 =
    let
        cone =
            CsgShape.cone (Length.meters 0.5) (Length.meters 0.5)

        twoCones =
            CsgShape.group
                [ cone
                , cone
                    |> CsgShape.rotateAround Axis3d.x (Angle.degrees 180)
                ]

        halfCones =
            (CsgShape.cube (Length.meters 1)
                |> CsgShape.translateBy (Vector3d.meters -0.5 0 0.5)
            )
                |> CsgShape.subtractFrom
                    twoCones

        sphericon =
            CsgShape.group
                [ halfCones
                , halfCones
                    |> CsgShape.rotateAround Axis3d.z (Angle.degrees 180)
                    |> CsgShape.rotateAround Axis3d.y (Angle.degrees 90)
                ]
                |> CsgShape.withColor Color.orange
    in
    CsgShape.group
        [ sphericon
        , sphericon
            |> CsgShape.scaleBy (Vector3d.meters 1.2 1 0.5)
            |> CsgShape.translateBy (Vector3d.meters 1.2 0 0)
            |> CsgShape.withColor Color.green
        ]



{-
   |> Csg.toLines
   |> Mesh.lineSegments
   |> Scene3d.mesh (Material.color Color.green)
-}


trianglesCount csg =
    csg |> Csg.toTriangles |> List.length


sphere =
    CsgShape.sphere (Length.centimeters 70)
        |> CsgShape.withColor Color.blue


grid =
    let
        stepMajor =
            Length.meters 1

        lengthMajor =
            Length.centimeters 25

        stepMinor =
            Length.centimeters 20

        lengthMinor =
            Length.centimeters 10

        drawTo =
            Length.meters 20

        stepsMajor =
            Quantity.range
                { start = stepMajor
                , end = drawTo
                , steps = floor (Length.inMeters drawTo / Length.inMeters stepMajor) - 1
                }

        stepsMinor =
            Quantity.range
                { start = stepMinor
                , end = drawTo
                , steps = floor (Length.inMeters drawTo / Length.inMeters stepMinor) - 1
                }

        scaleX length steps numbers =
            steps
                |> List.indexedMap
                    (\i step_ ->
                        [ LineSegment3d.from (Point3d.xyz step_ Quantity.zero Quantity.zero)
                            (Point3d.xyz step_ (Quantity.negate length) Quantity.zero)
                        , LineSegment3d.from (Point3d.xyz (Quantity.negate step_) Quantity.zero Quantity.zero)
                            (Point3d.xyz (Quantity.negate step_) (Quantity.negate length) Quantity.zero)
                        ]
                            ++ (if numbers then
                                    [ (numberToLineSegments <| ceiling <| Length.inCentimeters step_)
                                        |> List.map (LineSegment3d.translateIn Direction3d.x step_)
                                        |> List.map (LineSegment3d.translateIn Direction3d.y (Length.centimeters 12))
                                    , (numberToLineSegments <| negate <| ceiling <| Length.inCentimeters step_)
                                        |> List.map (LineSegment3d.translateIn Direction3d.negativeX step_)
                                        |> List.map (LineSegment3d.translateIn Direction3d.y (Length.centimeters 12))
                                    ]
                                        |> List.concat

                                else
                                    []
                               )
                    )
                |> List.concat

        scaleY length steps numbers =
            scaleX length steps numbers
                |> List.map (\segment -> LineSegment3d.rotateAround Axis3d.z (Angle.degrees 90) segment)

        scaleZ length steps numbers =
            scaleX length steps numbers
                |> List.map
                    (\segment ->
                        LineSegment3d.rotateAround Axis3d.z (Angle.degrees -90) segment
                            |> LineSegment3d.rotateAround Axis3d.x (Angle.degrees -90)
                            |> LineSegment3d.mirrorAcross Plane3d.yz
                    )

        scales =
            [ scaleX lengthMajor stepsMajor True
            , scaleX lengthMinor stepsMinor False
            , scaleY lengthMajor stepsMajor True
            , scaleY lengthMinor stepsMinor False
            , scaleZ lengthMajor stepsMajor True
            , scaleZ lengthMinor stepsMinor False
            ]
                |> List.concat
    in
    scales
        ++ [ LineSegment3d.along Axis3d.x (Quantity.negate drawTo) drawTo
           , LineSegment3d.along Axis3d.y (Quantity.negate drawTo) drawTo
           , LineSegment3d.along Axis3d.z (Quantity.negate drawTo) drawTo
           ]
        |> Mesh.lineSegments
        |> Scene3d.mesh (Material.color Color.black)


segmentDigit : Char -> List (LineSegment3d Meters c)
segmentDigit char =
    let
        width =
            Length.centimeters 4

        heightTop =
            Length.centimeters 4

        heightBottom =
            Length.centimeters 5

        height =
            Quantity.plus heightTop heightBottom

        s1 =
            LineSegment3d.from
                (Point3d.xyz Quantity.zero Quantity.zero Quantity.zero)
                (Point3d.xyz Quantity.zero heightBottom Quantity.zero)

        s2 =
            LineSegment3d.from
                (Point3d.xyz Quantity.zero heightBottom Quantity.zero)
                (Point3d.xyz Quantity.zero height Quantity.zero)

        s3 =
            LineSegment3d.from
                (Point3d.xyz Quantity.zero height Quantity.zero)
                (Point3d.xyz width height Quantity.zero)

        s4 =
            LineSegment3d.from
                (Point3d.xyz width heightBottom Quantity.zero)
                (Point3d.xyz width height Quantity.zero)

        s5 =
            LineSegment3d.from
                (Point3d.xyz width heightBottom Quantity.zero)
                (Point3d.xyz width Quantity.zero Quantity.zero)

        s7 =
            LineSegment3d.from
                (Point3d.xyz Quantity.zero heightBottom Quantity.zero)
                (Point3d.xyz width heightBottom Quantity.zero)

        s6 =
            LineSegment3d.from
                (Point3d.xyz Quantity.zero Quantity.zero Quantity.zero)
                (Point3d.xyz width Quantity.zero Quantity.zero)
    in
    case char of
        '1' ->
            [ s4, s5 ]

        '2' ->
            [ s1, s3, s4, s6, s7 ]

        '3' ->
            [ s3, s4, s5, s6, s7 ]

        '4' ->
            [ s2, s4, s5, s7 ]

        '5' ->
            [ s2, s3, s6, s5, s7 ]

        '6' ->
            [ s1, s2, s3, s5, s6, s7 ]

        '7' ->
            [ s3, s4, s5 ]

        '8' ->
            [ s1, s2, s3, s4, s5, s6, s7 ]

        '9' ->
            [ s2, s3, s4, s5, s7 ]

        '0' ->
            [ s1, s2, s3, s4, s5, s6 ]

        '-' ->
            [ s7 ]

        _ ->
            []


numberToLineSegments : Int -> List (LineSegment3d Meters c)
numberToLineSegments num =
    let
        center list =
            list
                |> List.map
                    (List.map
                        (LineSegment3d.translateIn
                            Direction3d.negativeX
                            (Length.centimeters 6
                                |> Quantity.multiplyBy (toFloat (List.length list - 1))
                                |> Quantity.half
                            )
                        )
                    )
    in
    String.fromInt num
        |> String.toList
        |> List.indexedMap
            (\i char ->
                segmentDigit char
                    |> List.map
                        (LineSegment3d.translateIn
                            Direction3d.x
                            (Quantity.multiplyBy (toFloat i) (Length.centimeters 6))
                        )
            )
        |> center
        |> List.concat


finalCsgMesh =
    finalCsg
        |> Csg.toTriangularMeshGroupedByColor
        |> List.map
            (\( mesh, color ) ->
                mesh
                    |> Mesh.indexedFaces
                    |> Scene3d.mesh (Material.metal { baseColor = color, roughness = 0 })
            )
        |> Scene3d.group
--}



{-
   , finalCsg
       |> Csg.toLines
       |> Mesh.lineSegments
       |> Scene3d.mesh (Material.color Color.black)
-}


view : Model -> Browser.Document Msg
view model =
    let
        -- Create a viewpoint by orbiting around a Z axis through the given
        -- focal point, with azimuth measured from the positive X direction
        -- towards positive Y
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.meters 0 0 0
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 7
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                }

        viewpointMesh =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.meters 0 0 0
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 2
                }

        cameraMesh =
            Camera3d.perspective
                { viewpoint = viewpointMesh
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    { title = "Elm CSG"
    , body =
        [ Html.div
            [ Attrs.style "position" "absolute"
            , Attrs.style "top" "730px"
            , Attrs.style "display" "flex"
            , Attrs.style "width" "100%"
            , Attrs.style "justify-content" "space-between"
            , Attrs.style "align-items" "flex-end"
            ]
            [ Scene3d.sunny
                { camera = camera
                , clipDepth = Length.meters 0.1
                , dimensions = ( Pixels.int 70, Pixels.int 70 )
                , background = Scene3d.transparentBackground
                , entities = [ originCross ]
                , upDirection = Direction3d.positiveZ
                , shadows = True
                , sunlightDirection = Direction3d.positiveZ
                }
            , Html.span
                [ Attrs.style "padding-right" "24px"
                , Attrs.style "user-select" "none"
                ]
                [ Html.text <| "tri count: " ++ (String.fromInt <| trianglesCount allShapes) ]
            ]
        , Html.div [ Attrs.style "background-color" "rgba(255, 255, 0, 0.2)" ]
            [ Scene3d.cloudy
                { camera = cameraMesh
                , clipDepth = Length.meters 0.1
                , dimensions = ( Pixels.int 800, Pixels.int 800 )
                , background = Scene3d.transparentBackground
                , entities =
                    (if model.showAxis then
                        [ grid ]

                     else
                        []
                    )
                        ++ [ model.mesh ]
                , upDirection = Direction3d.positiveZ
                }
            ]
        ]
    }


originCross =
    let
        radius =
            Pixels.pixels 4

        distance =
            1
    in
    Scene3d.group
        [ Scene3d.point { radius = radius } (Material.color Color.black) Point3d.origin
        , Scene3d.point { radius = radius } (Material.color Color.red) (Point3d.meters distance 0 0)
        , Scene3d.point { radius = radius } (Material.color Color.green) (Point3d.meters 0 distance 0)
        , Scene3d.point { radius = radius } (Material.color Color.blue) (Point3d.meters 0 0 distance)
        , LineSegment3d.along Axis3d.x (Length.meters 0) (Length.meters distance)
            |> Scene3d.lineSegment (Material.color Color.red)
        , LineSegment3d.along Axis3d.y (Length.meters 0) (Length.meters distance)
            |> Scene3d.lineSegment (Material.color Color.green)
        , LineSegment3d.along Axis3d.z (Length.meters 0) (Length.meters distance)
            |> Scene3d.lineSegment (Material.color Color.blue)
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
