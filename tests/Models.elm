module Models exposing
    ( allShapes
    , debug
    , dice
    , eightPawns
    , pawn
    , simpleTransformations
    , sphericon
    , torus
    , transformationsCube
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Color
import Csg
import Csg.Shape3d as CsgShape
import Length exposing (Meters)
import LineSegment3d exposing (LineSegment3d)
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Vector3d


allShapes =
    let
        smallCube =
            CsgShape.cube (Length.centimeters 10)
                |> CsgShape.withTag Color.red

        cuboid =
            CsgShape.cuboid
                { width = Length.centimeters 10
                , depth = Length.centimeters 20
                , height = Length.centimeters 30
                }
                |> CsgShape.withTag Color.blue

        cone =
            CsgShape.cone (Length.centimeters 5) (Length.centimeters 20)
                |> CsgShape.withTag Color.yellow

        coneWith =
            CsgShape.coneWith
                { slices = 6
                , bottomRadius = Length.centimeters 7
                , topRadius = Length.centimeters 3
                , bottomPoint = Point3d.origin
                , topPoint = Point3d.xyz Quantity.zero Quantity.zero (Length.centimeters 20)
                }
                |> CsgShape.withTag Color.darkGreen

        cylinder =
            CsgShape.cylinder (Length.centimeters 5) (Length.centimeters 20)
                |> CsgShape.withTag Color.green

        smallSphere =
            CsgShape.sphere (Length.centimeters 5)
                |> CsgShape.withTag Color.purple
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


dice =
    let
        dotRadius =
            Length.centimeters 8

        cubeSize =
            Length.meters 1

        dotAt x y =
            CsgShape.geodesicSphere dotRadius 1
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

        sphere =
            CsgShape.geodesicSphere (Length.centimeters 70) 1
                |> CsgShape.withTag Color.blue

        base =
            CsgShape.cube cubeSize
                |> CsgShape.translateBy (Vector3d.meters -0.5 -0.5 -0.5)
                |> CsgShape.intersectWith sphere
                |> CsgShape.withTag Color.green

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
                |> CsgShape.translateBy (Vector3d.meters -0.5 -0.5 0.5)
                |> CsgShape.withTag Color.white
    in
    dots
        |> CsgShape.subtractFrom base


sphericon =
    let
        cone =
            CsgShape.cone (Length.meters 0.5) (Length.meters 0.5)

        twoCones =
            cone
                |> CsgShape.rotateAround Axis3d.x (Angle.degrees 180)
                |> CsgShape.translateBy (Vector3d.meters 0 0 0.001)
                |> CsgShape.unionWith cone

        halfCones =
            (CsgShape.cube (Length.meters 1)
                |> CsgShape.translateBy (Vector3d.meters -0.5 0 -0.5)
            )
                |> CsgShape.subtractFrom
                    twoCones
    in
    twoCones



{--
    halfCones
        |> CsgShape.unionWith
            (halfCones
                |> CsgShape.rotateAround Axis3d.z (Angle.degrees 180)
                |> CsgShape.rotateAround Axis3d.y (Angle.degrees 90)
            )
        |> CsgShape.withTag Color.orange
        --}


transformationsCube =
    let
        cube =
            CsgShape.cube (Length.meters 1)
                |> CsgShape.translateBy (Vector3d.meters -0.5 -0.5 -0.5)
                |> CsgShape.withTag Color.red

        sphere =
            CsgShape.sphere (Length.centimeters 70)
                |> CsgShape.withTag Color.blue

        cylinderY =
            CsgShape.cylinderFromTo (Length.centimeters 40) (Point3d.meters 0 -1 0) (Point3d.meters 0 1 0)

        cylinderX =
            CsgShape.cylinderFromTo (Length.centimeters 40) (Point3d.meters -1 0 0) (Point3d.meters 1 0 0)

        cylinderZ =
            CsgShape.cylinderFromTo (Length.centimeters 40) (Point3d.meters 0 0 -1) (Point3d.meters 0 0 1)

        cylinders =
            cylinderX
                |> CsgShape.unionWith cylinderY
                |> CsgShape.unionWith cylinderZ
                |> CsgShape.withTag Color.green
    in
    cylinders
        |> CsgShape.subtractFrom
            (cube
                |> CsgShape.intersectWith sphere
            )


debug =
    let
        visor =
            CsgShape.cuboid { width = Length.centimeters 5, height = Length.centimeters 50, depth = Length.centimeters 5 }
                |> CsgShape.moveRight (Length.centimeters 22.5)
                |> CsgShape.unionWith
                    (CsgShape.cuboid { width = Length.centimeters 5, height = Length.centimeters 50, depth = Length.centimeters 5 }
                        |> CsgShape.rotateAround Axis3d.y (Angle.degrees 90)
                        |> CsgShape.moveUp (Length.centimeters 50)
                    )
                |> CsgShape.withTag Color.gray

        bust =
            CsgShape.torus (Length.centimeters 22) (Length.centimeters 60)
                |> CsgShape.moveLeft (Length.centimeters 25)
                |> CsgShape.subtractFrom
                    (CsgShape.cylinder (Length.centimeters 40) (Length.centimeters 80)
                        |> CsgShape.withTag Color.black
                    )
    in
    bust


torus =
    let
        visor =
            CsgShape.cuboid { width = Length.centimeters 5, height = Length.centimeters 50, depth = Length.centimeters 5 }
                |> CsgShape.moveLeft (Length.centimeters 2.5)
                |> CsgShape.unionWith
                    (CsgShape.cuboid { width = Length.centimeters 5, height = Length.centimeters 50, depth = Length.centimeters 5 }
                        |> CsgShape.rotateAround Axis3d.y (Angle.degrees 90)
                        |> CsgShape.moveUp (Length.centimeters 50)
                    )
                |> CsgShape.withTag Color.gray

        bust =
            CsgShape.torus (Length.centimeters 20) (Length.centimeters 60)
                |> CsgShape.withTag Color.white
                |> CsgShape.scaleBy (Vector3d.unitless 1 1 0.8)
                |> CsgShape.rotateAround Axis3d.y (Angle.degrees 20)
                |> CsgShape.moveUp (Length.centimeters 25)
                |> CsgShape.subtractFrom
                    (CsgShape.cylinder (Length.centimeters 40) (Length.centimeters 80)
                        |> CsgShape.withTag Color.black
                    )
    in
    visor
        |> CsgShape.moveForward (Length.centimeters 40)
        |> CsgShape.moveUp (Length.centimeters 20)
        |> CsgShape.rotateAround Axis3d.z (Angle.degrees 90)
        |> CsgShape.subtractFrom bust


simpleTransformations =
    CsgShape.cube (Length.meters 1)
        |> CsgShape.moveLeft (Length.meters 0.5)
        |> CsgShape.moveUp (Length.meters 0.5)
        |> CsgShape.moveForward (Length.meters 0.5)
        |> CsgShape.unionWith
            (CsgShape.cube (Length.meters 1))


simpleUnion =
    CsgShape.cube (Length.meters 1)
        |> CsgShape.moveLeft (Length.meters 0.5)
        |> CsgShape.intersectWith
            (CsgShape.cube (Length.meters 1))


pawn =
    let
        base =
            CsgShape.cone (Length.centimeters 19) (Length.centimeters 20)
                |> CsgShape.unionWith
                    (CsgShape.torus (Length.centimeters 6) (Length.centimeters 19)
                        |> CsgShape.moveUp (Length.centimeters 6)
                    )
                |> CsgShape.unionWith
                    (CsgShape.cylinder (Length.centimeters 15) (Length.centimeters 8)
                        |> CsgShape.moveUp (Length.centimeters 4)
                    )
                |> CsgShape.withTag Color.yellow

        top =
            CsgShape.torus (Length.centimeters 12) (Length.centimeters 29)
                |> CsgShape.moveUp (Length.centimeters 12)
                |> CsgShape.subtractFrom
                    (CsgShape.cylinder (Length.centimeters 15) (Length.centimeters 20))
                |> CsgShape.unionWith
                    (CsgShape.sphereWith
                        { radius =
                            Length.centimeters 8.4
                        , stacks = 4
                        , slices = 8
                        }
                        |> CsgShape.rotateAround Axis3d.x (Angle.degrees 90)
                        |> CsgShape.intersectWith
                            (CsgShape.cube (Length.centimeters 16)
                                |> CsgShape.moveForward (Length.centimeters 8)
                                |> CsgShape.moveLeft (Length.centimeters 8)
                            )
                        |> CsgShape.moveUp (Length.centimeters 19.9)
                    )
                |> CsgShape.unionWith hat

        hat =
            CsgShape.sphereWith
                { radius =
                    Length.centimeters 8.4
                , stacks = 4
                , slices = 8
                }
                |> CsgShape.rotateAround Axis3d.x (Angle.degrees 90)
                |> CsgShape.scaleBy (Vector3d.unitless 1 1 1.1)
                |> CsgShape.moveUp (Length.centimeters 34)
                |> CsgShape.withTag Color.red
    in
    top
        |> CsgShape.moveUp (Length.centimeters 12)
        |> CsgShape.unionWith base


eightPawns =
    pawn
        |> CsgShape.withTag Color.black
        |> CsgShape.scaleAbout Point3d.origin 2
