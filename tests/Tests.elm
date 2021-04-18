module Tests exposing (..)

import BspTree
import Direction3d
import Expect
import Length exposing (Meters)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Test exposing (Test)
import Triangle3d exposing (Triangle3d)


flipTriangle : Triangle3d Meters c -> Triangle3d Meters c
flipTriangle triangle =
    let
        ( v1, v2, v3 ) =
            Triangle3d.vertices triangle
    in
    Triangle3d.from v3 v2 v1


suite : Test
suite =
    Test.describe "Plane-triangle intersections"
        [ Test.test "Coplanar triangle should have all points intersected" <|
            \_ ->
                let
                    triangle =
                        Triangle3d.from
                            (Point3d.meters 0 0 0)
                            (Point3d.meters 1 0 0)
                            (Point3d.meters 1 1 0)

                    plane =
                        Plane3d.through (Point3d.meters 0 0 0) Direction3d.z
                in
                BspTree.splitByPlane plane triangle
                    |> Expect.equal { front = [ triangle ], back = [ flipTriangle triangle ] }
        , Test.test "Triangle outside of the plane has no points intersected" <|
            \_ ->
                let
                    triangle =
                        Triangle3d.from
                            (Point3d.meters 0 0 0)
                            (Point3d.meters 1 0 0)
                            (Point3d.meters 1 1 0)

                    plane =
                        Plane3d.through (Point3d.meters 0 0 1) Direction3d.negativeZ
                in
                BspTree.splitByPlane plane triangle
                    |> Expect.equal { front = [ triangle ], back = [] }
        , Test.test "Triangle on the other side of the plane has no points intersected" <|
            \_ ->
                let
                    triangle =
                        Triangle3d.from
                            (Point3d.meters 0 0 0)
                            (Point3d.meters 1 0 0)
                            (Point3d.meters 1 1 0)

                    plane =
                        Plane3d.through (Point3d.meters 0 0 -1) Direction3d.negativeZ
                in
                BspTree.splitByPlane plane triangle
                    |> Expect.equal { front = [], back = [ triangle ] }
        , Test.describe "Triangle on the front side of the plane with one point on the plane should stay on that side" <|
            let
                v1 =
                    Point3d.meters 0 0 0

                v2 =
                    Point3d.meters 1 1 0

                v3 =
                    Point3d.meters 0 1 0

                plane =
                    Plane3d.through (Point3d.meters 0 0 0) Direction3d.y
            in
            [ Test.test "first point touching the plane" <|
                \_ ->
                    let
                        triangle =
                            Triangle3d.from v1 v2 v3
                    in
                    triangle
                        |> BspTree.splitByPlane plane
                        |> Expect.equal { front = [ triangle ], back = [] }
            , Test.test "second point touching the plane" <|
                \_ ->
                    let
                        triangle =
                            Triangle3d.from v3 v1 v2
                    in
                    triangle
                        |> BspTree.splitByPlane plane
                        |> Expect.equal { front = [ triangle ], back = [] }
            , Test.test "third point touching the plane" <|
                \_ ->
                    let
                        triangle =
                            Triangle3d.from v2 v3 v1
                    in
                    triangle
                        |> BspTree.splitByPlane plane
                        |> Expect.equal { front = [ triangle ], back = [] }
            ]
        , Test.describe "Triangle on the back side of the plane with one point on the plane should stay on that side" <|
            let
                v1 =
                    Point3d.meters 0 0 0

                v2 =
                    Point3d.meters 0 -1 0

                v3 =
                    Point3d.meters 1 -1 0

                plane =
                    Plane3d.through (Point3d.meters 0 0 0) Direction3d.y
            in
            [ Test.test "first point touching the plane" <|
                \_ ->
                    let
                        triangle =
                            Triangle3d.from v1 v2 v3
                    in
                    triangle
                        |> BspTree.splitByPlane plane
                        |> Expect.equal { front = [], back = [ triangle ] }
            , Test.test "second point touching the plane" <|
                \_ ->
                    let
                        triangle =
                            Triangle3d.from v3 v1 v2
                    in
                    triangle
                        |> BspTree.splitByPlane plane
                        |> Expect.equal { front = [], back = [ triangle ] }
            , Test.test "third point touching the plane" <|
                \_ ->
                    let
                        triangle =
                            Triangle3d.from v2 v3 v1
                    in
                    triangle
                        |> BspTree.splitByPlane plane
                        |> Expect.equal { front = [], back = [ triangle ] }
            ]
        , Test.describe "Spanning triagnles" <|
            [ Test.test "Spanning triangle has one correct triangle in front" <|
                \_ ->
                    let
                        triangle =
                            Triangle3d.from
                                (Point3d.meters 0 1 0)
                                (Point3d.meters 0 -1 0)
                                (Point3d.meters 2 -1 0)

                        plane =
                            Plane3d.through (Point3d.meters 0 0 0) Direction3d.y
                    in
                    triangle
                        |> BspTree.splitByPlane plane
                        |> .front
                        |> Expect.equal [ Triangle3d.from (Point3d.meters 0 1 0) (Point3d.meters 0 0 0) (Point3d.meters 1 0 0) ]
            , Test.test "Spanning triangle has two correct triangles on the back" <|
                \_ ->
                    let
                        triangle =
                            Triangle3d.from
                                (Point3d.meters 0 1 0)
                                (Point3d.meters 0 -1 0)
                                (Point3d.meters 2 -1 0)

                        plane =
                            Plane3d.through (Point3d.meters 0 0 0) Direction3d.y
                    in
                    triangle
                        |> BspTree.splitByPlane plane
                        |> .back
                        |> Expect.equal
                            [ Triangle3d.from (Point3d.meters 0 0 0) (Point3d.meters 0 -1 0) (Point3d.meters 2 -1 0)
                            , Triangle3d.from (Point3d.meters 2 -1 0) (Point3d.meters 1 0 0) (Point3d.meters 0 0 0)
                            ]
            ]
        ]
