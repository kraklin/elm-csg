module Tests exposing (..)

import Direction3d
import Expect
import Length exposing (Meters)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Test exposing (Test)
import Triangle3d exposing (Triangle3d)
import Utils exposing (IntersectionResult(..))


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
                Utils.planeTriangleIntersection plane triangle
                    |> Expect.equal Coplanar
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
                Utils.planeTriangleIntersection plane triangle
                    |> Expect.equal Utils.None
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
                Utils.planeTriangleIntersection plane triangle
                    |> Expect.equal Utils.None
        ]
