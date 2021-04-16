module Utils exposing (..)

import Length exposing (Meters)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity
import Triangle3d exposing (Triangle3d)


type IntersectionResult c
    = Coplanar
    | None


planeTriangleIntersection : Plane3d Meters c -> Triangle3d Meters c -> IntersectionResult c
planeTriangleIntersection plane triangle =
    let
        ( v1, v2, v3 ) =
            Triangle3d.vertices triangle

        ( d1, d2, d3 ) =
            ( Point3d.signedDistanceFrom plane v1 |> Quantity.unwrap
            , Point3d.signedDistanceFrom plane v2 |> Quantity.unwrap
            , Point3d.signedDistanceFrom plane v3 |> Quantity.unwrap
            )
    in
    if d1 > 0 && d2 > 0 && d3 > 0 then
        None

    else if d1 < 0 && d2 < 0 && d3 < 0 then
        None

    else
        Coplanar
