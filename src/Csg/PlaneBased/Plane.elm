module Csg.PlaneBased.Plane exposing (..)

import Direction3d exposing (Direction3d)
import Length exposing (Meters)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity
import Vector3d exposing (Vector3d)


type alias Plane =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , originPoint :
        { x : Float, y : Float, z : Float }
    }


normalDirection : Plane -> Maybe (Direction3d c)
normalDirection { a, b, c } =
    Direction3d.from Point3d.origin (Point3d.xyz (Length.microns a) (Length.microns b) (Length.microns c))


areSimilaryOriented : Plane -> Plane -> Bool
areSimilaryOriented p q =
    p.a == q.a && p.b == q.b && p.c == q.c


orientation : ( Plane, Plane, Plane ) -> Plane -> Float
orientation ( p, q, r ) s =
    det3 p q r * det4 p q r s


det2 : Float -> Float -> Float -> Float -> Float
det2 a b c d =
    a * d - (b * c)


det3 : Plane -> Plane -> Plane -> Float
det3 p q r =
    det3_ ( ( p.a, p.b, p.c ), ( q.a, q.b, q.c ), ( r.a, r.b, r.c ) )


det3_ :
    ( ( Float, Float, Float )
    , ( Float, Float, Float )
    , ( Float, Float, Float )
    )
    -> Float
det3_ ( ( a11, a12, a13 ), ( a21, a22, a23 ), ( a31, a32, a33 ) ) =
    a11
        * det2 a22 a23 a32 a33
        - a12
        * det2 a21 a23 a31 a33
        + a13
        * det2 a21 a22 a31 a32


det4 : Plane -> Plane -> Plane -> Plane -> Float
det4 p q r s =
    (p.a * det3_ ( ( q.b, q.c, q.d ), ( r.b, r.c, r.d ), ( s.b, s.c, s.d ) ))
        - (p.b * det3_ ( ( q.a, q.c, q.d ), ( r.a, r.c, r.d ), ( s.a, s.c, s.d ) ))
        + (p.c * det3_ ( ( q.a, q.b, q.d ), ( r.a, r.b, r.d ), ( s.a, s.b, s.d ) ))
        - (p.d * det3_ ( ( q.a, q.b, q.c ), ( r.a, r.b, r.c ), ( s.a, s.b, s.c ) ))


areCoincident : Plane -> Plane -> Bool
areCoincident p q =
    (det2 p.a p.b q.a q.b == 0)
        && (det2 p.b p.c q.b q.c == 0)
        && (det2 p.c p.d q.c q.d == 0)
        && (det2 p.a p.c q.a q.c == 0)
        && (det2 p.b p.d q.b q.d == 0)
        && (det2 p.a p.d q.a q.d == 0)


flipNormal : Plane -> Plane
flipNormal plane =
    let
        { x, y, z } =
            plane.originPoint
    in
    { plane
        | a = -plane.a
        , b = -plane.b
        , c = -plane.c
        , d = -((-plane.a * x) + (-plane.b * y) + (-plane.c * z))
    }


planesIntersection : ( Plane, Plane, Plane ) -> Point3d Meters c
planesIntersection ( p, q, r ) =
    let
        m1 =
            Vector3d.fromUnitless { x = p.a, y = q.a, z = r.a }

        m2 =
            Vector3d.fromUnitless { x = p.b, y = q.b, z = r.b }

        m3 =
            Vector3d.fromUnitless { x = p.c, y = q.c, z = r.c }

        d =
            Vector3d.fromUnitless { x = -p.d, y = -q.d, z = -r.d }

        u =
            Vector3d.cross m2 m3

        v =
            Vector3d.cross m1 d

        denom =
            Vector3d.dot m1 u
                |> Quantity.unwrap

        toMicrons value =
            value
                |> ceiling
                |> toFloat
                |> Length.microns
    in
    Point3d.xyz
        (toMicrons ((Vector3d.dot d u |> Quantity.unwrap) / denom))
        (toMicrons ((Vector3d.dot m3 v |> Quantity.unwrap) / denom))
        (toMicrons (-(Vector3d.dot m2 v |> Quantity.unwrap) / denom))


sanitizeMeters : Float -> Float
sanitizeMeters value =
    value * 10 ^ 6 |> ceiling |> toFloat


fromPlane3d : Plane3d Meters c -> Plane
fromPlane3d plane =
    let
        ( a, b, c ) =
            Plane3d.normalDirection plane
                |> Direction3d.components
                |> (\( a_, b_, c_ ) -> ( sanitizeMeters a_, sanitizeMeters b_, sanitizeMeters c_ ))

        inMicrons =
            Length.inMicrons >> ceiling >> toFloat

        ( x, y, z ) =
            Plane3d.originPoint plane |> (\point -> ( Point3d.xCoordinate point |> inMicrons, Point3d.yCoordinate point |> inMicrons, Point3d.zCoordinate point |> inMicrons ))

        d =
            -(a * x + b * y + c * z)
    in
    { a = a, b = b, c = c, d = d, originPoint = { x = x, y = y, z = z } }


translatePlane : { x : Float, y : Float, z : Float } -> Plane -> Plane
translatePlane direction { a, b, c, originPoint } =
    let
        newOrigin =
            { x = originPoint.x + direction.x
            , y = originPoint.y + direction.y
            , z = originPoint.z + direction.y
            }

        newD =
            -(a * newOrigin.x + b * newOrigin.y + c * newOrigin.z)
    in
    { a = a, b = b, c = c, d = newD, originPoint = newOrigin }
