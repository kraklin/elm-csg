module Csg.PlaneBased.Face exposing (..)

import Array exposing (Array)
import BspTree exposing (Face)
import Color
import Csg.PlaneBased.CircularArray as CircularArray exposing (CircularArray)
import Direction3d exposing (Direction3d)
import Length exposing (Meters)
import List.Extra as List
import List.NonEmpty as NonEmpty
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity
import Vector3d


type alias PlaneBasedFace =
    { supportPlane : PlaneEquation
    , boundingPlanes : CircularArray ( PlaneEquation, Intersection4d )
    }


type alias Intersection4d =
    { x : Float, y : Float, z : Float, e : Float }


type alias PlaneEquation =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , originPoint :
        { x : Float, y : Float, z : Float }
    }


type alias NonEmptyTriplet t =
    ( ( t, t, t ), List t )


normalDirection : PlaneEquation -> Maybe (Direction3d c)
normalDirection { a, b, c } =
    Direction3d.from Point3d.origin (Point3d.xyz (Length.microns a) (Length.microns b) (Length.microns c))


type alias SplitResult =
    { inside : Maybe PlaneBasedFace, outside : Maybe PlaneBasedFace, original : PlaneBasedFace }


toFace : PlaneBasedFace -> Maybe (Face c)
toFace planebased =
    let
        toPlanesTriplets planes =
            planes
                |> List.map Tuple.first
                |> toPairs
                |> List.filterMap
                    (\pair ->
                        case pair of
                            [ a, b ] ->
                                Just ( a, b, planebased.supportPlane )

                            _ ->
                                Nothing
                    )

        toFace_ supportingPlane points =
            case points of
                v1 :: v2 :: v3 :: rest ->
                    normalDirection supportingPlane
                        |> Maybe.map
                            (\normal ->
                                Face (NonEmpty.fromCons v1 (v2 :: v3 :: rest))
                                    normal
                                    Color.gray
                            )

                _ ->
                    Nothing
    in
    planebased.boundingPlanes
        |> CircularArray.toList
        |> toPlanesTriplets
        |> List.map planesIntersection
        |> toFace_ planebased.supportPlane


withIntersectionPoints p q r rest =
    CircularArray.fromList p (q :: r :: rest)
        |> CircularArray.map (\plane -> ( plane, { x = 0, y = 0, z = 0, e = 0 } ))


fromPoints : List (Point3d Meters c) -> Maybe PlaneBasedFace
fromPoints points =
    let
        supportPlane =
            case points of
                v1 :: v2 :: v3 :: _ ->
                    Plane3d.throughPoints v1 v2 v3

                _ ->
                    Nothing

        supportPoint plane p =
            Point3d.translateIn (Plane3d.normalDirection plane) (Length.meters 1) p
    in
    supportPlane
        |> Maybe.andThen
            (\plane ->
                points
                    |> toPairs
                    |> (\points_ ->
                            points_
                                |> List.filterMap
                                    (\p ->
                                        case p of
                                            [ v0, v1 ] ->
                                                Plane3d.throughPoints v0 v1 (supportPoint plane v1)
                                                    |> Maybe.map fromPlane3d

                                            _ ->
                                                Nothing
                                    )
                       )
                    |> (\planes ->
                            case planes of
                                p :: q :: r :: rest ->
                                    Just { boundingPlanes = withIntersectionPoints p q r rest, supportPlane = fromPlane3d plane }

                                _ ->
                                    Nothing
                       )
            )


fromFace : Face c -> Maybe PlaneBasedFace
fromFace face =
    NonEmpty.toList face.points
        |> fromPoints


toPairs : List a -> List (List a)
toPairs items =
    case items of
        first :: _ :: _ ->
            (items ++ [ first ])
                |> List.groupsOfWithStep 2 1

        _ ->
            []


planesIntersection : ( PlaneEquation, PlaneEquation, PlaneEquation ) -> Point3d Meters c
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


fromPlane3d : Plane3d Meters c -> PlaneEquation
fromPlane3d plane =
    let
        ( a, b, c ) =
            Plane3d.normalDirection plane
                |> Direction3d.components
                |> (\( a_, b_, c_ ) -> ( a_, b_, c_ ))

        inMicrons =
            Length.inMicrons >> ceiling >> toFloat

        ( x, y, z ) =
            Plane3d.originPoint plane |> (\point -> ( Point3d.xCoordinate point |> inMicrons, Point3d.yCoordinate point |> inMicrons, Point3d.zCoordinate point |> inMicrons ))

        d =
            -(a * x + b * y + c * z)

        intersection4d =
            { x = 0, y = 0, z = 0, e = 0 }
    in
    { a = a, b = b, c = c, d = d, originPoint = { x = x, y = y, z = z } }


det2 : Float -> Float -> Float -> Float -> Float
det2 a b c d =
    a * d - (b * c)


det3 : PlaneEquation -> PlaneEquation -> PlaneEquation -> Float
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


det4 : PlaneEquation -> PlaneEquation -> PlaneEquation -> PlaneEquation -> Float
det4 p q r s =
    (p.a * det3_ ( ( q.b, q.c, q.d ), ( r.b, r.c, r.d ), ( s.b, s.c, s.d ) ))
        - (p.b * det3_ ( ( q.a, q.c, q.d ), ( r.a, r.c, r.d ), ( s.a, s.c, s.d ) ))
        + (p.c * det3_ ( ( q.a, q.b, q.d ), ( r.a, r.b, r.d ), ( s.a, s.b, s.d ) ))
        - (p.d * det3_ ( ( q.a, q.b, q.c ), ( r.a, r.b, r.c ), ( s.a, s.b, s.c ) ))


orientation : ( PlaneEquation, PlaneEquation, PlaneEquation ) -> PlaneEquation -> Float
orientation ( p, q, r ) s =
    det3 p q r * det4 p q r s


areSimilaryOriented : PlaneEquation -> PlaneEquation -> Bool
areSimilaryOriented p q =
    p.a == q.a && p.b == q.b && p.c == q.c


areCoincident : PlaneEquation -> PlaneEquation -> Bool
areCoincident p q =
    (det2 p.a p.b q.a q.b == 0)
        && (det2 p.b p.c q.b q.c == 0)
        && (det2 p.c p.d q.c q.d == 0)
        && (det2 p.a p.c q.a q.c == 0)
        && (det2 p.b p.d q.b q.d == 0)
        && (det2 p.a p.d q.a q.d == 0)


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


clipByPlane : PlaneEquation -> PlaneBasedFace -> Maybe PlaneBasedFace
clipByPlane splittingPlane face =
    let
        boundingPlanesCandidates planes =
            planes
                ++ List.take 3 planes
                |> List.groupsOfWithStep 4 1
                |> List.filterMap
                    (\groupPlanes ->
                        case groupPlanes of
                            [ a, b, c, d ] ->
                                Just { prev2 = a, prev = b, curr = c, next = d }

                            _ ->
                                Nothing
                    )

        outputPlane s bPlanes =
            let
                classifyPrev =
                    orientation ( s, Tuple.first bPlanes.prev2, Tuple.first bPlanes.prev ) splittingPlane

                classifyCurr =
                    orientation ( s, Tuple.first bPlanes.prev, Tuple.first bPlanes.curr ) splittingPlane

                classifyNext =
                    orientation ( s, Tuple.first bPlanes.curr, Tuple.first bPlanes.next ) splittingPlane

                splittingPlaneIntersection =
                    { x = 0, y = 0, z = 0, e = 0 }
            in
            case ( compare classifyPrev 0, compare classifyCurr 0, compare classifyNext 0 ) of
                ( _, GT, _ ) ->
                    Just [ bPlanes.curr ]

                ( GT, EQ, GT ) ->
                    Just [ bPlanes.curr ]

                ( _, EQ, GT ) ->
                    Just [ ( splittingPlane, splittingPlaneIntersection ), bPlanes.curr ]

                ( _, EQ, EQ ) ->
                    Nothing

                ( _, EQ, LT ) ->
                    Nothing

                ( _, LT, GT ) ->
                    Just [ ( splittingPlane, splittingPlaneIntersection ), bPlanes.curr ]

                ( _, LT, EQ ) ->
                    Nothing

                ( _, LT, LT ) ->
                    Nothing
    in
    if areCoincident splittingPlane face.supportPlane then
        if areSimilaryOriented splittingPlane face.supportPlane then
            Just face

        else
            Nothing

    else
        face.boundingPlanes
            |> CircularArray.toList
            |> boundingPlanesCandidates
            |> List.filterMap (outputPlane face.supportPlane)
            |> List.concat
            |> (\planes ->
                    case planes of
                        p :: rest ->
                            Just { face | boundingPlanes = CircularArray.fromList p rest }

                        _ ->
                            Nothing
               )


splitByPlane : PlaneEquation -> PlaneBasedFace -> SplitResult
splitByPlane splittingPlane face =
    { inside = clipByPlane splittingPlane face
    , outside = clipByPlane (flipNormal splittingPlane) face
    , original = face
    }
