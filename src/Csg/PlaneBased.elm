module Csg.PlaneBased exposing (..)

import BspTree exposing (Face)
import Color
import Direction3d
import Length exposing (Meters)
import List.Extra as List
import List.NonEmpty as NonEmpty
import Matrix
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity
import Triangle3d
import Vector3d exposing (Vector3d)


type alias NonEmptyTriplet t =
    ( ( t, t, t ), List t )


type alias PlaneEquation =
    { a : Float, b : Float, c : Float, d : Float }


type alias PlaneBasedFace =
    { supportPlane : PlaneEquation
    , boundingPlanes : NonEmptyTriplet PlaneEquation
    }


toPairs : List a -> List (List a)
toPairs items =
    case items of
        first :: rest ->
            (items ++ [ first ])
                |> List.groupsOfWithStep 2 1

        _ ->
            []


planeCoeficients : Plane3d Meters c -> { a : Float, b : Float, c : Float, d : Float }
planeCoeficients plane =
    let
        ( a, b, c ) =
            Plane3d.normalDirection plane |> Direction3d.components

        d =
            Plane3d.originPoint plane |> Point3d.toMeters |> (\{ x, y, z } -> -(a * x + b * y + c * z))
    in
    { a = a, b = b, c = c, d = d }


fromFace : Face c -> Maybe PlaneBasedFace
fromFace face =
    let
        fromPoints points =
            let
                supportPoint p =
                    Point3d.translateIn face.normalDirection (Length.meters 1) p
            in
            points
                |> NonEmpty.toList
                |> toPairs
                |> (\points_ ->
                        points_
                            |> List.filterMap
                                (\p ->
                                    case p of
                                        [ v0, v1 ] ->
                                            Plane3d.throughPoints v0 v1 (supportPoint v1)
                                                |> Maybe.map planeCoeficients

                                        _ ->
                                            Nothing
                                )
                   )
                |> (\planes ->
                        case planes of
                            first :: second :: third :: rest ->
                                Just ( ( first, second, third ), rest )

                            _ ->
                                Nothing
                   )
    in
    fromPoints face.points
        |> Maybe.map
            (\boundingPlanes ->
                { supportPlane = Plane3d.through (NonEmpty.head face.points) face.normalDirection |> planeCoeficients
                , boundingPlanes = boundingPlanes
                }
            )


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
    in
    Point3d.meters
        ((Vector3d.dot d u |> Quantity.unwrap) / denom)
        ((Vector3d.dot m3 v |> Quantity.unwrap) / denom)
        (negate (Vector3d.dot m2 v |> Quantity.unwrap) / denom)


toFace_ : List (Point3d Meters c) -> Maybe (Face c)
toFace_ points =
    case points of
        v1 :: v2 :: v3 :: rest ->
            Triangle3d.from v1 v2 v3
                |> Triangle3d.normalDirection
                |> Maybe.map
                    (\normal ->
                        Face (NonEmpty.fromCons v1 (v2 :: v3 :: rest))
                            normal
                            Color.gray
                    )

        _ ->
            Nothing


toFace : PlaneBasedFace -> Maybe (Face c)
toFace planebased =
    let
        toPlanesTriplets ( ( p, q, r ), rest ) =
            (p :: q :: r :: rest)
                |> toPairs
                |> List.filterMap
                    (\pair ->
                        case pair of
                            [ a, b ] ->
                                Just ( a, b, planebased.supportPlane )

                            _ ->
                                Nothing
                    )
    in
    toPlanesTriplets planebased.boundingPlanes
        |> List.map planesIntersection
        |> toFace_


splitByPlane : Plane3d Meters c -> List PlaneBasedFace -> List PlaneBasedFace
splitByPlane splittingPlane faces =
    let
        splittingPlaneCoef =
            planeCoeficients splittingPlane

        isSimilaryOriented p q =
            p.a == q.a && p.b == q.b && p.c == q.c

        det2 a b c d =
            a * d - b * c

        isCoincident p q =
            (det2 p.a p.b q.a q.b == 0)
                && (det2 p.b p.c q.b q.c == 0)
                && (det2 p.c p.d q.c q.d == 0)

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

        distance point =
            Point3d.toMeters point
                |> (\{ x, y, z } ->
                        (splittingPlaneCoef.a * x + splittingPlaneCoef.b * y + splittingPlaneCoef.c * z + splittingPlaneCoef.d)
                            / sqrt
                                (splittingPlaneCoef.a ^ 2 + splittingPlaneCoef.b ^ 2 + splittingPlaneCoef.c ^ 2)
                   )

        outputPlane s bPlanes =
            let
                classifyPrev =
                    planesIntersection ( s, bPlanes.prev2, bPlanes.prev )
                        |> distance
                        |> Debug.log "prev"

                classifyCurr =
                    planesIntersection ( s, bPlanes.prev, bPlanes.curr )
                        |> distance
                        |> Debug.log "curr"

                classifyNext =
                    planesIntersection ( s, bPlanes.curr, bPlanes.next )
                        |> distance
                        |> Debug.log "next"
            in
            case ( compare classifyPrev 0, compare classifyCurr 0, compare classifyNext 0 ) of
                ( _, GT, _ ) ->
                    Just [ bPlanes.curr ]

                ( GT, EQ, GT ) ->
                    Just [ bPlanes.curr ]

                ( _, EQ, GT ) ->
                    Just [ splittingPlaneCoef, bPlanes.curr ]

                ( _, LT, GT ) ->
                    Just [ splittingPlaneCoef, bPlanes.curr ]

                ( _, EQ, EQ ) ->
                    Nothing

                ( _, EQ, LT ) ->
                    Nothing

                _ ->
                    Nothing

        tripletToList ( ( p, q, r ), rest ) =
            p :: q :: r :: rest

        splitFaceByPlane : PlaneBasedFace -> Maybe PlaneBasedFace
        splitFaceByPlane face =
            if isCoincident splittingPlaneCoef face.supportPlane then
                if isSimilaryOriented splittingPlaneCoef face.supportPlane then
                    Just face

                else
                    Nothing

            else
                face.boundingPlanes
                    |> tripletToList
                    |> boundingPlanesCandidates
                    |> List.filterMap (outputPlane face.supportPlane)
                    |> List.concat
                    |> (\planes ->
                            case planes of
                                p :: q :: r :: rest ->
                                    Just { face | boundingPlanes = ( ( p, q, r ), rest ) }

                                _ ->
                                    Nothing
                       )
    in
    faces
        |> List.filterMap splitFaceByPlane
