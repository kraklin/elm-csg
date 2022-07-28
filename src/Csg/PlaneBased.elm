module Csg.PlaneBased exposing (..)

import Angle
import Axis3d
import BspTree exposing (Face)
import Color
import Csg.Shape3d exposing (Shape3d)
import Direction3d
import Length exposing (Length, Meters)
import List.Extra as List
import List.NonEmpty as NonEmpty
import Matrix
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity
import Triangle3d
import Vector3d exposing (Vector3d)


type alias Shape =
    List PlaneBasedFace


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
        first :: _ :: _ ->
            (items ++ [ first ])
                |> List.groupsOfWithStep 2 1

        _ ->
            []


planeCoeficients : Plane3d Meters c -> { a : Float, b : Float, c : Float, d : Float }
planeCoeficients plane =
    let
        sanitize value =
            value * 10 ^ 6 |> ceiling |> toFloat

        ( a, b, c ) =
            Plane3d.normalDirection plane
                |> Direction3d.components
                |> (\( a_, b_, c_ ) -> ( sanitize a_, sanitize b_, sanitize c_ ))

        inMicrons =
            Length.inMicrons >> ceiling >> toFloat

        d =
            Plane3d.originPoint plane |> (\point -> -(a * (Point3d.xCoordinate point |> inMicrons) + b * (Point3d.yCoordinate point |> inMicrons) + c * (Point3d.zCoordinate point |> inMicrons)))
    in
    { a = a, b = b, c = c, d = d }


fromFace : Face c -> Maybe PlaneBasedFace
fromFace face =
    NonEmpty.toList face.points
        |> faceFromPoints


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


toFace_ : PlaneEquation -> List (Point3d Meters c) -> Maybe (Face c)
toFace_ ({ a, b, c, d } as supportPlane) points =
    let
        normaldirection =
            Direction3d.from Point3d.origin (Point3d.xyz (Length.microns a) (Length.microns b) (Length.microns c))
    in
    case points of
        v1 :: v2 :: v3 :: rest ->
            normaldirection
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
        |> toFace_ planebased.supportPlane


det2 a b c d =
    a * d - (b * c)


det3 p q r =
    det3_ ( ( p.a, p.b, p.c ), ( q.a, q.b, q.c ), ( r.a, r.b, r.c ) )


det3_ ( ( a11, a12, a13 ), ( a21, a22, a23 ), ( a31, a32, a33 ) ) =
    a11
        * det2 a22 a23 a32 a33
        - a12
        * det2 a21 a23 a31 a33
        + a13
        * det2 a21 a22 a31 a32


det4 p q r s =
    (p.a * det3_ ( ( q.b, q.c, q.d ), ( r.b, r.c, r.d ), ( s.b, s.c, s.d ) ))
        - (p.b * det3_ ( ( q.a, q.c, q.d ), ( r.a, r.c, r.d ), ( s.a, s.c, s.d ) ))
        + (p.c * det3_ ( ( q.a, q.b, q.d ), ( r.a, r.b, r.d ), ( s.a, s.b, s.d ) ))
        - (p.d * det3_ ( ( q.a, q.b, q.c ), ( r.a, r.b, r.c ), ( s.a, s.b, s.c ) ))


orientation ( p, q, r ) s =
    det3 p q r * det4 p q r s


splitByPlane : Plane3d Meters c -> List PlaneBasedFace -> List PlaneBasedFace
splitByPlane splittingPlane faces =
    let
        splittingPlaneCoef =
            planeCoeficients splittingPlane

        isSimilaryOriented p q =
            p.a == q.a && p.b == q.b && p.c == q.c

        isCoincident p q =
            (det2 p.a p.b q.a q.b == 0)
                && (det2 p.b p.c q.b q.c == 0)
                && (det2 p.c p.d q.c q.d == 0)
                && (det2 p.a p.c q.a q.c == 0)
                && (det2 p.b p.d q.b q.d == 0)
                && (det2 p.a p.d q.a q.d == 0)

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
                    orientation ( s, bPlanes.prev2, bPlanes.prev ) splittingPlaneCoef

                classifyCurr =
                    orientation ( s, bPlanes.prev, bPlanes.curr ) splittingPlaneCoef

                classifyNext =
                    orientation ( s, bPlanes.curr, bPlanes.next ) splittingPlaneCoef
            in
            case ( compare classifyPrev 0, compare classifyCurr 0, compare classifyNext 0 ) of
                ( _, GT, _ ) ->
                    Just [ bPlanes.curr ]

                ( GT, EQ, GT ) ->
                    Just [ bPlanes.curr ]

                ( _, EQ, GT ) ->
                    Just [ splittingPlaneCoef, bPlanes.curr ]

                ( _, EQ, EQ ) ->
                    Nothing

                ( _, EQ, LT ) ->
                    Nothing

                ( _, LT, GT ) ->
                    Just [ splittingPlaneCoef, bPlanes.curr ]

                ( _, LT, EQ ) ->
                    Nothing

                ( _, LT, LT ) ->
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



-- BSP Tree


type BspTree
    = Leaf (List PlaneBasedFace)
    | Node { plane : PlaneEquation, inside : BspTree, outside : BspTree }


emptyTree : BspTree
emptyTree =
    Leaf []



-- items


faceFromPoints : List (Point3d Meters c) -> Maybe PlaneBasedFace
faceFromPoints points =
    let
        supportPlane =
            case points of
                v1 :: v2 :: v3 :: rest ->
                    Plane3d.throughPoints v1 v2 v3

                _ ->
                    Nothing

        supportPoint plane p =
            Point3d.translateIn (Plane3d.normalDirection plane) (Length.meters 1) p
    in
    case supportPlane of
        Just plane ->
            points
                |> toPairs
                |> (\points_ ->
                        points_
                            |> List.filterMap
                                (\p ->
                                    case p of
                                        [ v0, v1 ] ->
                                            Plane3d.throughPoints v0 v1 (supportPoint plane v1)
                                                |> Maybe.map planeCoeficients

                                        _ ->
                                            Nothing
                                )
                   )
                |> (\planes ->
                        case planes of
                            first :: second :: third :: rest ->
                                Just { boundingPlanes = ( ( first, second, third ), rest ), supportPlane = planeCoeficients plane }

                            _ ->
                                Nothing
                   )

        Nothing ->
            Nothing


flatFace =
    faceFromPoints [ Point3d.meters 0 0 0.499999, Point3d.meters 0.5 0 0, Point3d.meters 1 1 1, Point3d.meters 0 0 1 ]
        |> Maybe.map List.singleton
        |> Maybe.withDefault []


cuboid : { width : Length, height : Length, depth : Length } -> Shape
cuboid { width, height, depth } =
    let
        z =
            Length.meters 0

        a =
            Point3d.meters 0 0 0

        b =
            Point3d.xyz z z height

        c =
            Point3d.xyz width z height

        d =
            Point3d.xyz width z z

        e =
            Point3d.xyz z depth z

        f =
            Point3d.xyz z depth height

        g =
            Point3d.xyz width depth height

        h =
            Point3d.xyz width depth z

        front =
            faceFromPoints [ a, d, c, b ]

        back =
            faceFromPoints [ e, f, g, h ]

        top =
            faceFromPoints [ b, c, g, f ]

        bottom =
            faceFromPoints [ a, e, h, d ]

        left =
            faceFromPoints [ a, b, f, e ]

        right =
            faceFromPoints [ c, d, h, g ]
    in
    [ front, back, top, bottom, left, right ]
        |> List.filterMap identity


cube : Length -> Shape
cube size =
    cuboid { width = size, height = size, depth = size }


sphere : Shape
sphere =
    let
        stacks =
            16

        slices =
            16

        radius =
            Length.meters 1

        stacks_ =
            if stacks < 2 then
                2

            else
                stacks

        slices_ =
            if slices < 3 then
                3

            else
                slices

        deltaTheta =
            Angle.turns (0.5 / toFloat stacks_)

        deltaPhi =
            Angle.turns (1 / toFloat slices_)

        northPoint =
            Point3d.xyz (Length.meters 0) radius (Length.meters 0)

        vertex ( it, ip ) =
            northPoint
                |> Point3d.rotateAround Axis3d.x (Quantity.multiplyBy (toFloat it) deltaTheta)
                |> Point3d.rotateAround Axis3d.y (Quantity.multiplyBy (toFloat ip) deltaPhi)

        middle : List PlaneBasedFace
        middle =
            List.range 1 (stacks_ - 2)
                |> List.map
                    (\latId ->
                        List.range 0 (slices_ - 1)
                            |> List.filterMap
                                (\long ->
                                    faceFromPoints
                                        [ vertex ( latId, long )
                                        , vertex ( latId + 1, long )
                                        , vertex ( latId + 1, long + 1 )
                                        , vertex ( latId, long + 1 )
                                        ]
                                )
                    )
                |> List.concat

        northCap =
            List.range 0 (slices_ - 1)
                |> List.filterMap
                    (\long ->
                        let
                            i1 =
                                northPoint

                            i2 =
                                vertex ( 1, long )

                            i3 =
                                vertex ( 1, long + 1 )
                        in
                        faceFromPoints [ i1, i2, i3 ]
                    )

        southCap =
            List.range 0 (slices_ - 1)
                |> List.filterMap
                    (\long ->
                        let
                            i1 =
                                vertex ( stacks_ - 1, long )

                            i2 =
                                vertex ( stacks_, long )

                            i3 =
                                vertex ( stacks_ - 1, long + 1 )
                        in
                        faceFromPoints [ i1, i2, i3 ]
                    )
    in
    northCap ++ middle ++ southCap
