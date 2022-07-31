module Csg.PlaneBased.Face exposing (..)

import BspTree exposing (Face)
import Color
import Direction3d exposing (Direction3d)
import Html exposing (q)
import Length exposing (Meters)
import List.Extra as List
import List.NonEmpty as NonEmpty
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity
import Vector3d exposing (Vector3d)


type alias PlaneBasedFace =
    { supportPlane : PlaneEquation
    , boundingPlanes : NonEmptyTriplet PlaneEquation
    }


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
    toPlanesTriplets planebased.boundingPlanes
        |> List.map planesIntersection
        |> toFace_ planebased.supportPlane


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
                                                |> Maybe.map fromPlane3d

                                        _ ->
                                            Nothing
                                )
                   )
                |> (\planes ->
                        case planes of
                            first :: second :: third :: rest ->
                                Just { boundingPlanes = ( ( first, second, third ), rest ), supportPlane = fromPlane3d plane }

                            _ ->
                                Nothing
                   )

        Nothing ->
            Nothing


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


sanitizeMeters : Float -> Float
sanitizeMeters value =
    value * 10 ^ 6 |> ceiling |> toFloat


fromPlane3d : Plane3d Meters c -> PlaneEquation
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
                    orientation ( s, bPlanes.prev2, bPlanes.prev ) splittingPlane

                classifyCurr =
                    orientation ( s, bPlanes.prev, bPlanes.curr ) splittingPlane

                classifyNext =
                    orientation ( s, bPlanes.curr, bPlanes.next ) splittingPlane
            in
            case ( compare classifyPrev 0, compare classifyCurr 0, compare classifyNext 0 ) of
                ( _, GT, _ ) ->
                    Just [ bPlanes.curr ]

                ( GT, EQ, GT ) ->
                    Just [ bPlanes.curr ]

                ( _, EQ, GT ) ->
                    Just [ splittingPlane, bPlanes.curr ]

                ( _, EQ, EQ ) ->
                    Nothing

                ( _, EQ, LT ) ->
                    Nothing

                ( _, LT, GT ) ->
                    Just [ splittingPlane, bPlanes.curr ]

                ( _, LT, EQ ) ->
                    Nothing

                ( _, LT, LT ) ->
                    Nothing

        tripletToList ( ( p, q, r ), rest ) =
            p :: q :: r :: rest
    in
    if areCoincident splittingPlane face.supportPlane then
        if areSimilaryOriented splittingPlane face.supportPlane then
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


splitByPlane : PlaneEquation -> PlaneBasedFace -> SplitResult
splitByPlane splittingPlane face =
    { inside = clipByPlane splittingPlane face
    , outside = clipByPlane (flipNormal splittingPlane) face
    , original = face
    }


translatePlane : { x : Float, y : Float, z : Float } -> PlaneEquation -> PlaneEquation
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


translate : Vector3d Meters c -> PlaneBasedFace -> PlaneBasedFace
translate vector { boundingPlanes, supportPlane } =
    let
        translateVector =
            Vector3d.components vector |> (\( x, y, z ) -> { x = Length.inMeters x |> sanitizeMeters, y = Length.inMeters y |> sanitizeMeters, z = Length.inMeters z |> sanitizeMeters })

        newBoundingPlanes ( ( p, q, r ), rest ) =
            ( ( translatePlane translateVector p
              , translatePlane translateVector q
              , translatePlane translateVector r
              )
            , List.map (translatePlane translateVector) rest
            )
    in
    { boundingPlanes = newBoundingPlanes boundingPlanes, supportPlane = translatePlane translateVector supportPlane }
