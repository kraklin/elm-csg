module Csg.PlaneBased.Face exposing (..)

import BspTree exposing (Face)
import Color
import Csg.PlaneBased.Plane as Plane exposing (Plane)
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
    { supportPlane : Plane
    , boundingPlanes : NonEmptyTriplet Plane
    }


type alias NonEmptyTriplet t =
    ( ( t, t, t ), List t )


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
                    Plane.normalDirection supportingPlane
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
        |> List.map Plane.planesIntersection
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
                                                |> Maybe.map Plane.fromPlane3d

                                        _ ->
                                            Nothing
                                )
                   )
                |> (\planes ->
                        case planes of
                            first :: second :: third :: rest ->
                                Just { boundingPlanes = ( ( first, second, third ), rest ), supportPlane = Plane.fromPlane3d plane }

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


clipByPlane : Plane -> PlaneBasedFace -> Maybe PlaneBasedFace
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
                    Plane.orientation ( s, bPlanes.prev2, bPlanes.prev ) splittingPlane

                classifyCurr =
                    Plane.orientation ( s, bPlanes.prev, bPlanes.curr ) splittingPlane

                classifyNext =
                    Plane.orientation ( s, bPlanes.curr, bPlanes.next ) splittingPlane
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
    if Plane.areCoincident splittingPlane face.supportPlane then
        if Plane.areSimilaryOriented splittingPlane face.supportPlane then
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


splitByPlane : Plane -> PlaneBasedFace -> SplitResult
splitByPlane splittingPlane face =
    { inside = clipByPlane splittingPlane face
    , outside = clipByPlane (Plane.flipNormal splittingPlane) face
    , original = face
    }


sanitizeMeters : Float -> Float
sanitizeMeters value =
    value * 10 ^ 6 |> ceiling |> toFloat


translate : Vector3d Meters c -> PlaneBasedFace -> PlaneBasedFace
translate vector { boundingPlanes, supportPlane } =
    let
        translateVector =
            Vector3d.components vector |> (\( x, y, z ) -> { x = Length.inMeters x |> sanitizeMeters, y = Length.inMeters y |> sanitizeMeters, z = Length.inMeters z |> sanitizeMeters })

        newBoundingPlanes ( ( p, q, r ), rest ) =
            ( ( Plane.translatePlane translateVector p
              , Plane.translatePlane translateVector q
              , Plane.translatePlane translateVector r
              )
            , List.map (Plane.translatePlane translateVector) rest
            )
    in
    { boundingPlanes = newBoundingPlanes boundingPlanes, supportPlane = Plane.translatePlane translateVector supportPlane }
