module Csg.PlaneBased exposing (..)

import BspTree exposing (Face)
import Length exposing (Meters)
import List.Extra as List
import List.NonEmpty as NonEmpty
import Plane3d exposing (Plane3d)
import Point3d


type alias NonEmptyTriplet t =
    ( ( t, t, t ), List t )


type alias PlaneBasedFace c =
    { supportPlane : Plane3d Meters c
    , boundingPlanes : NonEmptyTriplet (Plane3d Meters c)
    , tempFace : Face c
    }


toPairs : List a -> Maybe (List (List a))
toPairs items =
    case items of
        first :: rest ->
            (items ++ [ first ])
                |> List.groupsOfWithStep 2 1
                |> Just

        _ ->
            Nothing


fromFace : Face c -> Maybe (PlaneBasedFace c)
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
                |> Maybe.map
                    (\points_ ->
                        points_
                            |> List.filterMap
                                (\p ->
                                    case p of
                                        [ v0, v1 ] ->
                                            Plane3d.throughPoints v0 v1 (supportPoint v0)

                                        _ ->
                                            Nothing
                                )
                    )
                |> (\planes ->
                        case planes of
                            Just (first :: second :: third :: rest) ->
                                Just ( ( first, second, third ), rest )

                            _ ->
                                Nothing
                   )
    in
    fromPoints face.points
        |> Maybe.map
            (\boundingPlanes ->
                { supportPlane = Plane3d.through (NonEmpty.head face.points) face.normalDirection
                , boundingPlanes = boundingPlanes
                , tempFace = face
                }
            )
        |> Debug.log "bounded plane"


toFace : PlaneBasedFace c -> Face c
toFace planebased =
    planebased.tempFace
