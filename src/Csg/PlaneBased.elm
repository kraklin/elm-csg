module Csg.PlaneBased exposing (..)

import Angle
import Axis3d
import Csg.PlaneBased.BspTree as BspTree exposing (BspTree)
import Csg.PlaneBased.Face as PlaneBasedFace exposing (PlaneBasedFace)
import Length exposing (Length, Meters)
import List.Extra as List
import Plane3d exposing (Plane3d)
import Point3d
import Quantity
import Vector3d exposing (Vector3d)


type alias Shape =
    BspTree


toFaces : Shape -> List PlaneBasedFace
toFaces =
    BspTree.toFaces


toShape : List PlaneBasedFace -> Shape
toShape =
    BspTree.build


splitByPlane : Plane3d Meters c -> Shape -> ( Shape, Shape )
splitByPlane splittingPlane faces =
    let
        plane =
            PlaneBasedFace.fromPlane3d splittingPlane

        handle face acc =
            case face of
                Just f ->
                    f :: acc

                Nothing ->
                    acc
    in
    faces
        |> BspTree.toFaces
        |> List.map (PlaneBasedFace.splitByPlane plane)
        |> List.foldl (\{ inside, outside } ( inAcc, outAcc ) -> ( handle inside inAcc, handle outside outAcc )) ( [], [] )
        |> Tuple.mapBoth BspTree.build BspTree.build


mergeShapes : ( List PlaneBasedFace, List PlaneBasedFace ) -> List PlaneBasedFace
mergeShapes ( a, b ) =
    a ++ b



-- items


flatFace =
    PlaneBasedFace.fromPoints [ Point3d.meters 0 0 0.499999, Point3d.meters 0.5 0 0, Point3d.meters 1 1 1, Point3d.meters 0 0 1 ]
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
            PlaneBasedFace.fromPoints [ a, d, c, b ]

        back =
            PlaneBasedFace.fromPoints [ e, f, g, h ]

        top =
            PlaneBasedFace.fromPoints [ b, c, g, f ]

        bottom =
            PlaneBasedFace.fromPoints [ a, e, h, d ]

        left =
            PlaneBasedFace.fromPoints [ a, b, f, e ]

        right =
            PlaneBasedFace.fromPoints [ c, d, h, g ]
    in
    [ front, back, top, bottom, left, right ]
        |> List.filterMap identity
        |> BspTree.build


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
                                    PlaneBasedFace.fromPoints
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
                        PlaneBasedFace.fromPoints [ i1, i2, i3 ]
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
                        PlaneBasedFace.fromPoints [ i1, i2, i3 ]
                    )
    in
    northCap
        ++ middle
        ++ southCap
        |> BspTree.build


translateBy : Vector3d Meters c -> BspTree -> BspTree
translateBy vector tree =
    BspTree.mapFaces (PlaneBasedFace.translate vector) tree
