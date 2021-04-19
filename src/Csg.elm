module Csg exposing (..)

import BspTree exposing (BspTree, Face)
import Color exposing (Color)
import Length exposing (Meters)
import LineSegment3d exposing (LineSegment3d)
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import Triangle3d
import Vector3d exposing (Vector3d)


type XYZCoordinates
    = XYZCoordinates


type Csg c
    = Csg (BspTree c)


defaultColor : Color
defaultColor =
    Color.yellow



-- Solids construction


simpleFace : Float -> Csg c
simpleFace t =
    let
        a =
            Point3d.meters 0 0 0

        b =
            Point3d.meters 0 0 -1

        c =
            Point3d.meters t 0 -1

        d =
            Point3d.meters t 0 0

        frontNormal =
            Vector3d.unitless 0 1 0
    in
    Face ( Triangle3d.from a b c, [ Triangle3d.from a c d ] ) frontNormal defaultColor
        |> List.singleton
        |> BspTree.build
        |> Csg


pyramid : Csg c
pyramid =
    let
        v =
            Point3d.meters 0 1 0

        a =
            Point3d.meters -1 0 1

        b =
            Point3d.meters 1 0 1

        c =
            Point3d.meters -1 0 -1

        d =
            Point3d.meters 1 0 -1

        bottomNormal =
            Vector3d.unitless 0 -1 0

        frontNormal =
            Vector3d.unitless 0 1 1

        backNormal =
            Vector3d.unitless 0 1 -1

        leftNormal =
            Vector3d.unitless -1 1 0

        rightNormal =
            Vector3d.unitless 1 1 0

        front =
            Face ( Triangle3d.from v a b, [] ) frontNormal defaultColor

        back =
            Face ( Triangle3d.from v d c, [] ) backNormal defaultColor

        left =
            Face ( Triangle3d.from v c a, [] ) leftNormal defaultColor

        right =
            Face ( Triangle3d.from v b d, [] ) rightNormal defaultColor

        bottom =
            Face ( Triangle3d.from a c d, [ Triangle3d.from d b a ] ) bottomNormal defaultColor
    in
    BspTree.build [ front, back, bottom, left, right ]
        |> Csg


cube : Length.Length -> Csg coordinates
cube size =
    cuboid { width = size, height = size, depth = size }


cuboid : { width : Length.Length, height : Length.Length, depth : Length.Length } -> Csg coordinates
cuboid { width, height, depth } =
    let
        z =
            Length.meters 0

        a =
            Point3d.meters 0 0 0

        b =
            Point3d.xyz z height z

        c =
            Point3d.xyz width height z

        d =
            Point3d.xyz width z z

        e =
            Point3d.xyz z z (Quantity.negate depth)

        f =
            Point3d.xyz z height (Quantity.negate depth)

        g =
            Point3d.xyz width height (Quantity.negate depth)

        h =
            Point3d.xyz width z (Quantity.negate depth)

        frontNormal =
            Vector3d.unitless 0 0 1

        backNormal =
            Vector3d.unitless 0 0 -1

        topNormal =
            Vector3d.unitless 0 1 0

        bottomNormal =
            Vector3d.unitless 0 -1 0

        leftNormal =
            Vector3d.unitless -1 0 0

        rightNormal =
            Vector3d.unitless 1 0 0

        triangle =
            Triangle3d.from

        front =
            Face ( triangle a b c, [ triangle a c d ] ) frontNormal defaultColor

        back =
            Face ( triangle e f g, [ triangle e g h ] ) backNormal defaultColor

        top =
            Face ( triangle b f g, [ triangle b g c ] ) topNormal defaultColor

        bottom =
            Face ( triangle a e h, [ triangle a h d ] ) bottomNormal defaultColor

        left =
            Face ( triangle e f b, [ triangle e b a ] ) leftNormal defaultColor

        right =
            Face ( triangle d c g, [ triangle d g h ] ) rightNormal defaultColor
    in
    BspTree.build [ front, back, top, bottom, left, right ]
        |> Csg



-- Operations


subtraction : Csg c -> Csg c -> Csg c
subtraction (Csg t1) (Csg t2) =
    let
        a =
            t1
                |> BspTree.invert
                |> BspTree.clip t2
    in
    t2
        |> BspTree.clip a
        |> Csg


invert : Csg c -> Csg c
invert (Csg tree) =
    tree
        |> BspTree.invert
        |> Csg


translate : Vector3d Meters c -> Csg c -> Csg c
translate vector (Csg tree) =
    tree
        |> BspTree.translate vector
        |> Csg


withColor : Color -> Csg c -> Csg c
withColor color (Csg tree) =
    BspTree.mapFaces (\f -> { f | color = color }) tree
        |> Csg



-- Rendering to Mesh or Wireframe


type alias Triangle c =
    ( Vertex c
    , Vertex c
    , Vertex c
    )


type alias Vertex c =
    { position :
        Point3d Length.Meters c
    , normal : Vector3d Unitless c
    , color : Color
    }


facesToTriangles : Face coordinates -> List (Triangle coordinates)
facesToTriangles { triangles, normal, color } =
    let
        ( first, rest ) =
            triangles

        withNormal ( v1, v2, v3 ) =
            ( { position = v1, normal = normal, color = color }
            , { position = v2, normal = normal, color = color }
            , { position = v3, normal = normal, color = color }
            )
    in
    first :: rest |> List.map (\triangle -> Triangle3d.vertices triangle |> withNormal)


toMesh : Csg coordinates -> List (Triangle coordinates)
toMesh (Csg tree) =
    BspTree.toFaces tree
        |> List.map facesToTriangles
        |> List.concat


toLines : Csg coordinates -> List (LineSegment3d Length.Meters coordinates)
toLines (Csg tree) =
    let
        triangleSegments ( v1, v2, v3 ) =
            [ LineSegment3d.from v1.position v2.position
            , LineSegment3d.from v2.position v3.position
            , LineSegment3d.from v3.position v1.position
            ]
    in
    BspTree.toFaces tree
        |> List.map facesToTriangles
        |> List.concat
        |> List.map triangleSegments
        |> List.concat
