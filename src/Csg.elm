module Csg exposing (..)

import Color exposing (Color)
import Length
import LineSegment3d exposing (LineSegment3d)
import Point3d exposing (Point3d)
import Quantity
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


type XYZCoordinates
    = XYZCoordinates


type alias Triangle coordinates =
    ( Vertex coordinates
    , Vertex coordinates
    , Vertex coordinates
    )


type alias Vertex coordinates =
    { position :
        Point3d Length.Meters coordinates
    , normal : Vector3d Quantity.Unitless coordinates
    }


type alias Face coordinates =
    { triangles : ( Triangle3d Length.Meters coordinates, List (Triangle3d Length.Meters coordinates) )
    , normal : Vector3d Quantity.Unitless coordinates
    }


type CsgOperation
    = Union


type CsgPlacement coordinates
    = Translate (Vector3d Length.Meters coordinates)


type Csg coords
    = Operation CsgOperation (Csg coords) (Csg coords)
    | Placement (CsgPlacement coords) (Csg coords)
    | Primitive (List (Face coords))


facesToTriangles : Face coordinates -> List (Triangle coordinates)
facesToTriangles { triangles, normal } =
    let
        ( first, rest ) =
            triangles

        withNormal ( v1, v2, v3 ) =
            ( { position = v1, normal = normal }
            , { position = v2, normal = normal }
            , { position = v3, normal = normal }
            )
    in
    first :: rest |> List.map (\triangle -> Triangle3d.vertices triangle |> withNormal)


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
            Vector3d.unitless 0 0 -1

        backNormal =
            Vector3d.unitless 0 0 1

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
            Face ( triangle a b c, [ triangle a c d ] ) frontNormal

        back =
            Face ( triangle e f g, [ triangle e g h ] ) backNormal

        top =
            Face ( triangle b f g, [ triangle b g c ] ) topNormal

        bottom =
            Face ( triangle a e h, [ triangle a h d ] ) bottomNormal

        left =
            Face ( triangle e f b, [ triangle e b a ] ) leftNormal

        right =
            Face ( triangle d c g, [ triangle d g h ] ) rightNormal
    in
    Primitive [ front, back, top, bottom, left, right ]


union : Csg coords -> Csg coords -> Csg coords
union csgLeft csgRight =
    Operation Union csgLeft csgRight


translate : Vector3d Length.Meters coords -> Csg coords -> Csg coords
translate vector csg =
    Placement (Translate vector) csg


build : Csg coordinates -> List (Triangle coordinates)
build csg =
    case csg of
        Primitive faces ->
            faces
                |> List.map facesToTriangles
                |> List.concat

        Placement placement csgToPlace ->
            case placement of
                Translate v ->
                    build csgToPlace
                        |> List.map
                            (\( v1, v2, v3 ) ->
                                ( { v1 | position = Point3d.translateBy v v1.position }
                                , { v2 | position = Point3d.translateBy v v2.position }
                                , { v3 | position = Point3d.translateBy v v3.position }
                                )
                            )

        Operation op leftCsg rightCsg ->
            case op of
                Union ->
                    build leftCsg ++ build rightCsg


toMesh : Csg coordinates -> TriangularMesh (Vertex coordinates)
toMesh csg =
    build csg
        |> TriangularMesh.triangles


toLines : Csg coordinates -> List (LineSegment3d Length.Meters coordinates)
toLines csg =
    let
        triangleSegments ( v1, v2, v3 ) =
            [ LineSegment3d.from v1.position v2.position
            , LineSegment3d.from v2.position v3.position
            , LineSegment3d.from v3.position v1.position
            ]
    in
    build csg
        |> List.map triangleSegments
        |> List.concat
