module Csg exposing (..)

import Angle
import Axis3d
import BspTree exposing (BspTree, Face)
import Color exposing (Color)
import Direction3d
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
            Face ( triangle a c b, [ triangle a d c ] ) frontNormal defaultColor

        back =
            Face ( triangle e f g, [ triangle e g h ] ) backNormal defaultColor

        top =
            Face ( triangle b g f, [ triangle b c g ] ) topNormal defaultColor

        bottom =
            Face ( triangle a e h, [ triangle a h d ] ) bottomNormal defaultColor

        left =
            Face ( triangle e a b, [ triangle e b f ] ) leftNormal defaultColor

        right =
            Face ( triangle d h g, [ triangle d g c ] ) rightNormal defaultColor
    in
    BspTree.build [ front, back, top, bottom, left, right ]
        |> Csg


sphere : Csg c
sphere =
    sphereWith 16 8 0.7


sphereWith : Int -> Int -> Float -> Csg c
sphereWith slices stacks radius =
    let
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
            Point3d.meters 0 radius 0

        vertex ( it, ip ) =
            northPoint
                |> Point3d.rotateAround Axis3d.x (Quantity.multiplyBy (toFloat it) deltaTheta)
                |> Point3d.rotateAround Axis3d.y (Quantity.multiplyBy (toFloat ip) deltaPhi)

        triangles =
            List.range 1 (stacks_ - 2)
                |> List.map
                    (\latId ->
                        List.range 0 (slices_ - 1)
                            |> List.map
                                (\long ->
                                    { i1 = vertex ( latId, long )
                                    , i2 = vertex ( latId + 1, long )
                                    , i3 = vertex ( latId, long + 1 )
                                    , i4 = vertex ( latId + 1, long + 1 )
                                    }
                                )
                    )
                |> List.concat
                |> List.map
                    (\idx ->
                        [ Triangle3d.from idx.i1 idx.i2 idx.i3
                        , Triangle3d.from idx.i2 idx.i4 idx.i3
                        ]
                    )
                |> List.concat
                |> List.filterMap toFace

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
                        Triangle3d.from i1 i2 i3
                            |> toFace
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
                        Triangle3d.from i1 i2 i3
                            |> toFace
                    )

        toFace tri =
            let
                maybeNormal =
                    tri
                        |> Triangle3d.normalDirection
                        |> Maybe.map (\dir -> Direction3d.toVector dir)
            in
            maybeNormal
                |> Maybe.map
                    (\normal ->
                        Face ( tri, [] )
                            normal
                            defaultColor
                    )
    in
    (northCap ++ triangles ++ southCap)
        |> BspTree.build
        |> Csg



-- Operations


subtract : Csg c -> Csg c -> Csg c
subtract (Csg t1) (Csg t2) =
    let
        a =
            t1
                |> BspTree.invert
                |> BspTree.clip t2

        b =
            t2
                |> BspTree.clip a
                |> BspTree.toFaces
    in
    (BspTree.toFaces a ++ b)
        |> BspTree.build
        |> Csg


intersect : Csg c -> Csg c -> Csg c
intersect (Csg a) (Csg b) =
    {-
       intersect: function(csg) {
          var a = new CSG.Node(this.clone().polygons);
          var b = new CSG.Node(csg.clone().polygons);
          a.invert();
          b.clipTo(a);
          b.invert();
          a.clipTo(b);
          b.clipTo(a);
          a.build(b.allPolygons());
          a.invert();
          return CSG.fromPolygons(a.allPolygons());
        },

    -}
    let
        t1 =
            a
                |> BspTree.invert
                |> BspTree.clip b
                |> BspTree.invert
                |> BspTree.toFaces

        t2 =
            b
                |> BspTree.invert
                |> BspTree.clip a
                |> BspTree.invert
                |> BspTree.toFaces
    in
    BspTree.build (t1 ++ t2)
        |> Csg


union : Csg c -> Csg c -> Csg c
union (Csg t1) (Csg t2) =
    let
        a_ =
            t1
                |> BspTree.invert
                |> BspTree.clip t2

        b =
            t2
                |> BspTree.clip
                    (t1
                        |> BspTree.invert
                        |> BspTree.clip t2
                    )
                |> BspTree.toFaces

        a =
            t1
                |> BspTree.clip
                    (t2
                        |> BspTree.invert
                        |> BspTree.clip t1
                    )
                |> BspTree.toFaces
    in
    (a ++ b)
        |> BspTree.build
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
        centroid : Triangle c -> Point3d Meters c
        centroid ( v1, v2, v3 ) =
            Point3d.centroid3 v1.position v2.position v3.position

        normalEnd : Triangle c -> Point3d Meters c
        normalEnd ( v1, v2, v3 ) =
            Point3d.translateBy
                (Vector3d.toUnitless v1.normal
                    |> Vector3d.fromMeters
                )
                (centroid ( v1, v2, v3 ))

        triangleSegments ( v1, v2, v3 ) =
            [ LineSegment3d.from v1.position v2.position
            , LineSegment3d.from v2.position v3.position
            , LineSegment3d.from v3.position v1.position
            , LineSegment3d.from (centroid ( v1, v2, v3 )) (normalEnd ( v1, v2, v3 ))
            ]
    in
    tree
        |> BspTree.toFaces
        |> List.map facesToTriangles
        |> List.concat
        |> List.map triangleSegments
        |> List.concat
