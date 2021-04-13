module Csg exposing (..)

import Bitwise
import Color exposing (Color)
import Direction3d
import Length exposing (Meters)
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
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


fromTriangles : Vector3d Quantity.Unitless coordinates -> List (Triangle3d Meters coordinates) -> Maybe (Face coordinates)
fromTriangles normal triangles =
    case triangles of
        [] ->
            Nothing

        first :: rest ->
            Just <| { triangles = ( first, rest ), normal = normal }


cube : Length.Length -> Csg coordinates
cube size =
    cuboid { width = size, height = size, depth = size }


simpleFace t =
    let
        a =
            Point3d.meters 0 0 1

        b =
            Point3d.meters 0 0 -1

        c =
            Point3d.meters t 0 -1

        d =
            Point3d.meters t 0 1

        frontNormal =
            Vector3d.unitless 0 1 0
    in
    Face ( Triangle3d.from a b c, [ Triangle3d.from a c d ] ) frontNormal


simplePrimitive x =
    Primitive [ simpleFace x ]


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



-- math stuff


type Clasify c
    = CoplanarFront (Face c)
    | CoplanarBack (Face c)
    | Front (Face c)
    | Back (Face c)
    | Spanning { front : Face c, back : Face c }



-- make it for triangles


cutByPlane : Plane3d Meters c -> Vector3d Quantity.Unitless c -> Triangle3d Meters c -> { front : List (Triangle3d Meters c), back : List (Triangle3d Meters c) }
cutByPlane plane faceNormal triangle =
    let
        planeNormal =
            Plane3d.normalDirection plane |> Direction3d.toVector

        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle

        distanceFromPlane =
            Point3d.signedDistanceFrom plane

        order d =
            Quantity.compare d Quantity.zero

        ( d1, d2, d3 ) =
            ( distanceFromPlane p1, distanceFromPlane p2, distanceFromPlane p3 )

        ( o1, o2, o3 ) =
            ( order d1, order d2, order d3 )

        orderToNumber o =
            case o of
                EQ ->
                    0

                GT ->
                    1

                LT ->
                    2

        positioning =
            Bitwise.or (orderToNumber o1) (orderToNumber o2)
                |> Bitwise.or (orderToNumber o3)

        toPointInfo p d o =
            { point = p, distance = d, order = o }

        pa =
            toPointInfo p1 d1 o1

        pb =
            toPointInfo p2 d2 o2

        pc =
            toPointInfo p3 d3 o3

        pairs =
            [ ( pa, pb ), ( pa, pc ), ( pb, pc ) ]

        onDifferentSides =
            pairs |> List.filter (\( first, second ) -> first.order /= second.order)

        intersectionPoints =
            onDifferentSides
                |> List.map
                    (\( a, b ) ->
                        let
                            va =
                                Vector3d.from Point3d.origin a.point

                            --(this.w - this.normal.dot(vi.pos)) / this.normal.dot(vj.pos.minus(vi.pos));
                            vb =
                                Vector3d.from Point3d.origin b.point

                            vba =
                                Vector3d.minus vb va

                            w =
                                Vector3d.dot planeNormal (Vector3d.from Point3d.origin (Plane3d.originPoint plane))

                            t =
                                Quantity.ratio (Quantity.minus w (Vector3d.dot planeNormal va)) (Vector3d.dot planeNormal vba)
                        in
                        Point3d.translateBy (Vector3d.interpolateFrom va vb t) Point3d.origin
                    )

        ( onFront, onBack ) =
            ( [ pa, pb, pc ] |> List.filter (\p -> p.order == GT) |> List.map .point
            , [ pa, pb, pc ] |> List.filter (\p -> p.order == LT) |> List.map .point
            )

        makeTriangles points =
            let
                vectorLength a b =
                    Vector3d.from a b |> Vector3d.length

                sortPoints a i1 i2 =
                    if vectorLength a i2 |> Quantity.greaterThan (vectorLength a i1) then
                        ( i1, i2 )

                    else
                        ( i2, i1 )
            in
            case intersectionPoints of
                [ i1, i2 ] ->
                    case points of
                        [ onePoint ] ->
                            [ Triangle3d.from onePoint i1 i2 ]

                        [ firstPoint, secondPoint ] ->
                            [ Triangle3d.from firstPoint (sortPoints firstPoint i1 i2 |> Tuple.first) (sortPoints firstPoint i1 i2 |> Tuple.second)
                            , Triangle3d.from firstPoint (sortPoints firstPoint i1 i2 |> Tuple.second) secondPoint
                            ]

                        _ ->
                            []

                _ ->
                    []
    in
    { front = makeTriangles onFront, back = makeTriangles onBack }


clasify : Plane3d Meters c -> Face c -> Maybe (Clasify c)
clasify plane face =
    let
        ( firstTriangle, rest ) =
            face.triangles

        faceNormal =
            face.normal

        planeNormal =
            Plane3d.normalDirection plane |> Direction3d.toVector

        ( p1, p2, p3 ) =
            Triangle3d.vertices firstTriangle

        distanceFromPlane =
            Point3d.signedDistanceFrom plane

        order d =
            Quantity.compare d Quantity.zero

        ( d1, d2, d3 ) =
            ( distanceFromPlane p1, distanceFromPlane p2, distanceFromPlane p3 )

        ( o1, o2, o3 ) =
            ( order d1, order d2, order d3 )

        orderToNumber o =
            case o of
                EQ ->
                    0

                GT ->
                    1

                LT ->
                    2

        positioning =
            Bitwise.or (orderToNumber o1) (orderToNumber o2)
                |> Bitwise.or (orderToNumber o3)

        splittedFaces =
            firstTriangle
                :: rest
                |> List.map (cutByPlane plane planeNormal)
                |> List.foldl (\result acc -> { acc | front = acc.front ++ result.front, back = acc.back ++ result.back }) { front = [], back = [] }
                |> (\triangles ->
                        { front = fromTriangles faceNormal triangles.front
                        , back = fromTriangles faceNormal triangles.back
                        }
                   )
    in
    case positioning of
        0 ->
            if Quantity.lessThan (Vector3d.dot planeNormal faceNormal) Quantity.zero then
                Just <| CoplanarFront face

            else
                Just <| CoplanarBack face

        1 ->
            Just <| Front face

        2 ->
            Just <| Back face

        3 ->
            case ( splittedFaces.front, splittedFaces.back ) of
                ( Just front, Just back ) ->
                    Spanning { front = front, back = back }
                        |> Just

                _ ->
                    Nothing

        _ ->
            Nothing


clipByPlane : Plane3d Meters c -> Csg c -> Csg c
clipByPlane plane csg =
    case csg of
        Primitive faces ->
            faces
                |> List.map (getFrontFaces plane)
                |> List.concat
                |> Primitive

        _ ->
            csg


getFrontFaces : Plane3d Meters c -> Face c -> List (Face c)
getFrontFaces plane face =
    case clasify plane face of
        Just (Front f) ->
            [ f ]

        Just (Spanning faces) ->
            [ faces.front ]

        Just (CoplanarFront f) ->
            [ f ]

        _ ->
            []
