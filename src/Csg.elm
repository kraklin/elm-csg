module Csg exposing (..)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BspTree exposing (BspTree, Face)
import Color exposing (Color)
import Direction3d
import Length exposing (Length, Meters)
import LineSegment3d exposing (LineSegment3d)
import List.NonEmpty as NonEmpty
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


toFace : List (Point3d Meters c) -> Maybe (Face c)
toFace points =
    let
        maybeNormal tri =
            tri
                |> Triangle3d.normalDirection
    in
    case points of
        v1 :: v2 :: v3 :: rest ->
            maybeNormal (Triangle3d.from v1 v2 v3)
                |> Maybe.map
                    (\normal ->
                        Face (NonEmpty.fromCons v1 (v2 :: v3 :: rest))
                            normal
                            defaultColor
                    )

        _ ->
            Nothing



-- Solids construction


cube : Length -> Csg coordinates
cube size =
    cuboid { width = size, height = size, depth = size }


cuboid : { width : Length, height : Length, depth : Length } -> Csg coordinates
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
            Direction3d.z

        backNormal =
            Direction3d.negativeZ

        topNormal =
            Direction3d.y

        bottomNormal =
            Direction3d.negativeY

        leftNormal =
            Direction3d.negativeX

        rightNormal =
            Direction3d.x

        front =
            Face ( a, [ d, c, b ] ) frontNormal defaultColor

        back =
            Face ( e, [ f, g, h ] ) backNormal defaultColor

        top =
            Face ( b, [ c, g, f ] ) topNormal defaultColor

        bottom =
            Face ( a, [ e, h, d ] ) bottomNormal defaultColor

        left =
            Face ( a, [ b, f, e ] ) leftNormal defaultColor

        right =
            Face ( c, [ d, h, g ] ) rightNormal defaultColor
    in
    BspTree.build [ front, back, top, bottom, left, right ]
        |> Csg


type alias SphereSettings =
    { slices : Int
    , stacks : Int
    }


sphereDefaults : SphereSettings
sphereDefaults =
    { slices = 16
    , stacks = 8
    }


sphere : Length -> Csg c
sphere =
    sphereWith sphereDefaults


sphereWith : SphereSettings -> Length -> Csg c
sphereWith { slices, stacks } radius =
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
            Point3d.xyz (Length.meters 0) radius (Length.meters 0)

        vertex ( it, ip ) =
            northPoint
                |> Point3d.rotateAround Axis3d.x (Quantity.multiplyBy (toFloat it) deltaTheta)
                |> Point3d.rotateAround Axis3d.y (Quantity.multiplyBy (toFloat ip) deltaPhi)

        middle : List (Face c)
        middle =
            List.range 1 (stacks_ - 2)
                |> List.map
                    (\latId ->
                        List.range 0 (slices_ - 1)
                            |> List.filterMap
                                (\long ->
                                    toFace
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
                        toFace [ i1, i2, i3 ]
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
                        toFace [ i1, i2, i3 ]
                    )
    in
    (northCap ++ middle ++ southCap)
        |> BspTree.build
        |> Csg


cylinder : Length -> Point3d Meters c -> Point3d Meters c -> Csg c
cylinder radius start end =
    coneWith
        { slices = 16 }
        { bottomRadius = radius
        , topRadius = radius
        , bottomPoint = start
        , topPoint = end
        }


cone :
    Length
    -> Length
    -> Csg c
cone radius height =
    coneWith { slices = 16 }
        { bottomRadius = radius
        , bottomPoint = Point3d.origin
        , topPoint = Point3d.xyz (Length.meters 0) (Length.meters 0) height
        , topRadius = Length.meters 0
        }


coneWith :
    { slices : Int }
    ->
        { bottomRadius : Length
        , bottomPoint : Point3d Meters c
        , topPoint : Point3d Meters c
        , topRadius : Length
        }
    -> Csg c
coneWith { slices } { bottomRadius, bottomPoint, topRadius, topPoint } =
    let
        vector =
            Vector3d.from bottomPoint topPoint

        initialPointBottom =
            Point3d.translateBy
                (Vector3d.perpendicularTo vector
                    |> Vector3d.scaleTo bottomRadius
                )
                bottomPoint

        initialPointTop =
            Point3d.translateBy
                (Vector3d.perpendicularTo vector
                    |> Vector3d.scaleTo topRadius
                )
                topPoint

        deltaPhi =
            Angle.turns (1 / toFloat slices)

        maybeRotationAxis =
            Vector3d.direction vector
                |> Maybe.map (Axis3d.through bottomPoint)

        bottomPoints =
            List.range 0 (slices - 1)
                |> List.filterMap
                    (\idx ->
                        maybeRotationAxis
                            |> Maybe.map
                                (\axis ->
                                    ( Point3d.rotateAround axis
                                        (deltaPhi
                                            |> Quantity.multiplyBy (toFloat idx)
                                        )
                                        initialPointBottom
                                    , Point3d.rotateAround axis
                                        (deltaPhi
                                            |> Quantity.multiplyBy (toFloat (idx + 1))
                                        )
                                        initialPointBottom
                                    )
                                )
                    )

        topPoints =
            List.range 0 (slices - 1)
                |> List.filterMap
                    (\idx ->
                        maybeRotationAxis
                            |> Maybe.map
                                (\axis ->
                                    ( Point3d.rotateAround axis
                                        (deltaPhi
                                            |> Quantity.multiplyBy (toFloat idx)
                                        )
                                        initialPointTop
                                    , Point3d.rotateAround axis
                                        (deltaPhi
                                            |> Quantity.multiplyBy (toFloat (idx + 1))
                                        )
                                        initialPointTop
                                    )
                                )
                    )

        bottom =
            bottomPoints
                |> List.filterMap
                    (\( p1, p2 ) ->
                        toFace [ p1, bottomPoint, p2 ]
                    )

        top =
            topPoints
                |> List.filterMap
                    (\( p1, p2 ) ->
                        toFace [ p1, p2, topPoint ]
                    )

        sides =
            List.map2
                (\( b1, b2 ) ( t1, t2 ) ->
                    [ b1, b2, t2, t1 ]
                )
                bottomPoints
                topPoints
                |> List.filterMap toFace
    in
    (bottom ++ sides ++ top)
        |> BspTree.build
        |> Csg



-- Operations


subtractFrom : Csg c -> Csg c -> Csg c
subtractFrom (Csg t2) (Csg t1) =
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


intersectWith : Csg c -> Csg c -> Csg c
intersectWith (Csg t1) (Csg t2) =
    let
        a =
            t1
                |> BspTree.invert
                |> BspTree.clip t2
                |> BspTree.invert
                |> BspTree.toFaces

        b =
            t2
                |> BspTree.invert
                |> BspTree.clip t1
                |> BspTree.invert
                |> BspTree.toFaces
    in
    (a ++ b)
        |> BspTree.build
        |> Csg


union : Csg c -> Csg c -> Csg c
union (Csg t1) (Csg t2) =
    let
        a =
            t1
                |> BspTree.clip
                    (t2
                        |> BspTree.invert
                        |> BspTree.clip t1
                    )
                |> BspTree.toFaces

        b =
            t2
                |> BspTree.clip
                    (t1
                        |> BspTree.invert
                        |> BspTree.clip t2
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


group : List (Csg c) -> Csg c
group csgs =
    csgs
        |> List.foldl (\(Csg tree) acc -> BspTree.toFaces tree ++ acc) []
        |> BspTree.build
        |> Csg


translateBy : Vector3d Meters c -> Csg c -> Csg c
translateBy vector (Csg tree) =
    tree
        |> BspTree.translate vector
        |> Csg


rotateAround : Axis3d Meters c -> Angle -> Csg c -> Csg c
rotateAround axis angle (Csg tree) =
    tree
        |> BspTree.rotateAround axis angle
        |> Csg


scaleAbout : Point3d Meters c -> Float -> Csg c -> Csg c
scaleAbout origin factor (Csg tree) =
    tree
        |> BspTree.scaleAbout origin factor
        |> Csg


scaleBy : Vector3d Meters c -> Csg c -> Csg c
scaleBy vector (Csg tree) =
    tree
        |> BspTree.scaleBy vector
        |> Csg


withColor : Color -> Csg c -> Csg c
withColor color (Csg tree) =
    tree
        |> BspTree.mapFaces (\f -> { f | color = color })
        |> Csg



-- Rendering to Mesh or Wireframe


type alias Triangle c =
    ( Vertex c
    , Vertex c
    , Vertex c
    )


type alias Vertex c =
    { position : Point3d Length.Meters c
    , normal : Vector3d Unitless c
    , color : Color
    }


facesToTriangles : Face coordinates -> List (Triangle coordinates)
facesToTriangles ({ normalDirection, color } as face) =
    let
        withNormal ( v1, v2, v3 ) =
            ( { position = v1, normal = Direction3d.toVector normalDirection, color = color }
            , { position = v2, normal = Direction3d.toVector normalDirection, color = color }
            , { position = v3, normal = Direction3d.toVector normalDirection, color = color }
            )
    in
    case BspTree.allPoints face of
        p1 :: p2 :: rest ->
            rest
                |> List.foldl
                    (\pNext { pPrevious, triangles } ->
                        { pPrevious = pNext, triangles = withNormal ( p1, pPrevious, pNext ) :: triangles }
                    )
                    { pPrevious = p2, triangles = [] }
                |> .triangles

        _ ->
            []


toMesh : Csg coordinates -> List (Triangle coordinates)
toMesh (Csg tree) =
    tree
        |> BspTree.toFaces
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
                    |> Vector3d.scaleBy 0.2
                )
                (centroid ( v1, v2, v3 ))

        triangleSegments ( v1, v2, v3 ) =
            [ LineSegment3d.from v1.position v2.position
            , LineSegment3d.from v2.position v3.position
            , LineSegment3d.from v3.position v1.position

            --, LineSegment3d.from (centroid ( v1, v2, v3 )) (normalEnd ( v1, v2, v3 ))
            ]
    in
    tree
        |> BspTree.toFaces
        |> List.map facesToTriangles
        |> List.concat
        |> List.map triangleSegments
        |> List.concat
