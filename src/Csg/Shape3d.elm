module Csg.Shape3d exposing
    ( Shape3d
    , cone
    , coneWith
    , cube
    , cuboid
    , cylinder
    , cylinderFromTo
    , geodesicSphere
    , group
    , intersectWith
    , moveBackward
    , moveDown
    , moveForward
    , moveLeft
    , moveRight
    , moveUp
    , rotateAround
    , roundedCuboid
    , scaleAbout
    , scaleBy
    , sphere
    , sphereWith
    , subtractFrom
    , toFaces
    , torus
    , translateBy
    , unionWith
    , withTag
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BspTree exposing (BspTree, Face)
import Direction3d
import Length exposing (Length, Meters)
import List.Extra as List
import List.NonEmpty as NonEmpty
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import Triangle3d
import Vector3d exposing (Vector3d)



-- Solids construction


type Shape3d tag c
    = Shape3d (BspTree tag c)


toFaces : Shape3d tag c -> List (Face tag c)
toFaces (Shape3d tree) =
    BspTree.toFaces tree


toTree : Shape3d tag c -> BspTree tag c
toTree (Shape3d tree) =
    tree


toFace : List (Point3d Meters c) -> Maybe (Face tag c)
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
                            Nothing
                    )

        _ ->
            Nothing


cuboid : { width : Length, height : Length, depth : Length } -> Shape3d tag c
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
            toFace [ a, d, c, b ]

        back =
            toFace [ e, f, g, h ]

        top =
            toFace [ b, c, g, f ]

        bottom =
            toFace [ a, e, h, d ]

        left =
            toFace [ a, b, f, e ]

        right =
            toFace [ c, d, h, g ]
    in
    [ front, back, top, bottom, left, right ]
        |> List.filterMap identity
        |> BspTree.build
        |> Shape3d


cube : Length -> Shape3d tag c
cube size =
    cuboid { width = size, height = size, depth = size }


roundedCuboid : { width : Length, height : Length, depth : Length, radius : Length } -> Shape3d tag c
roundedCuboid { width, height, depth, radius } =
    if Quantity.lessThanOrEqualTo Quantity.zero radius then
        cuboid { width = width, height = height, depth = depth }

    else
        let
            z =
                Length.meters 0

            minusRadius =
                Quantity.minus radius

            -- front
            fa =
                Point3d.xyz radius z radius

            fd =
                Point3d.xyz (minusRadius width) z radius

            fc =
                Point3d.xyz (minusRadius width) z (minusRadius height)

            fb =
                Point3d.xyz radius z (minusRadius height)

            front =
                toFace [ fa, fd, fc, fb ]

            -- back
            be =
                Point3d.xyz radius depth radius

            bf =
                Point3d.xyz radius depth (minusRadius height)

            bg =
                Point3d.xyz (minusRadius width) depth (minusRadius height)

            bh =
                Point3d.xyz (minusRadius width) depth radius

            back =
                toFace [ be, bf, bg, bh ]

            -- top
            tb =
                Point3d.xyz radius radius height

            tc =
                Point3d.xyz (minusRadius width) radius height

            tg =
                Point3d.xyz (minusRadius width) (minusRadius depth) height

            tf =
                Point3d.xyz radius (minusRadius depth) height

            top =
                toFace [ tb, tc, tg, tf ]

            -- bottom
            bta =
                Point3d.xyz radius radius z

            bte =
                Point3d.xyz radius (minusRadius depth) z

            bth =
                Point3d.xyz (minusRadius width) (minusRadius depth) z

            btd =
                Point3d.xyz (minusRadius width) radius z

            bottom =
                toFace [ bta, bte, bth, btd ]

            --left
            la =
                Point3d.xyz z radius radius

            lb =
                Point3d.xyz z radius (minusRadius height)

            lf =
                Point3d.xyz z (minusRadius depth) (minusRadius height)

            le =
                Point3d.xyz z (minusRadius depth) radius

            left =
                toFace [ la, lb, lf, le ]

            -- right
            rc =
                Point3d.xyz width radius (minusRadius height)

            rd =
                Point3d.xyz width radius radius

            rh =
                Point3d.xyz width (minusRadius depth) radius

            rg =
                Point3d.xyz width (minusRadius depth) (minusRadius height)

            right =
                toFace [ rc, rd, rh, rg ]

            -- edges
            frontTopEdge =
                toFace [ fc, tc, tb, fb ]

            frontBottomEdge =
                toFace [ fa, bta, btd, fd ]

            backBottomEdge =
                toFace [ be, bh, bth, bte ]

            backTopEdge =
                toFace [ bf, tf, tg, bg ]

            leftBottomEdge =
                toFace [ bta, la, le, bte ]

            leftTopEdge =
                toFace [ lb, tb, tf, lf ]

            rightTopEdge =
                toFace [ rc, rg, tg, tc ]

            rightBottomEdge =
                toFace [ btd, bth, rh, rd ]

            frontLeftEdge =
                toFace [ la, fa, fb, lb ]

            frontRightEdge =
                toFace [ fd, rd, rc, fc ]

            backRightEdge =
                toFace [ rh, bh, bg, rg ]

            backLeftEdge =
                toFace [ be, le, lf, bf ]
        in
        [ front, back, top, bottom, left, right ]
            ++ [ frontTopEdge, frontBottomEdge, backBottomEdge, backTopEdge, leftBottomEdge, leftTopEdge, rightTopEdge, rightBottomEdge, frontLeftEdge, frontRightEdge, backRightEdge, backLeftEdge ]
            |> List.filterMap identity
            |> BspTree.build
            |> Shape3d



-- Sphere


type alias SphereSettings =
    { slices : Int
    , stacks : Int
    , radius : Length
    }


sphereDefaultSettings : SphereSettings
sphereDefaultSettings =
    { slices = 16
    , stacks = 8
    , radius = Length.meters 0.5
    }


sphere : Length -> Shape3d tag c
sphere radius =
    sphereWith { sphereDefaultSettings | radius = radius }



-- TODO - orientation of top should be Z axis, not Y


sphereWith : SphereSettings -> Shape3d tag c
sphereWith { slices, stacks, radius } =
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

        middle : List (Face tag c)
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
        |> Shape3d



-- Cone


type alias ConeSettings c =
    { bottomRadius : Length
    , bottomPoint : Point3d Meters c
    , topPoint : Point3d Meters c
    , topRadius : Length
    , slices : Int
    }


coneDefaultSettings : ConeSettings c
coneDefaultSettings =
    { bottomRadius = Length.meters 0.25
    , bottomPoint = Point3d.origin
    , topPoint = Point3d.meters 0 0 1
    , topRadius = Length.meters 0
    , slices = 16
    }


cone :
    Length
    -> Length
    -> Shape3d tag c
cone radius height =
    coneWith
        { coneDefaultSettings
            | bottomRadius = radius
            , topPoint = Point3d.xyz (Length.meters 0) (Length.meters 0) height
        }


coneWith :
    ConeSettings c
    -> Shape3d tag c
coneWith { slices, bottomRadius, bottomPoint, topRadius, topPoint } =
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

        coneSides =
            List.map
                (\( b1, b2 ) ->
                    [ b1, b2, topPoint ]
                )
                bottomPoints
                |> List.filterMap toFace
    in
    (if Length.inMeters topRadius == 0 then
        bottom ++ coneSides

     else
        bottom ++ sides ++ top
    )
        |> BspTree.build
        |> Shape3d



-- Cylinder


cylinder : Length -> Length -> Shape3d tag c
cylinder radius height =
    coneWith
        { coneDefaultSettings
            | bottomRadius = radius
            , topRadius = radius
            , topPoint = Point3d.xyz Quantity.zero Quantity.zero height
        }


cylinderFromTo : Length -> Point3d Meters c -> Point3d Meters c -> Shape3d tag c
cylinderFromTo radius start end =
    coneWith
        { coneDefaultSettings
            | bottomRadius = radius
            , topRadius = radius
            , bottomPoint = start
            , topPoint = end
        }



-- Torus


torus : Length -> Length -> Shape3d tag c
torus innerRadius outerRadius =
    let
        stacks_ =
            16

        slices_ =
            16

        deltaOuter =
            Angle.turns (1 / toFloat stacks_)

        deltaInner =
            Angle.turns (1 / toFloat slices_)

        firstCircle =
            List.range 0 stacks_
                |> List.map
                    (\stackId ->
                        Point3d.xyz (Length.meters 0) innerRadius (Length.meters 0)
                            |> Point3d.rotateAround Axis3d.x (Quantity.multiplyBy (toFloat stackId) deltaOuter)
                            |> Point3d.translateBy (Vector3d.xyz (Length.meters 0) (Quantity.minus innerRadius outerRadius) (Length.meters 0))
                    )

        allCircles =
            List.range 0 (slices_ - 1)
                |> List.map
                    (\innerIdx ->
                        let
                            firstCirclePoints =
                                firstCircle
                                    |> List.map
                                        (Point3d.rotateAround Axis3d.z (Quantity.multiplyBy (toFloat (innerIdx - 1)) deltaInner))

                            secondCirclePoints =
                                firstCircle
                                    |> List.map (Point3d.rotateAround Axis3d.z (Quantity.multiplyBy (toFloat innerIdx) deltaInner))
                        in
                        List.map2 (\fli sli -> ( fli, sli ))
                            firstCirclePoints
                            secondCirclePoints
                            |> List.foldl
                                (\( p3, p4 ) { previous, faces } ->
                                    case previous of
                                        Nothing ->
                                            { previous = Just ( p3, p4 ), faces = faces }

                                        Just ( p1, p2 ) ->
                                            { previous = Just ( p3, p4 ), faces = toFace [ p1, p2, p4, p3 ] :: faces }
                                )
                                { previous = Nothing, faces = [] }
                            |> .faces
                            |> List.filterMap identity
                    )

        toFace_ points =
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
                                    Nothing
                            )

                _ ->
                    Nothing
    in
    allCircles
        |> List.concat
        |> BspTree.build
        |> Shape3d



-- geodesic sphere


goldenRatio : Float
goldenRatio =
    (1 + sqrt 5) / 2


icosahedron : List (Face tag c)
icosahedron =
    let
        v1 =
            Point3d.meters 0.850651 0.0 -0.525731

        v2 =
            Point3d.meters 0.850651 -0.0 0.525731

        v3 =
            Point3d.meters -0.850651 -0.0 0.525731

        v4 =
            Point3d.meters -0.850651 0.0 -0.525731

        v5 =
            Point3d.meters 0.0 -0.525731 0.850651

        v6 =
            Point3d.meters 0.0 0.525731 0.850651

        v7 =
            Point3d.meters 0.0 0.525731 -0.850651

        v8 =
            Point3d.meters 0.0 -0.525731 -0.850651

        v9 =
            Point3d.meters -0.525731 -0.850651 -0.0

        v10 =
            Point3d.meters 0.525731 -0.850651 -0.0

        v11 =
            Point3d.meters 0.525731 0.850651 0.0

        v12 =
            Point3d.meters -0.525731 0.850651 0.0
    in
    [ [ v1, v2, v10 ]
    , [ v2, v1, v11 ]
    , [ v7, v1, v8 ]
    , [ v11, v1, v7 ]
    , [ v8, v1, v10 ]
    , [ v6, v5, v2 ]
    , [ v5, v10, v2 ]
    , [ v6, v2, v11 ]
    , [ v3, v4, v9 ]
    , [ v4, v3, v12 ]
    , [ v3, v5, v6 ]
    , [ v5, v3, v9 ]
    , [ v3, v6, v12 ]
    , [ v4, v7, v8 ]
    , [ v7, v4, v12 ]
    , [ v9, v4, v8 ]
    , [ v10, v5, v9 ]
    , [ v12, v6, v11 ]
    , [ v11, v7, v12 ]
    , [ v9, v8, v10 ]
    ]
        |> List.filterMap toFace



-- Function to create faces from points.


facesFromPoints : List (Point3d Meters coordinates) -> List (Face tag coordinates)
facesFromPoints vertices =
    vertices
        |> List.greedyGroupsOf 3
        |> List.filterMap toFace



-- Subdivide each triangle into four smaller triangles.


subdivide : Length -> List (Face tag coordinates) -> List (Face tag coordinates)
subdivide radius faces =
    let
        projectToSphere point =
            Direction3d.from Point3d.origin point
                |> Maybe.map (\direction -> Point3d.translateIn direction radius Point3d.origin)
                |> Maybe.withDefault point
    in
    faces
        |> List.concatMap
            (\face ->
                case BspTree.allPoints face of
                    a :: b :: c :: rest ->
                        let
                            ab =
                                Point3d.midpoint a b
                                    |> projectToSphere

                            bc =
                                Point3d.midpoint b c
                                    |> projectToSphere

                            ca =
                                Point3d.midpoint c a
                                    |> projectToSphere

                            newPoints =
                                [ [ projectToSphere a, ab, ca ], [ ab, projectToSphere b, bc ], [ ca, bc, projectToSphere c ], [ ca, ab, bc ] ]
                        in
                        newPoints
                            |> List.filterMap toFace

                    _ ->
                        []
            )


scale : Length -> List (Face tag coordinates) -> List (Face tag coordinates)
scale radius faces =
    let
        projectToSphere point =
            Direction3d.from Point3d.origin point
                |> Maybe.map (\direction -> Point3d.translateIn direction radius Point3d.origin)
                |> Maybe.withDefault point
    in
    faces
        |> List.concatMap
            (\face ->
                case BspTree.allPoints face of
                    a :: b :: c :: rest ->
                        let
                            newPoints =
                                [ [ projectToSphere a, projectToSphere b, projectToSphere c ] ]
                        in
                        newPoints
                            |> List.filterMap toFace

                    _ ->
                        []
            )



-- Generate a geodesic sphere.


geodesicSphere : Length -> Int -> Shape3d tag c
geodesicSphere radius subdivisions =
    if subdivisions < 1 then
        scale radius icosahedron
            |> BspTree.build
            |> Shape3d

    else
        List.range 0 subdivisions
            |> List.foldl (\_ -> subdivide radius) icosahedron
            |> BspTree.build
            |> Shape3d



-- Operations


intersectWith : Shape3d tag c -> Shape3d tag c -> Shape3d tag c
intersectWith (Shape3d t1) (Shape3d t2) =
    let
        a =
            t1
                |> BspTree.toFaces
                |> List.map (BspTree.findInside BspTree.Same t2)
                |> List.concat

        b =
            t2
                |> BspTree.toFaces
                |> List.map (BspTree.findInside BspTree.None t1)
                |> List.concat
    in
    (a ++ b)
        |> BspTree.build
        |> Shape3d


unionWith : Shape3d tag c -> Shape3d tag c -> Shape3d tag c
unionWith (Shape3d t1) (Shape3d t2) =
    let
        a =
            t1
                |> BspTree.toFaces
                |> List.map (BspTree.findOutside BspTree.Same t2)
                |> List.concat

        b =
            t2
                |> BspTree.toFaces
                |> List.map (BspTree.findOutside BspTree.None t1)
                |> List.concat
    in
    (a ++ b)
        |> BspTree.build
        |> Shape3d


subtractFrom : Shape3d tag c -> Shape3d tag c -> Shape3d tag c
subtractFrom (Shape3d t1) (Shape3d t2) =
    let
        a =
            t1
                |> BspTree.toFaces
                |> List.map (BspTree.findOutside BspTree.Opposite t2)
                |> List.concat

        b =
            t2
                |> BspTree.toFaces
                |> List.map (BspTree.findInside BspTree.None t1)
                |> List.concat
                |> BspTree.invertFaces
    in
    (a ++ b)
        |> BspTree.build
        |> Shape3d


{-| TODO: bottleneck for combining lots of complicated models
-- maybe it would be treated in a simple way when the shapes do not overlap..
-}
group : List (Shape3d tag c) -> Shape3d tag c
group csgs =
    csgs
        |> List.foldl (\(Shape3d tree) acc -> BspTree.toFaces tree ++ acc) []
        |> BspTree.build
        |> Shape3d


translateBy : Vector3d Meters c -> Shape3d tag c -> Shape3d tag c
translateBy vector shape =
    toTree shape
        |> BspTree.translate vector
        |> Shape3d


moveRight : Length -> Shape3d tag c -> Shape3d tag c
moveRight distance shape =
    toTree shape
        |> BspTree.translate (Vector3d.xyz distance Quantity.zero Quantity.zero)
        |> Shape3d


moveLeft : Length -> Shape3d tag c -> Shape3d tag c
moveLeft distance shape =
    toTree shape
        |> BspTree.translate (Vector3d.xyz (Quantity.negate distance) Quantity.zero Quantity.zero)
        |> Shape3d


moveBackward : Length -> Shape3d tag c -> Shape3d tag c
moveBackward distance shape =
    toTree shape
        |> BspTree.translate (Vector3d.xyz Quantity.zero distance Quantity.zero)
        |> Shape3d


moveForward : Length -> Shape3d tag c -> Shape3d tag c
moveForward distance shape =
    toTree shape
        |> BspTree.translate (Vector3d.xyz Quantity.zero (Quantity.negate distance) Quantity.zero)
        |> Shape3d


moveUp : Length -> Shape3d tag c -> Shape3d tag c
moveUp distance shape =
    toTree shape
        |> BspTree.translate (Vector3d.xyz Quantity.zero Quantity.zero distance)
        |> Shape3d


moveDown : Length -> Shape3d tag c -> Shape3d tag c
moveDown distance shape =
    toTree shape
        |> BspTree.translate (Vector3d.xyz Quantity.zero Quantity.zero (Quantity.negate distance))
        |> Shape3d


rotateAround : Axis3d Meters c -> Angle -> Shape3d tag c -> Shape3d tag c
rotateAround axis angle shape =
    toTree shape
        |> BspTree.rotateAround axis angle
        |> Shape3d


scaleAbout : Point3d Meters c -> Float -> Shape3d tag c -> Shape3d tag c
scaleAbout origin factor shape =
    toTree shape
        |> BspTree.scaleAbout origin factor
        |> Shape3d



-- TODO: needs to be unitless


scaleBy : Vector3d Unitless c -> Shape3d tag c -> Shape3d tag c
scaleBy vector shape =
    toTree shape
        |> BspTree.scaleBy vector
        |> Shape3d


withTag : tag -> Shape3d tag c -> Shape3d tag c
withTag tag shape =
    toTree shape
        |> BspTree.mapFaces (\f -> { f | tag = Just tag })
        |> Shape3d
