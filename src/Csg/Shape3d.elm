module Csg.Shape3d exposing
    ( Shape3d
    , cone
    , coneWith
    , cube
    , cuboid
    , cylinder
    , cylinderFromTo
    , group
    , intersectWith
    , moveBackward
    , moveDown
    , moveForward
    , moveLeft
    , moveRight
    , moveUp
    , rotateAround
    , scaleAbout
    , scaleBy
    , sphere
    , sphereWith
    , subtractFrom
    , toFaces
    , torus
    , translateBy
    , unionWith
    , withColor
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BspTree exposing (BspTree, Face)
import Color exposing (Color)
import Direction3d
import Length exposing (Length, Meters)
import List.NonEmpty as NonEmpty
import Point3d exposing (Point3d)
import Quantity
import Triangle3d
import Vector3d exposing (Vector3d)


defaultColor : Color
defaultColor =
    Color.yellow



-- Solids construction


type Shape3d c
    = Shape3d (BspTree c)


toFaces : Shape3d c -> List (Face c)
toFaces (Shape3d tree) =
    BspTree.toFaces tree


toTree : Shape3d c -> BspTree c
toTree (Shape3d tree) =
    tree


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


cuboid : { width : Length, height : Length, depth : Length } -> Shape3d coordinates
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


cube : Length -> Shape3d coordinates
cube size =
    cuboid { width = size, height = size, depth = size }



-- Sphere


type alias SphereSettings =
    { slices : Int
    , stacks : Int
    , radius : Length
    }


sphereDefaultSettings : SphereSettings
sphereDefaultSettings =
    { slices = 32
    , stacks = 16
    , radius = Length.meters 0.5
    }


sphere : Length -> Shape3d c
sphere radius =
    sphereWith { sphereDefaultSettings | radius = radius }


sphereWith : SphereSettings -> Shape3d c
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
    -> Shape3d c
cone radius height =
    coneWith
        { coneDefaultSettings
            | bottomRadius = radius
            , topPoint = Point3d.xyz (Length.meters 0) (Length.meters 0) height
        }


coneWith :
    ConeSettings c
    -> Shape3d c
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
    in
    (bottom ++ sides ++ top)
        |> BspTree.build
        |> Shape3d



-- Cylinder


cylinder : Length -> Length -> Shape3d c
cylinder radius height =
    coneWith
        { coneDefaultSettings
            | bottomRadius = radius
            , topRadius = radius
            , topPoint = Point3d.xyz Quantity.zero Quantity.zero height
        }


cylinderFromTo : Length -> Point3d Meters c -> Point3d Meters c -> Shape3d c
cylinderFromTo radius start end =
    coneWith
        { coneDefaultSettings
            | bottomRadius = radius
            , topRadius = radius
            , bottomPoint = start
            , topPoint = end
        }



-- Torus


torus : Length -> Length -> Shape3d c
torus innerRadius outerRadius =
    let
        stacks_ =
            8

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
    in
    allCircles
        |> List.concat
        |> BspTree.build
        |> Shape3d



-- Operations


intersectWith : Shape3d c -> Shape3d c -> Shape3d c
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


unionWith : Shape3d c -> Shape3d c -> Shape3d c
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


subtractFrom : Shape3d c -> Shape3d c -> Shape3d c
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


group : List (Shape3d c) -> Shape3d c
group csgs =
    csgs
        |> List.foldl (\(Shape3d tree) acc -> BspTree.toFaces tree ++ acc) []
        |> BspTree.build
        |> Shape3d


translateBy : Vector3d Meters c -> Shape3d c -> Shape3d c
translateBy vector shape =
    toTree shape
        |> BspTree.translate vector
        |> Shape3d


moveRight : Length -> Shape3d c -> Shape3d c
moveRight distance shape =
    toTree shape
        |> BspTree.translate (Vector3d.xyz distance Quantity.zero Quantity.zero)
        |> Shape3d


moveLeft : Length -> Shape3d c -> Shape3d c
moveLeft distance shape =
    toTree shape
        |> BspTree.translate (Vector3d.xyz (Quantity.negate distance) Quantity.zero Quantity.zero)
        |> Shape3d


moveBackward : Length -> Shape3d c -> Shape3d c
moveBackward distance shape =
    toTree shape
        |> BspTree.translate (Vector3d.xyz Quantity.zero distance Quantity.zero)
        |> Shape3d


moveForward : Length -> Shape3d c -> Shape3d c
moveForward distance shape =
    toTree shape
        |> BspTree.translate (Vector3d.xyz Quantity.zero (Quantity.negate distance) Quantity.zero)
        |> Shape3d


moveUp : Length -> Shape3d c -> Shape3d c
moveUp distance shape =
    toTree shape
        |> BspTree.translate (Vector3d.xyz Quantity.zero Quantity.zero distance)
        |> Shape3d


moveDown : Length -> Shape3d c -> Shape3d c
moveDown distance shape =
    toTree shape
        |> BspTree.translate (Vector3d.xyz Quantity.zero Quantity.zero (Quantity.negate distance))
        |> Shape3d


rotateAround : Axis3d Meters c -> Angle -> Shape3d c -> Shape3d c
rotateAround axis angle shape =
    toTree shape
        |> BspTree.rotateAround axis angle
        |> Shape3d


scaleAbout : Point3d Meters c -> Float -> Shape3d c -> Shape3d c
scaleAbout origin factor shape =
    toTree shape
        |> BspTree.scaleAbout origin factor
        |> Shape3d


scaleBy : Vector3d Meters c -> Shape3d c -> Shape3d c
scaleBy vector shape =
    toTree shape
        |> BspTree.scaleBy vector
        |> Shape3d


withColor : Color -> Shape3d c -> Shape3d c
withColor color shape =
    toTree shape
        |> BspTree.mapFaces (\f -> { f | color = color })
        |> Shape3d
