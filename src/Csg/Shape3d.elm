module Csg.Shape3d exposing
    ( Shape3d
    , cone
    , cube
    , cuboid
    , cylinder
    , group
    , intersectWith
    , rotateAround
    , scaleAbout
    , scaleBy
    , sphere
    , sphereWith
    , subtractFrom
    , toTree
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
import Quantity exposing (Unitless)
import Triangle3d
import Vector3d exposing (Vector3d)


defaultColor : Color
defaultColor =
    Color.gray



-- Solids construction


type Shape3d c
    = Shape3d (BspTree c)


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


yzPlane : Shape3d coordinates
yzPlane =
    let
        size =
            Length.meters 2

        negSize =
            Length.meters -2

        z =
            Length.meters 0

        a =
            Point3d.xyz z negSize negSize

        b =
            Point3d.xyz z size negSize

        c =
            Point3d.xyz z size size

        d =
            Point3d.xyz z negSize size

        rightNormal =
            Direction3d.x
    in
    [ Face ( a, [ b, c, d ] ) rightNormal defaultColor ]
        |> BspTree.build
        |> Shape3d


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
    }


sphereDefaults : SphereSettings
sphereDefaults =
    { slices = 32
    , stacks = 16
    }


sphere : Length -> Shape3d c
sphere =
    sphereWith sphereDefaults


sphereWith : SphereSettings -> Length -> Shape3d c
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
        |> Shape3d


cylinder : Length -> Point3d Meters c -> Point3d Meters c -> Shape3d c
cylinder radius start end =
    coneWith
        { slices = 32 }
        { bottomRadius = radius
        , topRadius = radius
        , bottomPoint = start
        , topPoint = end
        }



-- Cone


cone :
    Length
    -> Length
    -> Shape3d c
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
    -> Shape3d c
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
translateBy vector (Shape3d tree) =
    tree
        |> BspTree.translate vector
        |> Shape3d


rotateAround : Axis3d Meters c -> Angle -> Shape3d c -> Shape3d c
rotateAround axis angle (Shape3d tree) =
    tree
        |> BspTree.rotateAround axis angle
        |> Shape3d


scaleAbout : Point3d Meters c -> Float -> Shape3d c -> Shape3d c
scaleAbout origin factor (Shape3d tree) =
    tree
        |> BspTree.scaleAbout origin factor
        |> Shape3d


scaleBy : Vector3d Meters c -> Shape3d c -> Shape3d c
scaleBy vector (Shape3d tree) =
    tree
        |> BspTree.scaleBy vector
        |> Shape3d


withColor : Color -> Shape3d c -> Shape3d c
withColor color (Shape3d tree) =
    tree
        |> BspTree.mapFaces (\f -> { f | color = color })
        |> Shape3d
