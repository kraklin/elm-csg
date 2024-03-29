module BspTree exposing
    ( BspTree
    , Face
    , Orientation(..)
    , allPoints
    , build
    , empty
    , findInside
    , findOutside
    , invertFaces
    , mapFaces
    , rotateAround
    , scaleAbout
    , scaleBy
    , toFaces
    , translate
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Bitwise
import Dict
import Direction3d exposing (Direction3d)
import Length exposing (Meters)
import List
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import Recursion
import Vector3d exposing (Vector3d)


epsilon : Float
epsilon =
    1.0e-5


type alias Face tag c =
    { points : NonEmpty (Point3d Meters c)
    , normalDirection : Direction3d c
    , tag : Maybe tag
    }


type alias NodeData tag c =
    { faces : NonEmpty (Face tag c)
    , outside : BspTree tag c
    , inside : BspTree tag c
    , plane : Plane3d Meters c
    }


type Orientation
    = Same
    | Opposite
    | None


type alias IntersectionResult tag c =
    { outside : Maybe (Face tag c)
    , inside : Maybe (Face tag c)
    }


type Classification
    = Coplanar
    | Outside
    | Inside
    | Spanning


type alias ClassifiedVertex c =
    { point : Point3d Meters c
    , class : Classification
    }


type alias ClassifiedFace c =
    { points : List (ClassifiedVertex c)
    , class : Classification
    , normalDirection : Direction3d c
    }


type BspTree tag c
    = Node (NodeData tag c)
    | Empty


allPoints : Face tag c -> List (Point3d Meters c)
allPoints { points } =
    NonEmpty.toList points


toFacePoints : List (Point3d Meters c) -> Maybe ( Point3d Meters c, List (Point3d Meters c) )
toFacePoints points =
    NonEmpty.fromList points


planeFromFace : Face tag c -> Plane3d Meters c
planeFromFace { points, normalDirection } =
    Plane3d.through (Tuple.first points) normalDirection


empty : BspTree tag c
empty =
    Empty


build : List (Face tag c) -> BspTree tag c
build faces =
    List.foldl insert empty faces


toFaces : BspTree tag c -> List (Face tag c)
toFaces initTree =
    Recursion.runRecursion
        (\tree ->
            case tree of
                Empty ->
                    Recursion.base []

                Node { faces, inside, outside } ->
                    Recursion.recurseThen inside
                        (\newInside ->
                            Recursion.recurseThen outside
                                (\newOutside ->
                                    Recursion.base <|
                                        NonEmpty.toList faces
                                            ++ (newInside ++ newOutside)
                                )
                        )
        )
        initTree


insert : Face tag c -> BspTree tag c -> BspTree tag c
insert initFace initTree =
    Recursion.runRecursion
        (\( face, tree ) ->
            case tree of
                Empty ->
                    Recursion.base
                        (Node <|
                            { faces = ( face, [] ), outside = Empty, inside = Empty, plane = planeFromFace face }
                        )

                Node rootData ->
                    splitByPlane rootData.plane face
                        |> (\{ outside, inside } ->
                                case ( outside, inside ) of
                                    ( Just out, Nothing ) ->
                                        Recursion.recurse ( out, rootData.outside ) |> Recursion.map (\outs -> Node { rootData | outside = outs })

                                    ( Nothing, Just ins ) ->
                                        Recursion.recurse ( ins, rootData.inside ) |> Recursion.map (\newIns -> Node { rootData | inside = newIns })

                                    ( Just out, Just ins ) ->
                                        Recursion.recurseThen ( out, rootData.outside )
                                            (\newOut ->
                                                Recursion.recurseThen ( ins, rootData.inside )
                                                    (\newIns ->
                                                        Recursion.base <|
                                                            Node
                                                                { rootData
                                                                    | outside = newOut
                                                                    , inside = newIns
                                                                }
                                                    )
                                            )

                                    ( Nothing, Nothing ) ->
                                        Recursion.base tree
                           )
        )
        ( initFace, initTree )


fromPoints : Direction3d coordinates -> Maybe tag -> NonEmpty (Point3d Meters coordinates) -> Face tag coordinates
fromPoints normalDirection tag points =
    { tag = tag, points = points, normalDirection = normalDirection }


mapFaces : (Face tag c -> Face tag c) -> BspTree tag c -> BspTree tag c
mapFaces fn initTree =
    Recursion.runRecursion
        (\tree ->
            case tree of
                Empty ->
                    Recursion.base Empty

                Node nodeData ->
                    Recursion.recurseThen nodeData.inside
                        (\newInside ->
                            Recursion.recurseThen nodeData.outside
                                (\newOutside ->
                                    Recursion.base <|
                                        Node
                                            { nodeData
                                                | faces = NonEmpty.map fn nodeData.faces
                                                , inside = newInside
                                                , outside = newOutside
                                            }
                                )
                        )
        )
        initTree


translate : Vector3d Meters c -> BspTree tag c -> BspTree tag c
translate vector initTree =
    let
        translateFace f =
            f.points
                |> NonEmpty.map (Point3d.translateBy vector)
                |> fromPoints f.normalDirection f.tag
    in
    Recursion.runRecursion
        (\tree ->
            case tree of
                Empty ->
                    Recursion.base Empty

                Node nodeData ->
                    Recursion.recurseThen nodeData.inside
                        (\newInside ->
                            Recursion.recurseThen nodeData.outside
                                (\newOutside ->
                                    Recursion.base <|
                                        Node
                                            { nodeData
                                                | faces = NonEmpty.map translateFace nodeData.faces
                                                , inside = newInside
                                                , outside = newOutside
                                                , plane = Plane3d.translateBy vector nodeData.plane
                                            }
                                )
                        )
        )
        initTree


rotateAround : Axis3d Meters c -> Angle -> BspTree tag c -> BspTree tag c
rotateAround axis angle initTree =
    let
        rotateFace f =
            f.points
                |> NonEmpty.map (Point3d.rotateAround axis angle)
                |> fromPoints (Direction3d.rotateAround axis angle f.normalDirection) f.tag
    in
    Recursion.runRecursion
        (\tree ->
            case tree of
                Empty ->
                    Recursion.base Empty

                Node nodeData ->
                    Recursion.recurseThen nodeData.inside
                        (\newInside ->
                            Recursion.recurseThen nodeData.outside
                                (\newOutside ->
                                    Recursion.base <|
                                        Node
                                            { nodeData
                                                | faces = NonEmpty.map rotateFace nodeData.faces
                                                , inside = newInside
                                                , outside = newOutside
                                                , plane = Plane3d.rotateAround axis angle nodeData.plane
                                            }
                                )
                        )
        )
        initTree


scaleAbout : Point3d Meters c -> Float -> BspTree tag c -> BspTree tag c
scaleAbout origin factor initTree =
    let
        scaleFace f =
            f.points
                |> NonEmpty.map (Point3d.scaleAbout origin factor)
                |> fromPoints f.normalDirection f.tag

        scalePlane p =
            Plane3d.originPoint p
                |> Point3d.scaleAbout origin factor
                |> (\newOrigin -> Plane3d.through newOrigin (Plane3d.normalDirection p))
    in
    Recursion.runRecursion
        (\tree ->
            case tree of
                Empty ->
                    Recursion.base Empty

                Node nodeData ->
                    Recursion.recurseThen nodeData.inside
                        (\newInside ->
                            Recursion.recurseThen nodeData.outside
                                (\newOutside ->
                                    Recursion.base <|
                                        Node
                                            { nodeData
                                                | faces = NonEmpty.map scaleFace nodeData.faces
                                                , inside = newInside
                                                , outside = newOutside
                                                , plane = scalePlane nodeData.plane
                                            }
                                )
                        )
        )
        initTree


scaleBy : Vector3d Unitless c -> BspTree tag c -> BspTree tag c
scaleBy vector initTree =
    let
        ( xScale, yScale, zScale ) =
            Vector3d.toUnitless vector
                |> (\{ x, y, z } -> ( x, y, z ))

        scalePoint : Point3d Meters c -> Point3d Meters c
        scalePoint p =
            Point3d.coordinates p
                |> (\( x, y, z ) ->
                        Point3d.xyz
                            (Quantity.multiplyBy xScale x)
                            (Quantity.multiplyBy yScale y)
                            (Quantity.multiplyBy zScale z)
                   )

        scaleDirection : Direction3d c -> Direction3d c
        scaleDirection d =
            let
                ( dx, dy, dz ) =
                    Direction3d.components d

                handleScale dFloat s =
                    if dFloat < 0 then
                        -(-dFloat / s)

                    else
                        dFloat / s
            in
            Point3d.xyz
                (Length.meters (handleScale dx xScale))
                (Length.meters (handleScale dy yScale))
                (Length.meters (handleScale dz zScale))
                |> Direction3d.from Point3d.origin
                |> Maybe.withDefault d

        scaleFace : Face tag c -> Face tag c
        scaleFace f =
            f.points
                |> NonEmpty.map scalePoint
                |> fromPoints (scaleDirection f.normalDirection) f.tag

        scalePlane : Plane3d Meters c -> Plane3d Meters c
        scalePlane p =
            Plane3d.originPoint p
                |> scalePoint
                |> (\newOrigin -> Plane3d.through newOrigin (scaleDirection (Plane3d.normalDirection p)))
    in
    Recursion.runRecursion
        (\tree ->
            case tree of
                Empty ->
                    Recursion.base Empty

                Node nodeData ->
                    Recursion.recurseThen nodeData.inside
                        (\newInside ->
                            Recursion.recurseThen nodeData.outside
                                (\newOutside ->
                                    Recursion.base <|
                                        Node
                                            { nodeData
                                                | faces = NonEmpty.map scaleFace nodeData.faces
                                                , inside = newInside
                                                , outside = newOutside
                                                , plane = scalePlane nodeData.plane
                                            }
                                )
                        )
        )
        initTree


getOrientation : Plane3d Meters c -> ClassifiedFace c -> Orientation
getOrientation plane face =
    let
        facePlaneDotProduct =
            Vector3d.dot (Direction3d.toVector face.normalDirection) (Direction3d.toVector (Plane3d.normalDirection plane))
                |> Quantity.unwrap

        areSame =
            facePlaneDotProduct > -epsilon || facePlaneDotProduct < epsilon
    in
    if face.class == Coplanar && areSame then
        Same

    else
        Opposite


findInside : Orientation -> BspTree tag c -> Face tag c -> List (Face tag c)
findInside orientation tree face =
    case tree of
        Empty ->
            []

        Node nodeData ->
            let
                classification =
                    classifyFace nodeData.plane face

                handleInside : Maybe (Face tag c) -> BspTree tag c -> List (Face tag c)
                handleInside maybeFace tree_ =
                    maybeFace
                        |> Maybe.map
                            (\face_ ->
                                case tree_ of
                                    Empty ->
                                        [ face_ ]

                                    Node _ ->
                                        findInside orientation tree_ face_
                            )
                        |> Maybe.withDefault []

                handleOutside : Maybe (Face tag c) -> BspTree tag c -> List (Face tag c)
                handleOutside maybeFace tree_ =
                    maybeFace
                        |> Maybe.map
                            (\face_ ->
                                case tree_ of
                                    Empty ->
                                        []

                                    Node _ ->
                                        findInside orientation tree_ face_
                            )
                        |> Maybe.withDefault []
            in
            (if classification.class == Coplanar then
                if getOrientation nodeData.plane classification == orientation then
                    { inside = Just face, outside = Nothing }

                else
                    { inside = Nothing, outside = Just face }

             else
                splitByPlane nodeData.plane face
            )
                |> (\{ inside, outside } ->
                        handleInside inside nodeData.inside ++ handleOutside outside nodeData.outside
                   )


findOutside : Orientation -> BspTree tag c -> Face tag c -> List (Face tag c)
findOutside orientation tree face =
    case tree of
        Empty ->
            []

        Node nodeData ->
            let
                classification =
                    classifyFace nodeData.plane face

                handleInside : Maybe (Face tag c) -> BspTree tag c -> List (Face tag c)
                handleInside maybeFace tree_ =
                    maybeFace
                        |> Maybe.map
                            (\face_ ->
                                case tree_ of
                                    Empty ->
                                        []

                                    Node _ ->
                                        findOutside orientation tree_ face_
                            )
                        |> Maybe.withDefault []

                handleOutside : Maybe (Face tag c) -> BspTree tag c -> List (Face tag c)
                handleOutside maybeFace tree_ =
                    maybeFace
                        |> Maybe.map
                            (\face_ ->
                                case tree_ of
                                    Empty ->
                                        [ face_ ]

                                    Node _ ->
                                        findOutside orientation tree_ face_
                            )
                        |> Maybe.withDefault []
            in
            (if classification.class == Coplanar then
                if getOrientation nodeData.plane classification == orientation then
                    { inside = Nothing, outside = Just face }

                else
                    { inside = Just face, outside = Nothing }

             else
                splitByPlane nodeData.plane face
            )
                |> (\{ inside, outside } ->
                        handleInside inside nodeData.inside ++ handleOutside outside nodeData.outside
                   )


areFacesSame : Face tag c -> Face tag c -> Bool
areFacesSame faceA faceB =
    let
        pointsA =
            allPoints faceA

        pointsB =
            allPoints faceB
    in
    pointsA
        |> List.all (\a -> List.member a pointsB)


invertFaces : List (Face tag c) -> List (Face tag c)
invertFaces =
    let
        invertFace : Face tag c -> Face tag c
        invertFace f =
            { f
                | points = NonEmpty.reverse f.points
                , normalDirection = Direction3d.reverse f.normalDirection
            }
    in
    List.map invertFace


dedup : List (Point3d Meters c) -> List (Point3d Meters c)
dedup list =
    list
        |> List.map (\point -> ( Point3d.toTuple Length.inMeters point, point ))
        |> Dict.fromList
        |> Dict.values


classToNumber : Classification -> Int
classToNumber class =
    case class of
        Coplanar ->
            0

        Outside ->
            1

        Inside ->
            2

        Spanning ->
            3


numberToClass : Int -> Classification
numberToClass class =
    case class of
        0 ->
            Coplanar

        1 ->
            Outside

        2 ->
            Inside

        _ ->
            Spanning


combineClasses : Classification -> Classification -> Classification
combineClasses c1 c2 =
    Bitwise.or (classToNumber c1) (classToNumber c2)
        |> numberToClass


classifyFace : Plane3d Meters c -> Face tag c -> ClassifiedFace c
classifyFace plane face =
    let
        planeNormal =
            Plane3d.normalDirection plane |> Direction3d.toVector

        w =
            Vector3d.dot planeNormal (Vector3d.from Point3d.origin (Plane3d.originPoint plane))

        t v =
            Quantity.minus w (Vector3d.dot planeNormal v)
                |> Quantity.unwrap
                |> (\result ->
                        if result < -epsilon then
                            Inside

                        else if result > epsilon then
                            Outside

                        else
                            Coplanar
                   )

        classifiedPoints =
            allPoints face
                |> List.map (\point -> { class = Vector3d.from Point3d.origin point |> t, point = point })

        combinedClasses =
            classifiedPoints
                |> List.foldl (.class >> combineClasses) Coplanar
    in
    { class = combinedClasses
    , points = classifiedPoints
    , normalDirection = face.normalDirection
    }


splitByPlane : Plane3d Meters c -> Face tag c -> IntersectionResult tag c
splitByPlane plane face =
    let
        classifiedFace =
            classifyFace plane face

        isOutside : Face tag c -> Bool
        isOutside face_ =
            face_
                |> .normalDirection
                |> Direction3d.toVector
                |> Vector3d.dot
                    (Plane3d.normalDirection plane
                        |> Direction3d.toVector
                    )
                |> Quantity.greaterThan Quantity.zero
    in
    case classifiedFace.class of
        Coplanar ->
            if isOutside face then
                { outside = Just face, inside = Nothing }

            else
                { outside = Nothing, inside = Just face }

        Outside ->
            { outside = Just face, inside = Nothing }

        Inside ->
            { outside = Nothing, inside = Just face }

        Spanning ->
            let
                planeNormal =
                    Plane3d.normalDirection plane |> Direction3d.toVector

                maybeSplitPoint ( t1, t2 ) =
                    if combineClasses t1.class t2.class == Spanning then
                        let
                            va =
                                Vector3d.from Point3d.origin t1.point

                            vb =
                                Vector3d.from Point3d.origin t2.point

                            vba =
                                Vector3d.minus vb va

                            w =
                                Vector3d.dot planeNormal (Vector3d.from Point3d.origin (Plane3d.originPoint plane))

                            t =
                                Quantity.ratio (Quantity.minus w (Vector3d.dot planeNormal va)) (Vector3d.dot planeNormal vba)
                        in
                        Just <| Point3d.translateBy (Vector3d.interpolateFrom va vb t) Point3d.origin

                    else
                        Nothing

                maybeOutsidePoint ( t1, _ ) =
                    if t1.class == Outside || t1.class == Coplanar then
                        Just t1.point

                    else
                        Nothing

                maybeInsidePoint ( t1, _ ) =
                    if t1.class == Inside || t1.class == Coplanar then
                        Just t1.point

                    else
                        Nothing

                pairs =
                    case classifiedFace.points of
                        first :: _ :: _ ->
                            classifiedFace.points
                                |> List.reverse
                                |> List.foldl (\point { prev, pairs_ } -> { prev = point, pairs_ = ( point, prev ) :: pairs_ })
                                    { prev = first, pairs_ = [] }
                                |> .pairs_

                        _ ->
                            []

                toFace points =
                    toFacePoints points
                        |> Maybe.map
                            (\points_ ->
                                { face | points = points_ }
                            )
            in
            pairs
                |> List.map
                    (\tuple ->
                        { outside = [ maybeOutsidePoint tuple, maybeSplitPoint tuple ] |> List.filterMap identity
                        , inside = [ maybeInsidePoint tuple, maybeSplitPoint tuple ] |> List.filterMap identity
                        }
                    )
                |> List.foldl
                    (\{ outside, inside } acc ->
                        { acc
                            | outside = acc.outside ++ outside
                            , inside = acc.inside ++ inside
                        }
                    )
                    { outside = [], inside = [] }
                |> (\{ outside, inside } -> { outside = toFace outside, inside = toFace inside })
