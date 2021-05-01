module BspTree exposing (..)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Bitwise
import Color exposing (Color)
import Dict
import Direction3d exposing (Direction3d)
import Length exposing (Meters)
import List
import List.NonEmpty as NonEmpty exposing (NonEmpty)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity
import Vector3d exposing (Vector3d)


epsilon : Float
epsilon =
    1.0e-5


type alias Face c =
    { points : NonEmpty (Point3d Meters c)
    , normalDirection : Direction3d c
    , color : Color
    }


type alias NodeData c =
    { faces : NonEmpty (Face c)
    , outside : BspTree c
    , inside : BspTree c
    , plane : Plane3d Meters c
    }


type Orientation
    = Same
    | Opposite
    | None


type alias IntersectionResult c =
    { outside : Maybe (Face c)
    , inside : Maybe (Face c)
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


type BspTree c
    = Node (NodeData c)
    | Empty


allPoints : Face c -> List (Point3d Meters c)
allPoints { points } =
    NonEmpty.toList points


toFacePoints : List (Point3d Meters c) -> Maybe ( Point3d Meters c, List (Point3d Meters c) )
toFacePoints points =
    NonEmpty.fromList points


planeFromFace : Face c -> Plane3d Meters c
planeFromFace { points, normalDirection } =
    Plane3d.through (Tuple.first points) normalDirection


empty : BspTree c
empty =
    Empty


build : List (Face c) -> BspTree c
build faces =
    List.foldl insert empty faces


toFaces : BspTree c -> List (Face c)
toFaces tree =
    let
        traverse t =
            case t of
                Empty ->
                    []

                Node { faces, inside, outside } ->
                    NonEmpty.toList faces ++ (traverse inside ++ traverse outside)
    in
    traverse tree


insert : Face c -> BspTree c -> BspTree c
insert face tree =
    let
        facePlane =
            planeFromFace face

        newNode : Plane3d Meters c -> Face c -> NodeData c
        newNode plane newFace =
            { faces = ( newFace, [] ), outside = Empty, inside = Empty, plane = plane }

        handleOutside : Plane3d Meters c -> Maybe (Face c) -> NodeData c -> NodeData c
        handleOutside plane maybeFace node =
            maybeFace
                |> Maybe.map
                    (\newFace ->
                        if node.outside == Empty then
                            { node | outside = Node <| newNode plane newFace }

                        else
                            { node | outside = insert newFace node.outside }
                    )
                |> Maybe.withDefault node

        handleInside : Plane3d Meters c -> Maybe (Face c) -> NodeData c -> NodeData c
        handleInside plane maybeFace node =
            maybeFace
                |> Maybe.map
                    (\newFace ->
                        if node.inside == Empty then
                            { node | inside = Node <| newNode plane newFace }

                        else
                            { node | inside = insert newFace node.inside }
                    )
                |> Maybe.withDefault node
    in
    case tree of
        Empty ->
            Node <| newNode facePlane face

        Node rootData ->
            splitByPlane rootData.plane face
                |> (\{ outside, inside } ->
                        rootData
                            |> handleOutside facePlane outside
                            |> handleInside facePlane inside
                            |> Node
                   )


fromPoints : Direction3d coordinates -> Color -> List (Point3d Meters coordinates) -> Maybe (Face coordinates)
fromPoints normalDirection color triangles =
    case triangles of
        [] ->
            Nothing

        first :: rest ->
            Just { color = color, points = ( first, rest ), normalDirection = normalDirection }


mapFaces : (Face c -> Face c) -> BspTree c -> BspTree c
mapFaces fn tree =
    let
        traverse t =
            case t of
                Empty ->
                    Empty

                Node nodeData ->
                    Node
                        { nodeData
                            | faces = NonEmpty.map fn nodeData.faces
                            , inside = traverse nodeData.inside
                            , outside = traverse nodeData.outside
                        }
    in
    traverse tree


translate : Vector3d Meters c -> BspTree c -> BspTree c
translate vector tree =
    let
        translateFace f =
            allPoints f
                |> List.map (Point3d.translateBy vector)
                |> fromPoints f.normalDirection f.color
                |> Maybe.withDefault f
    in
    case tree of
        Empty ->
            Empty

        Node nodeData ->
            Node
                { nodeData
                    | faces = NonEmpty.map translateFace nodeData.faces
                    , inside = translate vector nodeData.inside
                    , outside = translate vector nodeData.outside
                    , plane = Plane3d.translateBy vector nodeData.plane
                }


rotateAround : Axis3d Meters c -> Angle -> BspTree c -> BspTree c
rotateAround axis angle tree =
    let
        rotateFace f =
            allPoints f
                |> List.map (Point3d.rotateAround axis angle)
                |> fromPoints (Direction3d.rotateAround axis angle f.normalDirection) f.color
                |> Maybe.withDefault f
    in
    case tree of
        Empty ->
            Empty

        Node nodeData ->
            Node
                { nodeData
                    | faces = NonEmpty.map rotateFace nodeData.faces
                    , inside = rotateAround axis angle nodeData.inside
                    , outside = rotateAround axis angle nodeData.outside
                    , plane = Plane3d.rotateAround axis angle nodeData.plane
                }


scaleAbout : Point3d Meters c -> Float -> BspTree c -> BspTree c
scaleAbout origin factor tree =
    let
        scaleFace f =
            allPoints f
                |> List.map (Point3d.scaleAbout origin factor)
                |> fromPoints f.normalDirection f.color
                |> Maybe.withDefault f

        scalePlane p =
            Plane3d.originPoint p
                |> Point3d.scaleAbout origin factor
                |> (\newOrigin -> Plane3d.through newOrigin (Plane3d.normalDirection p))
    in
    case tree of
        Empty ->
            Empty

        Node nodeData ->
            Node
                { nodeData
                    | faces = NonEmpty.map scaleFace nodeData.faces
                    , inside = scaleAbout origin factor nodeData.inside
                    , outside = scaleAbout origin factor nodeData.outside
                    , plane = scalePlane nodeData.plane
                }


scaleBy : Vector3d Meters c -> BspTree c -> BspTree c
scaleBy vector tree =
    let
        ( xScale, yScale, zScale ) =
            Vector3d.components vector

        scalePoint p =
            Point3d.toMeters p
                |> (\{ x, y, z } ->
                        Point3d.xyz
                            (Quantity.multiplyBy x xScale)
                            (Quantity.multiplyBy y yScale)
                            (Quantity.multiplyBy z zScale)
                   )

        scaleDirection d =
            let
                ( dx, dy, dz ) =
                    Direction3d.components d

                handleScale dFloat s =
                    if dFloat < 0 then
                        -(-dFloat / Length.inMeters s)

                    else
                        dFloat / Length.inMeters s
            in
            Point3d.xyz
                (Length.meters (handleScale dx xScale))
                (Length.meters (handleScale dy yScale))
                (Length.meters (handleScale dz zScale))
                |> Direction3d.from Point3d.origin
                |> Maybe.withDefault d

        scaleFace f =
            allPoints f
                |> List.map scalePoint
                |> fromPoints (scaleDirection f.normalDirection) f.color
                |> Maybe.withDefault f

        scalePlane p =
            Plane3d.originPoint p
                |> scalePoint
                |> (\newOrigin -> Plane3d.through newOrigin (scaleDirection (Plane3d.normalDirection p)))
    in
    case tree of
        Empty ->
            Empty

        Node nodeData ->
            Node
                { nodeData
                    | faces = NonEmpty.map scaleFace nodeData.faces
                    , inside = scaleBy vector nodeData.inside
                    , outside = scaleBy vector nodeData.outside
                    , plane = scalePlane nodeData.plane
                }


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


findInside : Orientation -> BspTree c -> Face c -> List (Face c)
findInside orientation tree face =
    case tree of
        Empty ->
            []

        Node nodeData ->
            let
                classification =
                    classifyFace nodeData.plane face

                handleInside : Maybe (Face c) -> BspTree c -> List (Face c)
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

                handleOutside : Maybe (Face c) -> BspTree c -> List (Face c)
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


findOutside : Orientation -> BspTree c -> Face c -> List (Face c)
findOutside orientation tree face =
    case tree of
        Empty ->
            []

        Node nodeData ->
            let
                classification =
                    classifyFace nodeData.plane face

                handleInside : Maybe (Face c) -> BspTree c -> List (Face c)
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

                handleOutside : Maybe (Face c) -> BspTree c -> List (Face c)
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


classifyFace : Plane3d Meters c -> Face c -> ClassifiedFace c
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


splitByPlane : Plane3d Meters c -> Face c -> IntersectionResult c
splitByPlane plane face =
    let
        classifiedFace =
            classifyFace plane face

        isOutside : Face c -> Bool
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
