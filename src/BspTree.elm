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


type alias Face c =
    { points : NonEmpty (Point3d Meters c)
    , normalDirection : Direction3d c
    , color : Color
    }


allPoints : Face c -> List (Point3d Meters c)
allPoints { points } =
    NonEmpty.toList points


toFacePoints : List (Point3d Meters c) -> Maybe ( Point3d Meters c, List (Point3d Meters c) )
toFacePoints points =
    NonEmpty.fromList points


planeFromFace : Face c -> Plane3d Meters c
planeFromFace { points, normalDirection } =
    Plane3d.through (Tuple.first points) normalDirection


type alias NodeData c =
    { faces : NonEmpty (Face c)
    , outside : BspTree c
    , inside : BspTree c
    , plane : Plane3d Meters c
    }


type BspTree c
    = Node (NodeData c)
    | Empty


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
            divide rootData.plane face
                |> (\{ inside, outside } ->
                        rootData
                            |> handleOutside facePlane outside
                            |> handleInside facePlane inside
                            |> Node
                   )


divide : Plane3d Meters c -> Face c -> { inside : Maybe (Face c), outside : Maybe (Face c) }
divide splittingPlane face =
    splitByPlane splittingPlane face
        |> (\{ front, back } ->
                { inside = back
                , outside = front
                }
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


clip : BspTree c -> BspTree c -> BspTree c
clip t1 t2 =
    t2
        |> toFaces
        |> List.map (\f -> clipFace f t1)
        |> List.concat
        |> build


invert : BspTree c -> BspTree c
invert tree =
    let
        invertFace : Face c -> Face c
        invertFace f =
            { f
                | points = NonEmpty.reverse f.points
                , normalDirection = Direction3d.reverse f.normalDirection
            }
    in
    case tree of
        Empty ->
            Empty

        Node nodeData ->
            Node
                { nodeData
                    | faces = NonEmpty.map invertFace nodeData.faces
                    , inside = invert nodeData.inside
                    , outside = invert nodeData.outside
                    , plane = Plane3d.flip nodeData.plane
                }


clipFace : Face c -> BspTree c -> List (Face c)
clipFace clippedFace clippingTree =
    let
        arePlanesSame treePlane face =
            treePlane == planeFromFace face

        handleInside : Maybe (Face c) -> BspTree c -> List (Face c)
        handleInside maybeFace tree =
            maybeFace
                |> Maybe.map
                    (\face ->
                        case tree of
                            Empty ->
                                [ face ]

                            Node _ ->
                                clipFace face tree
                    )
                |> Maybe.withDefault []

        handleOutside : Maybe (Face c) -> BspTree c -> List (Face c)
        handleOutside maybeFace tree =
            maybeFace
                |> Maybe.map
                    (\face ->
                        case tree of
                            Empty ->
                                []

                            Node _ ->
                                clipFace face tree
                    )
                |> Maybe.withDefault []
    in
    case clippingTree of
        Empty ->
            []

        Node rootData ->
            if arePlanesSame rootData.plane clippedFace then
                []

            else
                divide rootData.plane clippedFace
                    |> (\{ inside, outside } ->
                            handleInside inside rootData.inside ++ handleOutside outside rootData.outside
                       )


dedup : List (Point3d Meters c) -> List (Point3d Meters c)
dedup list =
    list
        |> List.map (\point -> ( Point3d.toTuple Length.inMeters point, point ))
        |> Dict.fromList
        |> Dict.values


type alias IntersectionResult c =
    { front : Maybe (Face c)
    , back : Maybe (Face c)
    }


type Classification
    = Coplanar
    | Front
    | Back
    | Spanning


type alias ClassifiedVertex c =
    { point : Point3d Meters c
    , class : Classification
    }


type alias ClassifiedFace c =
    { points : List (ClassifiedVertex c)
    , class : Classification
    }


classToNumber : Classification -> Int
classToNumber class =
    case class of
        Coplanar ->
            0

        Front ->
            1

        Back ->
            2

        Spanning ->
            3


numberToClass : Int -> Classification
numberToClass class =
    case class of
        0 ->
            Coplanar

        1 ->
            Front

        2 ->
            Back

        _ ->
            Spanning


epsilon =
    1.0e-5


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
                            Back

                        else if result > epsilon then
                            Front

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
    }


splitByPlane : Plane3d Meters c -> Face c -> IntersectionResult c
splitByPlane plane face =
    let
        classifiedFace =
            classifyFace plane face

        isFront : Face c -> Bool
        isFront face_ =
            face_.normalDirection
                |> Direction3d.toVector
                |> Vector3d.dot
                    (Plane3d.normalDirection plane
                        |> Direction3d.toVector
                    )
                |> Quantity.greaterThan Quantity.zero
    in
    case classifiedFace.class of
        Coplanar ->
            if isFront face then
                { front = Just face, back = Nothing }

            else
                { front = Nothing, back = Just face }

        Front ->
            { front = Just face, back = Nothing }

        Back ->
            { front = Nothing, back = Just face }

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

                maybeFrontPoint ( t1, _ ) =
                    if t1.class == Front || t1.class == Coplanar then
                        Just t1.point

                    else
                        Nothing

                maybeBackPoint ( t1, _ ) =
                    if t1.class == Back || t1.class == Coplanar then
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
                        { front = [ maybeFrontPoint tuple, maybeSplitPoint tuple ] |> List.filterMap identity
                        , back = [ maybeBackPoint tuple, maybeSplitPoint tuple ] |> List.filterMap identity
                        }
                    )
                |> List.foldl
                    (\{ front, back } acc ->
                        { acc
                            | front = acc.front ++ front
                            , back = acc.back ++ back
                        }
                    )
                    { front = [], back = [] }
                |> (\{ front, back } -> { front = toFace front, back = toFace back })
