module BspTree exposing (..)

import Bitwise
import Dict
import Direction3d exposing (Direction3d)
import Length exposing (Meters)
import List
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Unitless)
import Triangle3d exposing (Triangle3d)
import Vector3d exposing (Vector3d)


type alias Face coordinates =
    { triangles : ( Triangle3d Meters coordinates, List (Triangle3d Meters coordinates) )
    , normal : Vector3d Unitless coordinates
    }


allTriangles : Face c -> List (Triangle3d Meters c)
allTriangles { triangles } =
    let
        ( first, rest ) =
            triangles
    in
    first :: rest


planeFromFace : Face c -> Maybe (Plane3d Meters c)
planeFromFace { normal, triangles } =
    let
        firstPoint =
            Tuple.first triangles
                |> Triangle3d.vertices
                |> (\( v1, _, _ ) -> v1)

        maybeDirection =
            Vector3d.direction normal
    in
    Maybe.map (Plane3d.through firstPoint) maybeDirection


type alias NodeData c =
    { faces : Face c
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
                    faces :: (traverse inside ++ traverse outside)
    in
    traverse tree


fromTriangles : Vector3d Unitless coordinates -> List (Triangle3d Meters coordinates) -> Maybe (Face coordinates)
fromTriangles normal triangles =
    case triangles of
        [] ->
            Nothing

        first :: rest ->
            Just <| { triangles = ( first, rest ), normal = normal }


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
                            | faces = fn nodeData.faces
                            , inside = traverse nodeData.inside
                            , outside = traverse nodeData.outside
                        }
    in
    traverse tree


translate : Vector3d Meters c -> BspTree c -> BspTree c
translate vector tree =
    let
        translateFace f =
            allTriangles f
                |> List.map (Triangle3d.translateBy vector)
                |> fromTriangles f.normal
                |> Maybe.withDefault f
    in
    case tree of
        Empty ->
            Empty

        Node nodeData ->
            Node
                { nodeData
                    | faces = translateFace nodeData.faces
                    , inside = translate vector nodeData.inside
                    , outside = translate vector nodeData.outside
                    , plane = Plane3d.translateBy vector nodeData.plane
                }


insert : Face c -> BspTree c -> BspTree c
insert face tree =
    let
        maybePlane =
            planeFromFace face

        newNode plane newFace =
            { faces = newFace, outside = Empty, inside = Empty, plane = plane }

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
    maybePlane
        |> Maybe.map
            (\plane ->
                case tree of
                    Empty ->
                        Node <| newNode plane face

                    Node rootData ->
                        divide rootData.plane face
                            |> (\{ inside, outside } ->
                                    rootData
                                        |> handleOutside plane outside
                                        |> handleInside plane inside
                                        |> Node
                               )
            )
        |> Maybe.withDefault Empty


divide : Plane3d Meters c -> Face c -> { inside : Maybe (Face c), outside : Maybe (Face c) }
divide splittingPlane face =
    let
        createFace triangles =
            case triangles of
                [] ->
                    Nothing

                [ one ] ->
                    Just { triangles = ( one, [] ), normal = face.normal }

                first :: rest ->
                    Just { triangles = ( first, rest ), normal = face.normal }
    in
    allTriangles face
        |> List.map (splitByPlane splittingPlane)
        |> List.foldl
            (\{ front, back } acc ->
                { inside = back ++ acc.inside
                , outside = front ++ acc.outside
                }
            )
            { inside = [], outside = [] }
        |> (\{ inside, outside } ->
                { inside = createFace inside, outside = createFace outside }
           )


clip : BspTree c -> BspTree c -> BspTree c
clip t1 t2 =
    t2
        |> toFaces
        |> List.filterMap (\f -> clipFace f t1)
        |> build


invert : BspTree c -> BspTree c
invert tree =
    let
        invertTriangle triangle =
            Triangle3d.vertices triangle
                |> (\( v1, v2, v3 ) -> Triangle3d.from v3 v2 v1)

        invertFace : Face c -> Face c
        invertFace f =
            { normal = Vector3d.minus f.normal Vector3d.zero
            , triangles = Tuple.mapBoth invertTriangle (List.map invertTriangle) f.triangles
            }
    in
    case tree of
        Empty ->
            Empty

        Node nodeData ->
            Node
                { nodeData
                    | faces = invertFace nodeData.faces
                    , inside = invert nodeData.inside
                    , outside = invert nodeData.outside
                    , plane = Plane3d.flip nodeData.plane
                }


clipFace : Face c -> BspTree c -> Maybe (Face c)
clipFace clippedFace clippingTree =
    let
        arePlanesSame treePlane face =
            case planeFromFace face of
                Nothing ->
                    False

                Just plane ->
                    treePlane == plane

        combineFace ( maybeA, maybeB ) =
            case ( maybeA, maybeB ) of
                ( Nothing, Nothing ) ->
                    Nothing

                ( Just a, Nothing ) ->
                    Just a

                ( Nothing, Just b ) ->
                    Just b

                ( Just a, Just b ) ->
                    case allTriangles a ++ allTriangles b of
                        [] ->
                            Nothing

                        first :: rest ->
                            Just { triangles = ( first, rest ), normal = b.normal }

        handleInside : Maybe (Face c) -> BspTree c -> Maybe (Face c)
        handleInside maybeFace tree =
            maybeFace
                |> Maybe.andThen
                    (\face ->
                        case tree of
                            Empty ->
                                Just face

                            Node _ ->
                                clipFace face tree
                    )

        handleOutside : Maybe (Face c) -> BspTree c -> Maybe (Face c)
        handleOutside maybeFace tree =
            maybeFace
                |> Maybe.andThen
                    (\face ->
                        case tree of
                            Empty ->
                                Nothing

                            Node _ ->
                                clipFace face tree
                    )
    in
    case clippingTree of
        Empty ->
            Nothing

        Node rootData ->
            if arePlanesSame rootData.plane clippedFace then
                Nothing

            else
                divide rootData.plane clippedFace
                    |> (\{ inside, outside } ->
                            ( handleInside inside rootData.inside, handleOutside outside rootData.outside )
                                |> combineFace
                       )


dedup : List (Point3d Meters c) -> List (Point3d Meters c)
dedup list =
    list
        |> List.map (\point -> ( Point3d.toTuple Length.inMeters point, point ))
        |> Dict.fromList
        |> Dict.values


type alias IntersectionResult c =
    { front : List (Triangle3d Meters c)
    , back : List (Triangle3d Meters c)
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


type alias ClassifiedTriangle c =
    { v1 : ClassifiedVertex c
    , v2 : ClassifiedVertex c
    , v3 : ClassifiedVertex c
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


combineClasses : Classification -> Classification -> Classification
combineClasses c1 c2 =
    Bitwise.or (classToNumber c1) (classToNumber c2)
        |> numberToClass


classifyTriangle : Plane3d Meters c -> Triangle3d Meters c -> ClassifiedTriangle c
classifyTriangle plane triangle =
    let
        ( v1, v2, v3 ) =
            Triangle3d.vertices triangle

        planeNormal =
            Plane3d.normalDirection plane |> Direction3d.toVector

        ( vec1, vec2, vec3 ) =
            ( Vector3d.from Point3d.origin v1
            , Vector3d.from Point3d.origin v2
            , Vector3d.from Point3d.origin v3
            )

        w =
            Vector3d.dot planeNormal (Vector3d.from Point3d.origin (Plane3d.originPoint plane))

        t v =
            -- var t = this.normal.dot(polygon.vertices[i].pos) - this.w;
            Quantity.minus w (Vector3d.dot planeNormal v)
                |> Quantity.compare Quantity.zero
                |> (\result ->
                        case result of
                            LT ->
                                Front

                            GT ->
                                Back

                            EQ ->
                                Coplanar
                   )

        ( c1, c2, c3 ) =
            ( t vec1
            , t vec2
            , t vec3
            )

        combinedClasses =
            combineClasses c1 c2
                |> combineClasses c3
    in
    { class = combinedClasses
    , v1 = { point = v1, class = c1 }
    , v2 = { point = v2, class = c2 }
    , v3 = { point = v3, class = c3 }
    }


splitByPlane : Plane3d Meters c -> Triangle3d Meters c -> IntersectionResult c
splitByPlane plane triangle =
    let
        classifiedTriangle =
            classifyTriangle plane triangle

        ( v1, v2, v3 ) =
            ( classifiedTriangle.v1
            , classifiedTriangle.v2
            , classifiedTriangle.v3
            )
    in
    case classifiedTriangle.class of
        Coplanar ->
            let
                flippedTriangle =
                    Triangle3d.from
                        classifiedTriangle.v3.point
                        classifiedTriangle.v2.point
                        classifiedTriangle.v1.point
            in
            { front = [ triangle ], back = [ flippedTriangle ] }

        Front ->
            { front = [ triangle ], back = [] }

        Back ->
            { front = [], back = [ triangle ] }

        Spanning ->
            let
                planeNormal =
                    Plane3d.normalDirection plane |> Direction3d.toVector

                maybeSplitPoint ( t1, t2 ) =
                    if combineClasses t1.class t2.class == Spanning then
                        let
                            va =
                                Vector3d.from Point3d.origin t1.point

                            --(this.w - this.normal.dot(vi.pos)) / this.normal.dot(vj.pos.minus(vi.pos));
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

                toTriangles points =
                    case points of
                        [ p1, p2, p3 ] ->
                            [ Triangle3d.from p1 p2 p3 ]

                        [ p1, p2, p3, p4 ] ->
                            [ Triangle3d.from p1 p2 p3, Triangle3d.from p3 p4 p1 ]

                        _ ->
                            []
            in
            [ ( v1, v2 ), ( v2, v3 ), ( v3, v1 ) ]
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
                |> (\{ front, back } -> { front = toTriangles front, back = toTriangles back })
