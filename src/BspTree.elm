module BspTree exposing (..)

import Dict
import Direction3d exposing (Direction3d)
import Length exposing (Meters)
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
                    [ faces ] ++ traverse inside ++ traverse outside
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
        |> List.map (cutByPlane splittingPlane)
        |> Debug.log "cuts"
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


clip : BspTree c -> BspTree c -> List (Face c)
clip t1 t2 =
    t2
        |> toFaces
        |> List.filterMap (\f -> clipFace f t1)


clipFace : Face c -> BspTree c -> Maybe (Face c)
clipFace clippedFace clippingTree =
    let
        maybePlane =
            case clippingTree of
                Empty ->
                    Nothing

                Node { plane } ->
                    Just plane

        arePlanesSame treePlane face =
            case planeFromFace face of
                Nothing ->
                    False

                Just plane ->
                    treePlane == plane

        combineFace ( maybeA, maybeB ) =
            --TODO: check they have same normal and are coplanar
            let
                toList =
                    Maybe.map allTriangles >> Maybe.withDefault []
            in
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
                    |> Debug.log "result"


dedup : List (Point3d Meters c) -> List (Point3d Meters c)
dedup list =
    list
        |> List.map (\point -> ( Point3d.toTuple Length.inMeters point, point ))
        |> Dict.fromList
        |> Dict.values


cutByPlane : Plane3d Meters c -> Triangle3d Meters c -> { front : List (Triangle3d Meters c), back : List (Triangle3d Meters c) }
cutByPlane plane triangle =
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
                |> dedup

        ( onFront, onBack ) =
            ( [ pa, pb, pc ] |> List.filter (\p -> p.order == GT || p.order == EQ) |> List.map .point
            , [ pa, pb, pc ] |> List.filter (\p -> p.order == LT) |> List.map .point
            )

        ( d1f, d2f, d3f ) =
            ( Quantity.unwrap d1, Quantity.unwrap d2, Quantity.unwrap d3 )

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
            Debug.log "make triangles" <|
                case intersectionPoints of
                    [ i1, i2 ] ->
                        case points of
                            [ onePoint ] ->
                                [ Triangle3d.from onePoint i1 i2 ]

                            [ firstPoint, secondPoint ] ->
                                [ Triangle3d.from firstPoint (sortPoints firstPoint i1 i2 |> Tuple.first) (sortPoints firstPoint i1 i2 |> Tuple.second)
                                , Triangle3d.from firstPoint (sortPoints firstPoint i1 i2 |> Tuple.second) secondPoint
                                ]

                            [ v1, v2, v3 ] ->
                                [ Triangle3d.from v1 v2 v3 ]

                            _ ->
                                []

                    rest ->
                        let
                            _ =
                                Debug.log "intersection point" rest
                        in
                        []
    in
    if d1f > 0 && d2f > 0 && d3f > 0 then
        { front = [ triangle ], back = [] }

    else if d1f <= 0 && d2f <= 0 && d3f <= 0 then
        { front = [], back = [ triangle ] }

    else
        { front = makeTriangles onFront, back = makeTriangles onBack }
