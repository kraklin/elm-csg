module Csg exposing
    ( toLineSegments
    , toLinesAndNormals
    , toTriangles
    , toTriangularMesh
    , toTriangularMeshGroupedByTag
    )

import BspTree exposing (Face)
import Color exposing (Color)
import Csg.Shape3d exposing (Shape3d)
import Dict exposing (Dict)
import Direction3d
import Length exposing (Meters)
import LineSegment3d exposing (LineSegment3d)
import List.NonEmpty as NonEmpty
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)



-- Rendering to Mesh or Wireframe


type alias Vertex coordinates =
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    }


type alias Triangle tag c =
    { vertices : ( Vertex c, Vertex c, Vertex c )
    , tag : Maybe tag
    }


facesToTriangles : Face tag coordinates -> List (Triangle tag coordinates)
facesToTriangles ({ normalDirection, tag } as face) =
    let
        withNormal ( v1, v2, v3 ) =
            ( { position = v1, normal = Direction3d.toVector normalDirection }
            , { position = v2, normal = Direction3d.toVector normalDirection }
            , { position = v3, normal = Direction3d.toVector normalDirection }
            )
    in
    case BspTree.allPoints face of
        p1 :: p2 :: rest ->
            rest
                |> List.foldl
                    (\pNext { pPrevious, triangles } ->
                        { pPrevious = pNext, triangles = { vertices = withNormal ( p1, pPrevious, pNext ), tag = tag } :: triangles }
                    )
                    { pPrevious = p2, triangles = [] }
                |> .triangles

        _ ->
            []


toTriangles : Shape3d tag coordinates -> List (Triangle tag coordinates)
toTriangles shape =
    Csg.Shape3d.toFaces shape
        |> List.map facesToTriangles
        |> List.concat


toLineSegments : Shape3d tag coordinates -> List (LineSegment3d Meters coordinates)
toLineSegments =
    toLinesAndNormals False


toLinesAndNormals : Bool -> Shape3d tag coordinates -> List (LineSegment3d Length.Meters coordinates)
toLinesAndNormals withNormals shape =
    let
        centroid : ( Vertex c, Vertex c, Vertex c ) -> Point3d Meters c
        centroid ( v1, v2, v3 ) =
            Point3d.centroid3 v1.position v2.position v3.position

        normalEnd : ( Vertex c, Vertex c, Vertex c ) -> Point3d Meters c
        normalEnd (( v1, _, _ ) as vertices) =
            Point3d.translateBy
                (Vector3d.toUnitless v1.normal
                    |> Vector3d.fromMeters
                    |> Vector3d.scaleBy 0.2
                )
                (centroid vertices)

        triangleSegments ( v1, v2, v3 ) =
            [ LineSegment3d.from v1.position v2.position
            , LineSegment3d.from v2.position v3.position
            , LineSegment3d.from v3.position v1.position
            ]
                ++ (if withNormals then
                        [ LineSegment3d.from (centroid ( v1, v2, v3 )) (normalEnd ( v1, v2, v3 )) ]

                    else
                        []
                   )
    in
    Csg.Shape3d.toFaces shape
        |> List.map facesToTriangles
        |> List.concat
        |> List.map (.vertices >> triangleSegments)
        |> List.concat


toTriangularMesh : Shape3d tag coordinates -> TriangularMesh (Vertex coordinates)
toTriangularMesh shape =
    let
        toVertex face point =
            { position = point
            , normal =
                Direction3d.toVector face.normalDirection
                    |> Vector3d.normalize
            }

        toFan face =
            NonEmpty.tail face.points
                |> List.map (toVertex face)
                |> TriangularMesh.fan (toVertex face (NonEmpty.head face.points))
    in
    Csg.Shape3d.toFaces shape
        |> List.map toFan
        |> TriangularMesh.combine


toTriangularMeshGroupedByTag : (Maybe tag -> comparable) -> Shape3d tag coordinates -> List ( TriangularMesh (Vertex coordinates), comparable )
toTriangularMeshGroupedByTag toComparable shape =
    let
        toVertex face point =
            { position = point
            , normal =
                Direction3d.toVector face.normalDirection
                    |> Vector3d.normalize
            }

        toColoredMeshMap :
            Face tag c
            -> Dict comparable (List (TriangularMesh (Vertex c)))
            -> Dict comparable (List (TriangularMesh (Vertex c)))
        toColoredMeshMap face coloredMeshMap =
            let
                newMesh =
                    NonEmpty.tail face.points
                        |> List.map (toVertex face)
                        |> TriangularMesh.fan (toVertex face (NonEmpty.head face.points))

                faceColorKey =
                    face.tag |> toComparable
            in
            if Dict.member faceColorKey coloredMeshMap then
                Dict.update faceColorKey
                    (Maybe.map (\list -> newMesh :: list))
                    coloredMeshMap

            else
                Dict.insert faceColorKey [ newMesh ] coloredMeshMap
    in
    Csg.Shape3d.toFaces shape
        |> List.foldl toColoredMeshMap Dict.empty
        |> Dict.map (\key meshes -> ( TriangularMesh.combine meshes, key ))
        |> Dict.values



