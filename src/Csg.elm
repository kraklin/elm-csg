module Csg exposing
    ( toLines
    , toMesh
    , toTriangularMesh
    , toTriangularMeshGroupedByColor
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


toMesh : Shape3d coordinates -> List (Triangle coordinates)
toMesh shape =
    Csg.Shape3d.toTree shape
        |> BspTree.toFaces
        |> List.map facesToTriangles
        |> List.concat


toLines : Shape3d coordinates -> List (LineSegment3d Length.Meters coordinates)
toLines shape =
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
    Csg.Shape3d.toTree shape
        |> BspTree.toFaces
        |> List.map facesToTriangles
        |> List.concat
        |> List.map triangleSegments
        |> List.concat


type alias VertexWithNormal coordinates =
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    }


toTriangularMesh :
    Shape3d coordinates
    -> TriangularMesh (VertexWithNormal coordinates)
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
    Csg.Shape3d.toTree shape
        |> BspTree.toFaces
        |> List.map toFan
        |> TriangularMesh.combine


toTriangularMeshGroupedByColor :
    Shape3d coordinates
    ->
        List
            ( TriangularMesh (VertexWithNormal coordinates)
            , Color
            )
toTriangularMeshGroupedByColor shape =
    let
        toVertex face point =
            { position = point
            , normal =
                Direction3d.toVector face.normalDirection
                    |> Vector3d.normalize
            }

        toColoredMeshMap :
            Face c
            -> Dict ( Float, Float, Float ) (List (TriangularMesh (VertexWithNormal c)))
            -> Dict ( Float, Float, Float ) (List (TriangularMesh (VertexWithNormal c)))
        toColoredMeshMap face coloredMeshMap =
            let
                newMesh =
                    NonEmpty.tail face.points
                        |> List.map (toVertex face)
                        |> TriangularMesh.fan (toVertex face (NonEmpty.head face.points))

                faceColorKey =
                    Color.toRgba face.color
                        |> (\{ red, green, blue } -> ( red, green, blue ))
            in
            if Dict.member faceColorKey coloredMeshMap then
                Dict.update faceColorKey
                    (Maybe.map (\list -> newMesh :: list))
                    coloredMeshMap

            else
                Dict.insert faceColorKey [ newMesh ] coloredMeshMap
    in
    Csg.Shape3d.toTree shape
        |> BspTree.toFaces
        |> List.foldl toColoredMeshMap Dict.empty
        |> Dict.map (\( r, g, b ) meshes -> ( TriangularMesh.combine meshes, Color.rgb r g b ))
        |> Dict.values
