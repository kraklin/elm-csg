module Csg exposing
    ( planeBasedTriangularMesh
    , toLineSegments
    , toLinesAndNormals
    , toPlaneBased
    , toTriangles
    , toTriangularMesh
    , toTriangularMeshGroupedByColor
    )

import BspTree exposing (Face)
import Color exposing (Color)
import Csg.PlaneBased as PlaneBased exposing (PlaneBasedFace)
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


type alias Triangle c =
    { vertices : ( Vertex c, Vertex c, Vertex c )
    , color : Color
    }


facesToTriangles : Face coordinates -> List (Triangle coordinates)
facesToTriangles ({ normalDirection, color } as face) =
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
                        { pPrevious = pNext, triangles = { vertices = withNormal ( p1, pPrevious, pNext ), color = color } :: triangles }
                    )
                    { pPrevious = p2, triangles = [] }
                |> .triangles

        _ ->
            []


toTriangles : Shape3d coordinates -> List (Triangle coordinates)
toTriangles shape =
    Csg.Shape3d.toFaces shape
        |> List.map facesToTriangles
        |> List.concat


toLineSegments : Shape3d coordinates -> List (LineSegment3d Meters coordinates)
toLineSegments =
    toLinesAndNormals False


toLinesAndNormals : Bool -> Shape3d coordinates -> List (LineSegment3d Length.Meters coordinates)
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


toTriangularMesh : Shape3d coordinates -> TriangularMesh (Vertex coordinates)
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


toTriangularMeshGroupedByColor : Shape3d coordinates -> List ( TriangularMesh (Vertex coordinates), Color )
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
            -> Dict ( Float, Float, Float ) (List (TriangularMesh (Vertex c)))
            -> Dict ( Float, Float, Float ) (List (TriangularMesh (Vertex c)))
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
    Csg.Shape3d.toFaces shape
        |> List.foldl toColoredMeshMap Dict.empty
        |> Dict.map (\( r, g, b ) meshes -> ( TriangularMesh.combine meshes, Color.rgb r g b ))
        |> Dict.values



-- Plane based CSG


toPlaneBased : Shape3d c -> List (PlaneBasedFace c)
toPlaneBased shape =
    shape
        |> Csg.Shape3d.toFaces
        |> List.take 1
        |> List.filterMap PlaneBased.fromFace


planeBasedTriangularMesh : List (PlaneBasedFace c) -> TriangularMesh (Vertex c)
planeBasedTriangularMesh planeBasedFaces =
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
    planeBasedFaces
        |> List.map PlaneBased.toFace
        |> Debug.log "face"
        |> List.map toFan
        |> TriangularMesh.combine
