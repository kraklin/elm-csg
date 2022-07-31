module Csg.PlaneBased.BspTree exposing (..)

import Csg.PlaneBased.Face as PlaneBasedFace exposing (PlaneBasedFace, PlaneEquation)


type BspTree
    = Empty
    | Node { face : PlaneBasedFace, plane : PlaneEquation, inside : BspTree, outside : BspTree }


emptyTree : BspTree
emptyTree =
    Empty


build : List PlaneBasedFace -> BspTree
build faces =
    List.foldl insert emptyTree faces


toFaces : BspTree -> List PlaneBasedFace
toFaces tree =
    let
        traverse t =
            case t of
                Empty ->
                    []

                Node { face, inside, outside } ->
                    face :: traverse inside ++ traverse outside
    in
    traverse tree


insert : PlaneBasedFace -> BspTree -> BspTree
insert face tree =
    let
        handleInside plane maybeFace node =
            maybeFace
                |> Maybe.map
                    (\newFace ->
                        if node.inside == Empty then
                            { node | inside = Node { face = newFace, inside = Empty, outside = Empty, plane = plane } }

                        else
                            { node | inside = insert newFace node.inside }
                    )
                |> Maybe.withDefault node

        handleOutside plane maybeFace node =
            maybeFace
                |> Maybe.map
                    (\newFace ->
                        if node.outside == Empty then
                            { node | outside = Node { face = newFace, inside = Empty, outside = Empty, plane = plane } }

                        else
                            { node | outside = insert newFace node.outside }
                    )
                |> Maybe.withDefault node
    in
    case tree of
        Empty ->
            Node { plane = face.supportPlane, inside = Empty, outside = Empty, face = face }

        Node treeNode ->
            PlaneBasedFace.splitByPlane treeNode.plane face
                |> (\{ inside, outside } ->
                        treeNode
                            |> handleInside face.supportPlane inside
                            |> handleOutside face.supportPlane outside
                            |> Node
                   )


mapFaces : (PlaneBasedFace -> PlaneBasedFace) -> BspTree -> BspTree
mapFaces f tree =
    let
        traverse t =
            case t of
                Empty ->
                    Empty

                Node nodeData ->
                    Node
                        { nodeData
                            | face = f nodeData.face
                            , inside = traverse nodeData.inside
                            , outside = traverse nodeData.outside
                        }
    in
    traverse tree
