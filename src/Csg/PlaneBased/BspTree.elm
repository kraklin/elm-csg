module Csg.PlaneBased.BspTree exposing (..)

import Csg.PlaneBased.Face as PlaneBasedFace exposing (PlaneBasedFace, PlaneEquation)


type BspTree
    = Leaf (List PlaneBasedFace)
    | Node { plane : PlaneEquation, inside : BspTree, outside : BspTree }


emptyTree : BspTree
emptyTree =
    Leaf []
