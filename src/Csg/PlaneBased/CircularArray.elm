module Csg.PlaneBased.CircularArray exposing (..)

import Array exposing (Array)


type alias CircularArray a =
    { selectedIdx : Int
    , array : ( a, Array a )
    }


fromCons : a -> Array a -> CircularArray a
fromCons element array =
    { selectedIdx = 0
    , array = ( element, array )
    }


fromList : a -> List a -> CircularArray a
fromList first rest =
    fromCons first (Array.fromList rest)


toList : CircularArray a -> List a
toList { array } =
    Tuple.first array :: Array.toList (Tuple.second array)


map : (a -> b) -> CircularArray a -> CircularArray b
map fn circularArray =
    let
        updatedArray : ( b, Array b )
        updatedArray =
            circularArray.array
                |> Tuple.mapBoth fn (Array.map fn)
    in
    { selectedIdx = circularArray.selectedIdx
    , array = updatedArray
    }


length : CircularArray a -> Int
length { array } =
    Tuple.second array
        |> Array.length
        |> (+) 1


select : Int -> CircularArray a -> CircularArray a
select idx array =
    { array | selectedIdx = modBy (length array) idx }


selectNext : CircularArray a -> CircularArray a
selectNext array =
    { array | selectedIdx = modBy (length array) (array.selectedIdx + 1) }


selectPrevious : CircularArray a -> CircularArray a
selectPrevious array =
    { array | selectedIdx = modBy (length array) (array.selectedIdx - 1) }


getAt : Int -> CircularArray a -> a
getAt idx ({ array } as circularArray) =
    let
        sanitizedIdx =
            modBy (length circularArray) idx
    in
    if sanitizedIdx == 0 then
        Tuple.first array

    else
        Tuple.second array
            |> Array.get (sanitizedIdx - 1)
            |> Maybe.withDefault (Tuple.first array)


getCurrent : CircularArray a -> a
getCurrent ({ selectedIdx } as circularArray) =
    getAt selectedIdx circularArray


getPrevious : CircularArray a -> a
getPrevious ({ selectedIdx } as circularArray) =
    getAt selectedIdx circularArray


getNext : CircularArray a -> a
getNext ({ selectedIdx } as circularArray) =
    getAt selectedIdx circularArray
