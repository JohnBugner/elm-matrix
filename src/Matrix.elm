module Matrix exposing
    ( Matrix
    , empty, initialize, repeat, fromList
    , isEmpty, size, get
    , set, slice
    , toList, toIndexedList
    , map, indexedMap
    )

{-|

# Matrices
@docs Matrix

# Creation
@docs empty, initialize, repeat, fromList

# Query
@docs isEmpty, size, get

# Manipulate
@docs set, slice

# Lists
@docs toList, toIndexedList

# Transform
@docs map, indexedMap
-}

import Array exposing (Array)
import List
import Maybe

-- Matrices

{-| Stores elements in a rectangular 2-dimensional grid. Uses arrays internally.
-}
type alias Matrix a =
    { size : (Int,Int)
    , arrays : Array (Array a)
    }

-- Creation

{-| Create an empty matrix.
-}
empty : Matrix a
empty = Matrix (0,0) Array.empty

{-| Create a matrix of a given size, with the element at index `(x,y)` initialized to the result of `f (x,y)`.
-}
initialize : (Int,Int) -> ((Int,Int) -> a) -> Matrix a
initialize (w,h) f =
    { size = (w,h)
    , arrays = Array.initialize h (\ y -> Array.initialize w (\ x -> f (x,y)))
    }

{-| Create a matrix of a given size, filled with a default element.
-}
repeat : (Int,Int) -> a -> Matrix a
repeat (w,h) v =
    { size = (w,h)
    , arrays = Array.repeat h (Array.repeat w v)
    }

{-| Create an array from a list of lists.
The height will be either 0 or the length of the outer list.
The width will be as wide as possible while keeping the matrix rectangular.
If the width is 0, then the height is also 0, and vice-versa.
-}
fromList : List (List a) -> Matrix a
fromList list =
    let
        w =
            List.map List.length list
            |> List.minimum
            |> Maybe.withDefault 0
        h =
            if w == 0
            then 0
            else List.length list
    in
        { size = (w,h)
        , arrays =
            List.map Array.fromList list
            |> List.map (Array.slice 0 w)
            |> List.filter (not << Array.isEmpty)
            |> Array.fromList
        }

-- Query

{-| Determine if a matrix is empty.
-}
isEmpty : Matrix a -> Bool
isEmpty mx = mx.size == (0,0)

{-| Return the dimensions of a matrix.
-}
size : Matrix a -> (Int,Int)
size mx = mx.size

{-| Return `Just` the element at the index or `Nothing` if the index is out of bounds.
-}
get : (Int,Int) -> Matrix a -> Maybe a
get (x,y) mx = Array.get y mx.arrays |> Maybe.andThen (Array.get x)

-- Manipulate

{-| Set the element at a particular index. Returns an updated matrix. If the index is out of bounds, the matrix is unaltered.
-}
set : (Int,Int) -> a -> Matrix a -> Matrix a 
set (x,y) v mx =
    case Array.get y mx.arrays of
        Just row -> { mx | arrays = Array.set y (Array.set x v row) mx.arrays }
        Nothing -> mx

{-| Get a sub-section of a matrix: `slice startIndex endIndex array`. The slice extracts from `startIndex` up to but not including `endIndex`.
-}
slice : (Int,Int) -> (Int,Int) -> Matrix a -> Matrix a
slice (xb,yb) (xe,ye) mx =
    let
        newArrays : Array (Array a)
        newArrays = Array.slice yb ye mx.arrays |> Array.map (Array.slice xb xe)
    in
        { size =
            case Array.get 0 newArrays of
                Just row -> (Array.length row, Array.length newArrays)
                Nothing -> (0, Array.length newArrays)
        , arrays = newArrays
        }

-- Lists

{-| Create a list of lists of elements from a matrix.
-}
toList : Matrix a -> List (List a)
toList mx = Array.map Array.toList mx.arrays |> Array.toList

{-| Create a list of indexed elements from a matrix. Each element of the matrix will be paired with its index.
-}
toIndexedList : Matrix a -> List ((Int,Int),a)
toIndexedList mx =
    toList mx
    |> List.indexedMap (\ y row -> List.indexedMap (\ x v -> ((x,y),v)) row)
    |> List.concat

-- Transform

{-| Apply a function to every element in a matrix.
-}
map : (a -> b) -> Matrix a -> Matrix b
map f mx =
    { size = mx.size
    , arrays = Array.map (\ row -> Array.map f row) mx.arrays
    }

{-| Apply a function to every element in a matrix with its index as the first argument.
-}
indexedMap : ((Int,Int) -> a -> b) -> Matrix a -> Matrix b
indexedMap f mx =
    { size = mx.size
    , arrays = Array.indexedMap (\ y row -> Array.indexedMap (\ x value -> f (x,y) value) row) mx.arrays
    }
