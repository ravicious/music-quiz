module ZipList.Nonempty exposing
    ( ZipList
    , fromList, singleton
    , current, toList, length
    , forward, backward
    )

{-| A `ZipList` is a collection which can be moved forward/backward and that exposes a single current element

Copied from <https://package.elm-lang.org/packages/Guid75/ziplist/1.0.1> and modified to be nonempty.


# ZipLists

@docs ZipList


# Creation

@docs fromList, singleton


# Consultation

@docs current, toList, length


# Moving

@docs forward, backward

-}

import Maybe


{-| A collection data type that can be moved forward/backward and that exposes a current element (see the `current` function)
-}
type ZipList a
    = Zipper (List a) a (List a)


{-| Craft a new ZipList out of a List
-}
fromList : List a -> Maybe (ZipList a)
fromList list =
    case list of
        [] ->
            Nothing

        head :: queue ->
            Just <| Zipper [] head queue


{-| Create a new ZipList with a single element in it
-}
singleton : a -> ZipList a
singleton item =
    Zipper [] item []


{-| Return the current element of a ZipList. `Nothing` will be returned if the ziplist is empty
-}
current : ZipList a -> a
current (Zipper _ elem _) =
    elem


{-| Move forward a `ZipList`
-}
forward : ZipList a -> ZipList a
forward (Zipper before elem after) =
    case after of
        [] ->
            Zipper before elem after

        head :: queue ->
            Zipper (elem :: before) head queue


{-| Move backward a `ZipList`
-}
backward : ZipList a -> ZipList a
backward (Zipper before elem after) =
    case before of
        [] ->
            Zipper before elem after

        head :: queue ->
            Zipper queue head (elem :: after)


{-| Convert a `ZipList` into a `List`
-}
toList : ZipList a -> List a
toList (Zipper before elem after) =
    List.concat
        [ List.reverse before
        , [ elem ]
        , after
        ]


{-| Return a `ZipList` length
-}
length : ZipList a -> Int
length (Zipper before elem after) =
    1 + List.length before + List.length after
