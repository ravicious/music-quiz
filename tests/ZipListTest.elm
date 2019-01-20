module ZipListTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import List.Nonempty
import Test exposing (..)
import ZipList.Nonempty as ZipList exposing (ZipList)


suite : Test
suite =
    describe "The ZipList module"
        [ fuzz (list int) "The fromList function" <|
            wrapListFuzz <|
                \randomList zipList ->
                    Expect.equal randomList <| ZipList.toList zipList
        , fuzz2 (list int) (intRange 0 20) "The fromNonemptyList function" <|
            \randomList n ->
                case List.Nonempty.fromList randomList of
                    Just nonEmptyList ->
                        nonEmptyList
                            |> ZipList.fromNonemptyList
                            |> applyNTimes n ZipList.forward
                            |> ZipList.toNonemptyList
                            |> Expect.equal nonEmptyList

                    Nothing ->
                        Expect.pass
        , describe "The singleton function"
            [ test "A singleton should have a length of 1" <|
                \_ ->
                    Expect.equal 1 (ZipList.singleton 1 |> ZipList.length)
            , test "The current item of a ZipList singleton should be the given item at construction time" <|
                \_ ->
                    Expect.equal 1 (ZipList.singleton 1 |> ZipList.current)
            ]
        , describe "The current function"
            [ test "A non empty list" <|
                \_ ->
                    ZipList.fromList [ 1, 2 ]
                        |> Maybe.map ZipList.current
                        |> Maybe.map (Expect.equal 1)
                        |> Maybe.withDefault (Expect.fail "ZipList.Nonempty.fromList returned Nothing given a non-empty list")
            ]
        , describe "The forward function" <|
            [ moveTest "already at the list end" ZipList.forward [ 1 ] 1
            , moveTest "still some moves to make" ZipList.forward [ 1, 2 ] 2
            ]
        , describe "The backward function" <|
            [ moveTest "already at the list beginning" ZipList.backward [ 1 ] 1
            , moveTest "still some moves to make" (ZipList.forward >> ZipList.backward) [ 1, 2 ] 1
            ]
        , fuzz (list int) "The length function" <|
            wrapListFuzz <|
                \randomList zipList ->
                    Expect.equal (List.length randomList) (ZipList.length zipList)
        ]


{-| Given a "move" function, a list of item, returns an expectation that will check that the current
value of the built ziplist will be equal to the given expected value
-}
moveTest : String -> (ZipList a -> ZipList a) -> List a -> a -> Test
moveTest testLabel moveFunc list expectedValue =
    test testLabel <|
        \_ ->
            let
                maybeZipList =
                    ZipList.fromList list
            in
            case maybeZipList of
                Just zipList ->
                    Expect.equal expectedValue (ZipList.current (moveFunc zipList))

                Nothing ->
                    Expect.fail "ZipList.Nonempty.fromList retuned Nothing"


{-| Given a fuzzed list, calls `ZipList.Nonempty.fromList` on it and handles impossible cases and
then passes it to the given function.
-}
wrapListFuzz : (List a -> ZipList a -> Expectation) -> List a -> Expectation
wrapListFuzz f randomList =
    if List.isEmpty randomList then
        Expect.pass

    else
        case ZipList.fromList randomList of
            Just zipList ->
                f randomList zipList

            Nothing ->
                Expect.fail "ZipList.Nonempty.fromList returned Nothing given a non-empty list"


applyNTimes : Int -> (a -> a) -> a -> a
applyNTimes n f x =
    if n < 1 then
        x

    else
        case n of
            1 ->
                f x

            _ ->
                applyNTimes (n - 1) f (f x)
