module Tests.Monads.ListM exposing (suite)

import Expect
import Fuzz
import List.Nonempty
import Monads.ListM as ListM
import Test exposing (..)


suite : Test
suite =
    describe "Monads.List"
        [ describe "applicative"
            [ fuzz2 (nonemptyListFuzzer Fuzz.int) (nonemptyListFuzzer Fuzz.int) "Handles nondeterminism correctly, taking the cartesian product with a single function" <|
                \ints1 ints2 ->
                    ListM.return (+)
                        |> ListM.applicative (ListM.fromNonemptyList ints1)
                        |> ListM.applicative (ListM.fromNonemptyList ints2)
                        |> (ListM.toNonemptyList >> List.Nonempty.length)
                        |> Expect.equal
                            (List.Nonempty.length ints1
                                * List.Nonempty.length ints2
                            )
            , fuzz2 (nonemptyListFuzzer Fuzz.int) (nonemptyListFuzzer Fuzz.int) "Handles nondeterminsm correctly, taking the cartesian product with several functions" <|
                \ints1 ints2 ->
                    ListM.fromNonemptyList (List.Nonempty.Nonempty (+) [ (-) ])
                        |> ListM.applicative (ListM.fromNonemptyList ints1)
                        |> ListM.applicative (ListM.fromNonemptyList ints2)
                        |> (ListM.toNonemptyList >> List.Nonempty.length)
                        |> Expect.equal (2 * List.Nonempty.length ints1 * List.Nonempty.length ints2)
            , test "executes a specific example as expected" <|
                \_ ->
                    ListM.fromNonemptyList (List.Nonempty.Nonempty (+) [ (-) ])
                        |> ListM.applicative (ListM.fromNonemptyList (List.Nonempty.Nonempty 1 [ 2 ]))
                        |> ListM.applicative (ListM.fromNonemptyList (List.Nonempty.Nonempty 3 [ 4, 5 ]))
                        |> ListM.toNonemptyList
                        |> List.Nonempty.toList
                        |> Expect.equalLists [ 4, 5, 6, 5, 6, 7, -2, -3, -4, -1, -2, -3 ]
            ]
        ]


nonemptyListFuzzer : Fuzz.Fuzzer a -> Fuzz.Fuzzer (List.Nonempty.Nonempty a)
nonemptyListFuzzer fuzzer =
    Fuzz.map2 List.Nonempty.Nonempty fuzzer (Fuzz.list fuzzer)
