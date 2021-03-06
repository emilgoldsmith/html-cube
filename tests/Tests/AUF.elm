module Tests.AUF exposing (aufFuzzer, fromStringTests, toStringTests)

import AUF exposing (AUF)
import Algorithm
import Expect
import Fuzz
import List.Nonempty
import PLL exposing (PLL(..))
import Test exposing (..)
import Tests.Algorithm


toStringTests : Test
toStringTests =
    describe "toString"
        [ test "works for None" <|
            \_ ->
                AUF.toString AUF.None
                    |> Expect.equal ""
        , test "works for Clockwise" <|
            \_ ->
                AUF.toString AUF.Clockwise
                    |> Expect.equal "U"
        , test "works for CounterClockwise" <|
            \_ ->
                AUF.toString AUF.CounterClockwise
                    |> Expect.equal "U'"
        , test "works for Halfway" <|
            \_ ->
                AUF.toString AUF.Halfway
                    |> Expect.equal "U2"
        , fuzz aufFuzzer "is consistent with Algorithm.toString" <|
            \auf ->
                AUF.toAlgorithm auf
                    |> Algorithm.toString
                    |> Expect.equal (AUF.toString auf)
        ]


fromStringTests : Test
fromStringTests =
    describe "fromString"
        [ fuzz aufFuzzer "stringified auf decodes to original value" <|
            \auf ->
                auf
                    |> AUF.toString
                    |> AUF.fromString
                    |> Expect.equal (Ok auf)
        , test "passes a specific example" <|
            \_ ->
                AUF.fromString "U2"
                    |> Expect.equal (Ok AUF.Halfway)
        , test "Nonsense fails" <|
            \_ ->
                AUF.fromString "asfdsarewqreqwafs"
                    |> Expect.err
        , fuzz Tests.Algorithm.algorithmFuzzer "Only passes for algorithms of length one using the U face" <|
            \algorithm ->
                let
                    turnList =
                        Algorithm.toTurnList algorithm

                    isExpectedToBeAUF =
                        case turnList of
                            [ Algorithm.Turn Algorithm.U _ _ ] ->
                                True

                            _ ->
                                False

                    algorithmString =
                        Algorithm.toString algorithm
                in
                AUF.fromString algorithmString
                    |> (if isExpectedToBeAUF then
                            Expect.ok

                        else
                            Expect.err
                       )
        , fuzz2
            Tests.Algorithm.turnLengthFuzzer
            Tests.Algorithm.turnDirectionFuzzer
            "Passes all valid aufs"
          <|
            \length direction ->
                Algorithm.fromTurnList [ Algorithm.Turn Algorithm.U length direction ]
                    |> Algorithm.toString
                    |> AUF.fromString
                    |> Expect.ok
        , test "handles a ton of weird whitespace" <|
            \_ ->
                AUF.fromString "  \t \t    \t\t\t"
                    |> Expect.equal (Ok AUF.None)
        ]


aufFuzzer : Fuzz.Fuzzer AUF
aufFuzzer =
    Fuzz.oneOf <|
        List.map Fuzz.constant (List.Nonempty.toList AUF.all)
