module Tests.PLL exposing (getAlgorithmTests, referenceAlgTests)

import Algorithm
import Cube
import Cube.Advanced
import Cube.Advanced.Types as CubeTypes exposing (Color(..))
import Expect
import List.Nonempty
import PLL
import Test exposing (..)
import TestHelpers.Cube exposing (plainCubie, solvedCubeRendering)


referenceAlgTests : Test
referenceAlgTests =
    describe "referenceAlgs"
        [ test "H perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = BackColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = FrontColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = RightColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = LeftColor } })
                in
                PLL.referenceAlgorithms.h
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Ua perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = LeftColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = FrontColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = RightColor } })
                in
                PLL.referenceAlgorithms.ua
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Ub perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = RightColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = FrontColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = LeftColor } })
                in
                PLL.referenceAlgorithms.ub
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Z perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = RightColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = FrontColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = BackColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = LeftColor } })
                in
                PLL.referenceAlgorithms.z
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Aa perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = LeftColor, r = BackColor } })
                in
                PLL.referenceAlgorithms.aa
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Ab perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = RightColor, l = BackColor } })
                in
                PLL.referenceAlgorithms.ab
                    |> expectEqualDisregardingAUF expectedRendering
        , test "E perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = LeftColor, l = FrontColor } })
                            |> (\x -> { x | ufl = { plainCubie | u = UpColor, f = LeftColor, l = BackColor } })
                in
                PLL.referenceAlgorithms.e
                    |> expectEqualDisregardingAUF expectedRendering
        , test "F perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = FrontColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = BackColor } })
                in
                PLL.referenceAlgorithms.f
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Ga perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = LeftColor, r = BackColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = BackColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = FrontColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = RightColor } })
                in
                PLL.referenceAlgorithms.ga
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Gb perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = RightColor, l = BackColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = RightColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = FrontColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = BackColor } })
                in
                PLL.referenceAlgorithms.gb
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Gc perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = RightColor, l = BackColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = RightColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = LeftColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = BackColor } })
                in
                PLL.referenceAlgorithms.gc
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Gd perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = LeftColor, r = BackColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = BackColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = LeftColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = RightColor } })
                in
                PLL.referenceAlgorithms.gd
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Ja perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = RightColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = BackColor } })
                in
                PLL.referenceAlgorithms.ja
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Jb perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = RightColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = FrontColor } })
                in
                PLL.referenceAlgorithms.jb
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Na perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = BackColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = FrontColor } })
                in
                PLL.referenceAlgorithms.na
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Nb perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = RightColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = LeftColor } })
                in
                PLL.referenceAlgorithms.nb
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Ra perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = LeftColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = BackColor } })
                in
                PLL.referenceAlgorithms.ra
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Rb perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = LeftColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = FrontColor } })
                in
                PLL.referenceAlgorithms.rb
                    |> expectEqualDisregardingAUF expectedRendering
        , test "T perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = LeftColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = RightColor } })
                in
                PLL.referenceAlgorithms.t
                    |> expectEqualDisregardingAUF expectedRendering
        , test "V perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = LeftColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = FrontColor } })
                in
                PLL.referenceAlgorithms.v
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Y perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = RightColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = FrontColor } })
                in
                PLL.referenceAlgorithms.y
                    |> expectEqualDisregardingAUF expectedRendering
        ]


getAlgorithmTests : Test
getAlgorithmTests =
    describe "getAlgorithm"
        [ test "gets the algorithm from the provided algorithm set" <|
            \_ ->
                let
                    referenceAlgorithms =
                        PLL.referenceAlgorithms

                    expectedAlgorithm =
                        Algorithm.build [ Algorithm.Turn Algorithm.M Algorithm.OneQuarter Algorithm.CounterClockwise ]

                    algorithmSet =
                        { referenceAlgorithms | aa = expectedAlgorithm }
                in
                PLL.getAlgorithm algorithmSet PLL.Aa
                    |> Expect.equal expectedAlgorithm
        ]


expectEqualDisregardingAUF : CubeTypes.Rendering -> Algorithm.Algorithm -> Expect.Expectation
expectEqualDisregardingAUF expectedRendering alg =
    let
        algWithAllAufs =
            Algorithm.aufs
                |> List.Nonempty.map (Algorithm.append alg)
                |> List.Nonempty.concatMap (\withPreAuf -> List.Nonempty.map (Algorithm.appendTo withPreAuf) Algorithm.aufs)

        candidates =
            algWithAllAufs
                |> List.Nonempty.map ((\x -> Cube.applyAlgorithm x Cube.solved) >> Cube.Advanced.render)
    in
    List.filter ((==) expectedRendering) (List.Nonempty.toList candidates)
        |> List.length
        |> Expect.greaterThan 0
        |> Expect.onFail
            ("Algorithm with or without pre and post AUF did not produce the expected rendering. Closest diff was:"
                ++ "\n\n"
                ++ "(Actual != Expected)"
                ++ "\n\n"
                ++ getShortestDiff candidates expectedRendering
            )


getShortestDiff : List.Nonempty.Nonempty CubeTypes.Rendering -> CubeTypes.Rendering -> String
getShortestDiff candidates expected =
    let
        diffs =
            List.Nonempty.map (\x -> TestHelpers.Cube.compareCubeRenderings x expected) candidates
    in
    List.Nonempty.foldl1 getShorterString diffs


getShorterString : String -> String -> String
getShorterString a b =
    if String.length a < String.length b then
        a

    else
        b
