module Tests.Cube exposing (applyAlgorithmTests, testHelperTests)

import Algorithm
import Cube
import Cube.Advanced
import Cube.Advanced.Types exposing (Color(..))
import Expect
import Expect.Extra
import Fuzz
import List.Nonempty
import Monads.ListM as ListM
import Parser exposing ((|.), (|=))
import Test exposing (..)
import TestHelpers.Cube exposing (cubeFuzzer, plainCubie, solvedCubeRendering)
import Tests.Algorithm exposing (algorithmFuzzer, turnDirectionFuzzer, turnFuzzer, turnableFuzzer)


applyAlgorithmTests : Test
applyAlgorithmTests =
    describe "applyAlgorithm"
        [ fuzz2 cubeFuzzer algorithmFuzzer "Applying an algorithm followed by its inverse results in the identity" <|
            \cube alg ->
                cube
                    |> Cube.applyAlgorithm alg
                    |> Cube.applyAlgorithm (Algorithm.inverse alg)
                    |> Expect.equal cube
        , fuzz2 cubeFuzzer turnFuzzer "Applying a single move is not an identity operation" <|
            \cube turn ->
                cube
                    |> Cube.applyAlgorithm (Algorithm.fromTurnList << List.singleton <| turn)
                    |> Expect.notEqual cube
        , fuzz3 cubeFuzzer algorithmFuzzer algorithmFuzzer "is associative, so applying combined or separated algs to cube should result in same cube" <|
            \cube alg1 alg2 ->
                let
                    appliedTogether =
                        cube |> Cube.applyAlgorithm (Algorithm.append alg1 alg2)

                    appliedSeparately =
                        cube |> Cube.applyAlgorithm alg1 |> Cube.applyAlgorithm alg2
                in
                appliedTogether |> Expect.equal appliedSeparately
        , fuzz2 commutativePairsFuzzer cubeFuzzer "parallel turns are commutative" <|
            \( turn1, turn2 ) cube ->
                Cube.applyAlgorithm (Algorithm.fromTurnList [ turn1, turn2 ]) cube
                    |> Expect.equal (Cube.applyAlgorithm (Algorithm.fromTurnList [ turn2, turn1 ]) cube)
        , fuzz2 nonCommutativePairsFuzzer cubeFuzzer "non parallel turns are not commutative" <|
            \( turn1, turn2 ) cube ->
                Cube.applyAlgorithm (Algorithm.fromTurnList [ turn1, turn2 ]) cube
                    |> Expect.notEqual (Cube.applyAlgorithm (Algorithm.fromTurnList [ turn2, turn1 ]) cube)
        , fuzz3 cubeFuzzer turnableFuzzer turnDirectionFuzzer "Applying a quarter turn twice equals applying a double turn" <|
            \cube turnable direction ->
                let
                    quarterTurn =
                        Algorithm.Turn turnable Algorithm.OneQuarter direction

                    doubleTurn =
                        Algorithm.Turn turnable Algorithm.Halfway direction

                    afterTwoQuarterTurns =
                        cube |> Cube.applyAlgorithm (Algorithm.fromTurnList [ quarterTurn, quarterTurn ])

                    afterOneHalfway =
                        cube |> Cube.applyAlgorithm (Algorithm.fromTurnList [ doubleTurn ])
                in
                afterTwoQuarterTurns |> Expect.equal afterOneHalfway
        , fuzz3 cubeFuzzer turnableFuzzer turnDirectionFuzzer "Applying a quarter turn thrice equals applying a triple turn" <|
            \cube turnable direction ->
                let
                    quarterTurn =
                        Algorithm.Turn turnable Algorithm.OneQuarter direction

                    tripleTurn =
                        Algorithm.Turn turnable Algorithm.ThreeQuarters direction

                    afterThreeQuarterTurns =
                        cube |> Cube.applyAlgorithm (Algorithm.fromTurnList [ quarterTurn, quarterTurn, quarterTurn ])

                    afterOneTripleTurn =
                        cube |> Cube.applyAlgorithm (Algorithm.fromTurnList [ tripleTurn ])
                in
                afterThreeQuarterTurns |> Expect.equal afterOneTripleTurn
        , fuzz3 cubeFuzzer turnableFuzzer turnDirectionFuzzer "Applying a quarter turn four times equals doing nothing" <|
            \cube turnable direction ->
                let
                    quarterTurn =
                        Algorithm.Turn turnable Algorithm.OneQuarter direction

                    afterFourQuarterTurns =
                        cube |> Cube.applyAlgorithm (Algorithm.fromTurnList [ quarterTurn, quarterTurn, quarterTurn, quarterTurn ])
                in
                afterFourQuarterTurns |> Expect.equal cube
        , fuzz2 cubeFuzzer turnFuzzer "Applying a NUM (e.g double, triple) turn in one direction equals applying a (4 - NUM) turn in the opposite direction" <|
            \cube ((Algorithm.Turn turnable length direction) as turn) ->
                let
                    flipDirection dir =
                        case dir of
                            Algorithm.Clockwise ->
                                Algorithm.CounterClockwise

                            Algorithm.CounterClockwise ->
                                Algorithm.Clockwise

                    flipLength len =
                        case len of
                            Algorithm.OneQuarter ->
                                Algorithm.ThreeQuarters

                            Algorithm.Halfway ->
                                Algorithm.Halfway

                            Algorithm.ThreeQuarters ->
                                Algorithm.OneQuarter

                    turnAlg =
                        Algorithm.fromTurnList << List.singleton <| turn

                    oppositeDirectionEquivalent =
                        Algorithm.fromTurnList << List.singleton <| Algorithm.Turn turnable (flipLength length) (flipDirection direction)
                in
                cube |> Cube.applyAlgorithm turnAlg |> Expect.equal (Cube.applyAlgorithm oppositeDirectionEquivalent cube)
        , test "solved cube has correct colors" <|
            \_ ->
                Cube.solved
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings solvedCubeRendering
        , test "U performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = RightColor } })
                            |> (\x -> { x | ufl = { plainCubie | u = UpColor, f = RightColor, l = FrontColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = FrontColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = LeftColor, l = FrontColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = LeftColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = LeftColor, r = BackColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = BackColor } })
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "D performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | dfl = { plainCubie | d = DownColor, f = LeftColor, l = BackColor } })
                            |> (\x -> { x | df = { plainCubie | d = DownColor, f = LeftColor } })
                            |> (\x -> { x | dfr = { plainCubie | d = DownColor, f = LeftColor, r = FrontColor } })
                            |> (\x -> { x | dr = { plainCubie | d = DownColor, r = FrontColor } })
                            |> (\x -> { x | dbr = { plainCubie | d = DownColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | db = { plainCubie | d = DownColor, b = RightColor } })
                            |> (\x -> { x | dbl = { plainCubie | d = DownColor, b = RightColor, l = BackColor } })
                            |> (\x -> { x | dl = { plainCubie | d = DownColor, l = BackColor } })
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "L performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ubl = { plainCubie | u = BackColor, b = DownColor, l = LeftColor } })
                            |> (\x -> { x | ul = { plainCubie | u = BackColor, l = LeftColor } })
                            |> (\x -> { x | ufl = { plainCubie | u = BackColor, f = UpColor, l = LeftColor } })
                            |> (\x -> { x | fl = { plainCubie | f = UpColor, l = LeftColor } })
                            |> (\x -> { x | dfl = { plainCubie | d = FrontColor, f = UpColor, l = LeftColor } })
                            |> (\x -> { x | dl = { plainCubie | d = FrontColor, l = LeftColor } })
                            |> (\x -> { x | dbl = { plainCubie | d = FrontColor, b = DownColor, l = LeftColor } })
                            |> (\x -> { x | bl = { plainCubie | b = DownColor, l = LeftColor } })
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "R performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = FrontColor, f = DownColor, r = RightColor } })
                            |> (\x -> { x | ur = { plainCubie | u = FrontColor, r = RightColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = FrontColor, b = UpColor, r = RightColor } })
                            |> (\x -> { x | br = { plainCubie | b = UpColor, r = RightColor } })
                            |> (\x -> { x | dbr = { plainCubie | d = BackColor, b = UpColor, r = RightColor } })
                            |> (\x -> { x | dr = { plainCubie | d = BackColor, r = RightColor } })
                            |> (\x -> { x | dfr = { plainCubie | d = BackColor, f = DownColor, r = RightColor } })
                            |> (\x -> { x | fr = { plainCubie | f = DownColor, r = RightColor } })
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "F performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufl = { plainCubie | u = LeftColor, f = FrontColor, l = DownColor } })
                            |> (\x -> { x | uf = { plainCubie | u = LeftColor, f = FrontColor } })
                            |> (\x -> { x | ufr = { plainCubie | u = LeftColor, f = FrontColor, r = UpColor } })
                            |> (\x -> { x | fr = { plainCubie | f = FrontColor, r = UpColor } })
                            |> (\x -> { x | dfr = { plainCubie | d = RightColor, f = FrontColor, r = UpColor } })
                            |> (\x -> { x | df = { plainCubie | d = RightColor, f = FrontColor } })
                            |> (\x -> { x | dfl = { plainCubie | d = RightColor, f = FrontColor, l = DownColor } })
                            |> (\x -> { x | fl = { plainCubie | f = FrontColor, l = DownColor } })
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "B performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ubr = { plainCubie | u = RightColor, b = BackColor, r = DownColor } })
                            |> (\x -> { x | ub = { plainCubie | u = RightColor, b = BackColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = RightColor, b = BackColor, l = UpColor } })
                            |> (\x -> { x | bl = { plainCubie | b = BackColor, l = UpColor } })
                            |> (\x -> { x | dbl = { plainCubie | d = LeftColor, b = BackColor, l = UpColor } })
                            |> (\x -> { x | db = { plainCubie | d = LeftColor, b = BackColor } })
                            |> (\x -> { x | dbr = { plainCubie | d = LeftColor, b = BackColor, r = DownColor } })
                            |> (\x -> { x | br = { plainCubie | b = BackColor, r = DownColor } })
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "M performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.M Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ub = { plainCubie | u = BackColor, b = DownColor } })
                            |> (\x -> { x | u = { plainCubie | u = BackColor } })
                            |> (\x -> { x | uf = { plainCubie | u = BackColor, f = UpColor } })
                            |> (\x -> { x | f = { plainCubie | f = UpColor } })
                            |> (\x -> { x | df = { plainCubie | d = FrontColor, f = UpColor } })
                            |> (\x -> { x | d = { plainCubie | d = FrontColor } })
                            |> (\x -> { x | db = { plainCubie | d = FrontColor, b = DownColor } })
                            |> (\x -> { x | b = { plainCubie | b = DownColor } })
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "S performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.S Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ul = { plainCubie | u = LeftColor, l = DownColor } })
                            |> (\x -> { x | u = { plainCubie | u = LeftColor } })
                            |> (\x -> { x | ur = { plainCubie | u = LeftColor, r = UpColor } })
                            |> (\x -> { x | r = { plainCubie | r = UpColor } })
                            |> (\x -> { x | dr = { plainCubie | d = RightColor, r = UpColor } })
                            |> (\x -> { x | d = { plainCubie | d = RightColor } })
                            |> (\x -> { x | dl = { plainCubie | d = RightColor, l = DownColor } })
                            |> (\x -> { x | l = { plainCubie | l = DownColor } })
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "E performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.E Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | fl = { plainCubie | f = LeftColor, l = BackColor } })
                            |> (\x -> { x | f = { plainCubie | f = LeftColor } })
                            |> (\x -> { x | fr = { plainCubie | f = LeftColor, r = FrontColor } })
                            |> (\x -> { x | r = { plainCubie | r = FrontColor } })
                            |> (\x -> { x | br = { plainCubie | b = RightColor, r = FrontColor } })
                            |> (\x -> { x | b = { plainCubie | b = RightColor } })
                            |> (\x -> { x | bl = { plainCubie | b = RightColor, l = BackColor } })
                            |> (\x -> { x | l = { plainCubie | l = BackColor } })
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "x performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.X Algorithm.OneQuarter Algorithm.Clockwise ]

                    -- The faces do the following transformation: U -> B -> D -> F -> U
                    expectedRendering =
                        { -- U Corners
                          ufr = { plainCubie | u = FrontColor, f = DownColor, r = RightColor }
                        , ufl = { plainCubie | u = FrontColor, f = DownColor, l = LeftColor }
                        , ubl = { plainCubie | u = FrontColor, b = UpColor, l = LeftColor }
                        , ubr = { plainCubie | u = FrontColor, b = UpColor, r = RightColor }

                        -- D Corners
                        , dbr = { plainCubie | d = BackColor, b = UpColor, r = RightColor }
                        , dbl = { plainCubie | d = BackColor, b = UpColor, l = LeftColor }
                        , dfl = { plainCubie | d = BackColor, f = DownColor, l = LeftColor }
                        , dfr = { plainCubie | d = BackColor, f = DownColor, r = RightColor }

                        -- M Edges
                        , uf = { plainCubie | u = FrontColor, f = DownColor }
                        , ub = { plainCubie | u = FrontColor, b = UpColor }
                        , db = { plainCubie | d = BackColor, b = UpColor }
                        , df = { plainCubie | d = BackColor, f = DownColor }

                        -- S Edges
                        , dl = { plainCubie | d = BackColor, l = LeftColor }
                        , dr = { plainCubie | d = BackColor, r = RightColor }
                        , ur = { plainCubie | u = FrontColor, r = RightColor }
                        , ul = { plainCubie | u = FrontColor, l = LeftColor }

                        -- E Edges
                        , fl = { plainCubie | f = DownColor, l = LeftColor }
                        , fr = { plainCubie | f = DownColor, r = RightColor }
                        , br = { plainCubie | b = UpColor, r = RightColor }
                        , bl = { plainCubie | b = UpColor, l = LeftColor }

                        -- Centers
                        , u = { plainCubie | u = FrontColor }
                        , d = { plainCubie | d = BackColor }
                        , f = { plainCubie | f = DownColor }
                        , b = { plainCubie | b = UpColor }
                        , l = { plainCubie | l = LeftColor }
                        , r = { plainCubie | r = RightColor }
                        }
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "y performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Y Algorithm.OneQuarter Algorithm.Clockwise ]

                    -- The faces do the following transformation: F -> L -> B -> R -> F
                    expectedRendering =
                        { -- U Corners
                          ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor }
                        , ufl = { plainCubie | u = UpColor, f = RightColor, l = FrontColor }
                        , ubl = { plainCubie | u = UpColor, b = LeftColor, l = FrontColor }
                        , ubr = { plainCubie | u = UpColor, b = LeftColor, r = BackColor }

                        -- D Corners
                        , dbr = { plainCubie | d = DownColor, b = LeftColor, r = BackColor }
                        , dbl = { plainCubie | d = DownColor, b = LeftColor, l = FrontColor }
                        , dfl = { plainCubie | d = DownColor, f = RightColor, l = FrontColor }
                        , dfr = { plainCubie | d = DownColor, f = RightColor, r = BackColor }

                        -- M Edges
                        , uf = { plainCubie | u = UpColor, f = RightColor }
                        , ub = { plainCubie | u = UpColor, b = LeftColor }
                        , db = { plainCubie | d = DownColor, b = LeftColor }
                        , df = { plainCubie | d = DownColor, f = RightColor }

                        -- S Edges
                        , dl = { plainCubie | d = DownColor, l = FrontColor }
                        , dr = { plainCubie | d = DownColor, r = BackColor }
                        , ur = { plainCubie | u = UpColor, r = BackColor }
                        , ul = { plainCubie | u = UpColor, l = FrontColor }

                        -- E Edges
                        , fl = { plainCubie | f = RightColor, l = FrontColor }
                        , fr = { plainCubie | f = RightColor, r = BackColor }
                        , br = { plainCubie | b = LeftColor, r = BackColor }
                        , bl = { plainCubie | b = LeftColor, l = FrontColor }

                        -- Centers
                        , u = { plainCubie | u = UpColor }
                        , d = { plainCubie | d = DownColor }
                        , f = { plainCubie | f = RightColor }
                        , b = { plainCubie | b = LeftColor }
                        , l = { plainCubie | l = FrontColor }
                        , r = { plainCubie | r = BackColor }
                        }
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "z performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Z Algorithm.OneQuarter Algorithm.Clockwise ]

                    -- The faces do the following transformation: U -> R -> D -> L -> U
                    expectedRendering =
                        { -- U Corners
                          ufr = { plainCubie | u = LeftColor, f = FrontColor, r = UpColor }
                        , ufl = { plainCubie | u = LeftColor, f = FrontColor, l = DownColor }
                        , ubl = { plainCubie | u = LeftColor, b = BackColor, l = DownColor }
                        , ubr = { plainCubie | u = LeftColor, b = BackColor, r = UpColor }

                        -- D Corners
                        , dbr = { plainCubie | d = RightColor, b = BackColor, r = UpColor }
                        , dbl = { plainCubie | d = RightColor, b = BackColor, l = DownColor }
                        , dfl = { plainCubie | d = RightColor, f = FrontColor, l = DownColor }
                        , dfr = { plainCubie | d = RightColor, f = FrontColor, r = UpColor }

                        -- M Edges
                        , uf = { plainCubie | u = LeftColor, f = FrontColor }
                        , ub = { plainCubie | u = LeftColor, b = BackColor }
                        , db = { plainCubie | d = RightColor, b = BackColor }
                        , df = { plainCubie | d = RightColor, f = FrontColor }

                        -- S Edges
                        , dl = { plainCubie | d = RightColor, l = DownColor }
                        , dr = { plainCubie | d = RightColor, r = UpColor }
                        , ur = { plainCubie | u = LeftColor, r = UpColor }
                        , ul = { plainCubie | u = LeftColor, l = DownColor }

                        -- E Edges
                        , fl = { plainCubie | f = FrontColor, l = DownColor }
                        , fr = { plainCubie | f = FrontColor, r = UpColor }
                        , br = { plainCubie | b = BackColor, r = UpColor }
                        , bl = { plainCubie | b = BackColor, l = DownColor }

                        -- Centers
                        , u = { plainCubie | u = LeftColor }
                        , d = { plainCubie | d = RightColor }
                        , f = { plainCubie | f = FrontColor }
                        , b = { plainCubie | b = BackColor }
                        , l = { plainCubie | l = DownColor }
                        , r = { plainCubie | r = UpColor }
                        }
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "0-length algorithm is identity operation to simplify types despite 0 length algorithm not making much sense" <|
            \_ ->
                Cube.solved |> Cube.applyAlgorithm Algorithm.empty |> Expect.equal Cube.solved
        ]


testHelperTests : Test
testHelperTests =
    describe "test helper tests"
        [ describe "parallel turns"
            [ test "up or down group is disjoint with front or back group" <|
                \_ ->
                    listsDisjoint upOrDownParallelGroup frontOrBackParallelGroup
                        |> Expect.true "Expected groups to be disjoint"
            , test "up or down group is disjoint with left or right group" <|
                \_ ->
                    listsDisjoint upOrDownParallelGroup leftOrRightParallelGroup
                        |> Expect.true "Expected groups to be disjoint"
            , test "front or back group is disjoint with left or right group" <|
                \_ ->
                    listsDisjoint frontOrBackParallelGroup leftOrRightParallelGroup
                        |> Expect.true "Expected groups to be disjoint"
            , test "three parallel groups have same length as all turns" <|
                \_ ->
                    [ upOrDownParallelGroup, frontOrBackParallelGroup, leftOrRightParallelGroup ]
                        |> List.map List.Nonempty.length
                        |> List.sum
                        |> Expect.equal (List.Nonempty.length Algorithm.allTurns)
            , test "commutativePairs and nonCommutativePairs are disjoint" <|
                \_ ->
                    List.filter (\commutative -> List.member commutative nonCommutativePairs) commutativePairs
                        |> Expect.equal []
            , test "commutativePairs + nonCommutativePairs have same length as amount of pairs of turns" <|
                \_ ->
                    let
                        numTurns =
                            List.Nonempty.length Algorithm.allTurns

                        -- Every turn is matched with all turns that haven't been matched with yet
                        -- to avoid (a, b) (b, a) duplicates. This gives us an arithmetic sequence
                        -- of numTurns + (numTurns - 1) + ... + 1 which can be calculated with
                        -- the below formula, where we can safely assume numTurns is even (and
                        -- otherwise the test should fail anyway!)
                        numUniquePairs =
                            (numTurns + 1) * (numTurns // 2)
                    in
                    [ commutativePairs, nonCommutativePairs ]
                        |> List.map List.length
                        |> List.sum
                        |> Expect.equal numUniquePairs
            ]
        ]


listsDisjoint : List.Nonempty.Nonempty a -> List.Nonempty.Nonempty a -> Bool
listsDisjoint a b =
    List.filter (\aa -> List.Nonempty.member aa b) (List.Nonempty.toList a) == []


commutativePairsFuzzer : Fuzz.Fuzzer ( Algorithm.Turn, Algorithm.Turn )
commutativePairsFuzzer =
    commutativePairs
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


nonCommutativePairsFuzzer : Fuzz.Fuzzer ( Algorithm.Turn, Algorithm.Turn )
nonCommutativePairsFuzzer =
    nonCommutativePairs
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


nonCommutativePairs : List ( Algorithm.Turn, Algorithm.Turn )
nonCommutativePairs =
    uniqueCartesianProductWithSelf Algorithm.allTurns
        |> List.Nonempty.toList
        |> List.filter (\anyPair -> not <| List.member anyPair commutativePairs)


commutativePairs : List ( Algorithm.Turn, Algorithm.Turn )
commutativePairs =
    List.concatMap (uniqueCartesianProductWithSelf >> List.Nonempty.toList)
        [ upOrDownParallelGroup, frontOrBackParallelGroup, leftOrRightParallelGroup ]
        ++ List.Nonempty.toList nonParallelCommutativePairs


uniqueCartesianProductWithSelf : List.Nonempty.Nonempty Algorithm.Turn -> List.Nonempty.Nonempty ( Algorithm.Turn, Algorithm.Turn )
uniqueCartesianProductWithSelf group =
    ListM.return Tuple.pair
        |> ListM.applicative (ListM.fromNonemptyList group)
        |> ListM.applicative (ListM.fromNonemptyList group)
        |> ListM.toNonemptyList
        |> List.Nonempty.map
            (\( a, b ) ->
                if compareTurns a b == GT then
                    ( b, a )

                else
                    ( a, b )
            )
        |> List.Nonempty.sortWith
            (\( aa, ab ) ( ba, bb ) ->
                if compareTurns aa ba /= EQ then
                    compareTurns aa ba

                else
                    compareTurns ab bb
            )
        |> List.Nonempty.uniq


{-| There are some moves that are commutative despite not being parallel to each other
-}
nonParallelCommutativePairs : List.Nonempty.Nonempty ( Algorithm.Turn, Algorithm.Turn )
nonParallelCommutativePairs =
    let
        sliceAndRotationTurnables =
            List.Nonempty.filter
                (\x -> isSlice x || isWholeRotation x)
                Algorithm.M
                Algorithm.allTurnables

        allDoubleSliceOrRotationTurns =
            ListM.return Algorithm.Turn
                |> ListM.applicative (ListM.fromNonemptyList sliceAndRotationTurnables)
                |> ListM.applicative (ListM.fromNonemptyList (List.Nonempty.singleton Algorithm.Halfway))
                |> ListM.applicative (ListM.fromNonemptyList Algorithm.allTurnDirections)
                |> ListM.toNonemptyList
    in
    uniqueCartesianProductWithSelf allDoubleSliceOrRotationTurns
        -- Remove the parallel ones
        |> List.Nonempty.filter
            (Tuple.mapBoth getParallelGroup getParallelGroup >> (\( a, b ) -> a /= b))
            ( Algorithm.Turn Algorithm.X Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.Y Algorithm.Halfway Algorithm.Clockwise
            )


isSlice : Algorithm.Turnable -> Bool
isSlice turnable =
    List.member turnable [ Algorithm.M, Algorithm.S, Algorithm.E ]


isWholeRotation : Algorithm.Turnable -> Bool
isWholeRotation turnable =
    List.member turnable [ Algorithm.X, Algorithm.Y, Algorithm.Z ]


compareTurns : Algorithm.Turn -> Algorithm.Turn -> Order
compareTurns (Algorithm.Turn turnableA lengthA directionA) (Algorithm.Turn turnableB lengthB directionB) =
    let
        turnable =
            compareTurnables turnableA turnableB

        length =
            compareTurnLengths lengthA lengthB

        direction =
            compareTurnDirections directionA directionB
    in
    if turnable /= EQ then
        turnable

    else if length /= EQ then
        length

    else
        direction


compareTurnables : Algorithm.Turnable -> Algorithm.Turnable -> Order
compareTurnables =
    compareByListOrder Algorithm.allTurnables


compareTurnLengths : Algorithm.TurnLength -> Algorithm.TurnLength -> Order
compareTurnLengths =
    compareByListOrder Algorithm.allTurnLengths


compareTurnDirections : Algorithm.TurnDirection -> Algorithm.TurnDirection -> Order
compareTurnDirections =
    compareByListOrder Algorithm.allTurnDirections


compareByListOrder : List.Nonempty.Nonempty a -> a -> a -> Order
compareByListOrder order a b =
    let
        orderedElements =
            List.Nonempty.filter (\x -> x == a || x == b) a order
    in
    if a == b then
        EQ

    else
        List.Nonempty.head orderedElements
            -- Unsafe function in the sense that we assume there are the two elements
            -- we expect, so weird stuff could happen with bad inputs
            |> (\x ->
                    if x == a then
                        LT

                    else
                        GT
               )


type ParallelGroup
    = UpOrDownGroup
    | FrontOrBackGroup
    | LeftOrRightGroup


upOrDownParallelGroup : List.Nonempty.Nonempty Algorithm.Turn
upOrDownParallelGroup =
    List.partition
        (getParallelGroup >> (==) UpOrDownGroup)
        (Algorithm.allTurns |> List.Nonempty.toList)
        |> Tuple.first
        |> List.Nonempty.fromList
        |> (Maybe.withDefault <|
                List.Nonempty.singleton (Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise)
           )


frontOrBackParallelGroup : List.Nonempty.Nonempty Algorithm.Turn
frontOrBackParallelGroup =
    List.partition
        (getParallelGroup >> (==) FrontOrBackGroup)
        (Algorithm.allTurns |> List.Nonempty.toList)
        |> Tuple.first
        |> List.Nonempty.fromList
        |> (Maybe.withDefault <|
                List.Nonempty.singleton (Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise)
           )


leftOrRightParallelGroup : List.Nonempty.Nonempty Algorithm.Turn
leftOrRightParallelGroup =
    List.partition
        (getParallelGroup >> (==) LeftOrRightGroup)
        (Algorithm.allTurns |> List.Nonempty.toList)
        |> Tuple.first
        |> List.Nonempty.fromList
        |> (Maybe.withDefault <|
                List.Nonempty.singleton (Algorithm.Turn Algorithm.L Algorithm.Halfway Algorithm.Clockwise)
           )


getParallelGroup : Algorithm.Turn -> ParallelGroup
getParallelGroup turn =
    case turn of
        Algorithm.Turn Algorithm.U _ _ ->
            UpOrDownGroup

        Algorithm.Turn Algorithm.D _ _ ->
            UpOrDownGroup

        Algorithm.Turn Algorithm.Uw _ _ ->
            UpOrDownGroup

        Algorithm.Turn Algorithm.Dw _ _ ->
            UpOrDownGroup

        Algorithm.Turn Algorithm.E _ _ ->
            UpOrDownGroup

        Algorithm.Turn Algorithm.Y _ _ ->
            UpOrDownGroup

        Algorithm.Turn Algorithm.F _ _ ->
            FrontOrBackGroup

        Algorithm.Turn Algorithm.B _ _ ->
            FrontOrBackGroup

        Algorithm.Turn Algorithm.Fw _ _ ->
            FrontOrBackGroup

        Algorithm.Turn Algorithm.Bw _ _ ->
            FrontOrBackGroup

        Algorithm.Turn Algorithm.S _ _ ->
            FrontOrBackGroup

        Algorithm.Turn Algorithm.Z _ _ ->
            FrontOrBackGroup

        Algorithm.Turn Algorithm.L _ _ ->
            LeftOrRightGroup

        Algorithm.Turn Algorithm.R _ _ ->
            LeftOrRightGroup

        Algorithm.Turn Algorithm.Lw _ _ ->
            LeftOrRightGroup

        Algorithm.Turn Algorithm.Rw _ _ ->
            LeftOrRightGroup

        Algorithm.Turn Algorithm.M _ _ ->
            LeftOrRightGroup

        Algorithm.Turn Algorithm.X _ _ ->
            LeftOrRightGroup
