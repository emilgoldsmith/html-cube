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
import TestHelpers.Cube exposing (cubeFuzzer, solvedCubeRendering)
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
                        cycleColorsFromSolvedCubeOnSlices [ U ] [ FrontColor, LeftColor, BackColor, RightColor ]
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
                        cycleColorsFromSolvedCubeOnSlices [ D ] [ FrontColor, RightColor, BackColor, LeftColor ]
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
                        cycleColorsFromSolvedCubeOnSlices [ L ] [ UpColor, FrontColor, DownColor, BackColor ]
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
                        cycleColorsFromSolvedCubeOnSlices [ R ] [ UpColor, BackColor, DownColor, FrontColor ]
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
                        cycleColorsFromSolvedCubeOnSlices [ F ] [ UpColor, RightColor, DownColor, LeftColor ]
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
                        cycleColorsFromSolvedCubeOnSlices [ B ] [ UpColor, LeftColor, DownColor, RightColor ]
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
                        cycleColorsFromSolvedCubeOnSlices [ M ] [ UpColor, FrontColor, DownColor, BackColor ]
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
                        cycleColorsFromSolvedCubeOnSlices [ S ] [ UpColor, RightColor, DownColor, LeftColor ]
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
                        cycleColorsFromSolvedCubeOnSlices [ E ] [ FrontColor, RightColor, BackColor, LeftColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "Uw performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Uw Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ U, E ] [ FrontColor, LeftColor, BackColor, RightColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "Dw performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Dw Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ D, E ] [ FrontColor, RightColor, BackColor, LeftColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "Lw performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Lw Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ L, M ] [ UpColor, FrontColor, DownColor, BackColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "Rw performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Rw Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ R, M ] [ UpColor, BackColor, DownColor, FrontColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "Fw performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Fw Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ F, S ] [ UpColor, RightColor, DownColor, LeftColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "Bw performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Bw Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ B, S ] [ UpColor, LeftColor, DownColor, RightColor ]
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

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ L, M, R ] [ UpColor, BackColor, DownColor, FrontColor ]
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

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ U, E, D ] [ FrontColor, LeftColor, BackColor, RightColor ]
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

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ F, S, B ] [ UpColor, RightColor, DownColor, LeftColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "0-length algorithm is identity operation which can be useful for things like AUF modelling" <|
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


type Slice
    = U
    | D
    | L
    | R
    | F
    | B
    | M
    | S
    | E


cycleColorsFromSolvedCubeOnSlices : List Slice -> List Color -> Cube.Advanced.Types.Rendering
cycleColorsFromSolvedCubeOnSlices slices colors =
    List.foldl
        (\slice rendering ->
            case slice of
                U ->
                    { rendering
                        | ufl = cycleCubieRendering colors rendering.ufl
                        , uf = cycleCubieRendering colors rendering.uf
                        , ufr = cycleCubieRendering colors rendering.ufr
                        , ur = cycleCubieRendering colors rendering.ur
                        , ubr = cycleCubieRendering colors rendering.ubr
                        , ub = cycleCubieRendering colors rendering.ub
                        , ubl = cycleCubieRendering colors rendering.ubl
                        , ul = cycleCubieRendering colors rendering.ul
                    }

                D ->
                    { rendering
                        | dfl = cycleCubieRendering colors rendering.dfl
                        , df = cycleCubieRendering colors rendering.df
                        , dfr = cycleCubieRendering colors rendering.dfr
                        , dr = cycleCubieRendering colors rendering.dr
                        , dbr = cycleCubieRendering colors rendering.dbr
                        , db = cycleCubieRendering colors rendering.db
                        , dbl = cycleCubieRendering colors rendering.dbl
                        , dl = cycleCubieRendering colors rendering.dl
                    }

                L ->
                    { rendering
                        | ufl = cycleCubieRendering colors rendering.ufl
                        , ul = cycleCubieRendering colors rendering.ul
                        , ubl = cycleCubieRendering colors rendering.ubl
                        , bl = cycleCubieRendering colors rendering.bl
                        , dbl = cycleCubieRendering colors rendering.dbl
                        , dl = cycleCubieRendering colors rendering.dl
                        , dfl = cycleCubieRendering colors rendering.dfl
                        , fl = cycleCubieRendering colors rendering.fl
                    }

                R ->
                    { rendering
                        | ufr = cycleCubieRendering colors rendering.ufr
                        , ur = cycleCubieRendering colors rendering.ur
                        , ubr = cycleCubieRendering colors rendering.ubr
                        , br = cycleCubieRendering colors rendering.br
                        , dbr = cycleCubieRendering colors rendering.dbr
                        , dr = cycleCubieRendering colors rendering.dr
                        , dfr = cycleCubieRendering colors rendering.dfr
                        , fr = cycleCubieRendering colors rendering.fr
                    }

                F ->
                    { rendering
                        | ufr = cycleCubieRendering colors rendering.ufr
                        , uf = cycleCubieRendering colors rendering.uf
                        , ufl = cycleCubieRendering colors rendering.ufl
                        , fl = cycleCubieRendering colors rendering.fl
                        , dfl = cycleCubieRendering colors rendering.dfl
                        , df = cycleCubieRendering colors rendering.df
                        , dfr = cycleCubieRendering colors rendering.dfr
                        , fr = cycleCubieRendering colors rendering.fr
                    }

                B ->
                    { rendering
                        | ubr = cycleCubieRendering colors rendering.ubr
                        , ub = cycleCubieRendering colors rendering.ub
                        , ubl = cycleCubieRendering colors rendering.ubl
                        , bl = cycleCubieRendering colors rendering.bl
                        , dbl = cycleCubieRendering colors rendering.dbl
                        , db = cycleCubieRendering colors rendering.db
                        , dbr = cycleCubieRendering colors rendering.dbr
                        , br = cycleCubieRendering colors rendering.br
                    }

                M ->
                    { rendering
                        | uf = cycleCubieRendering colors rendering.uf
                        , u = cycleCubieRendering colors rendering.u
                        , ub = cycleCubieRendering colors rendering.ub
                        , b = cycleCubieRendering colors rendering.b
                        , db = cycleCubieRendering colors rendering.db
                        , d = cycleCubieRendering colors rendering.d
                        , df = cycleCubieRendering colors rendering.df
                        , f = cycleCubieRendering colors rendering.f
                    }

                S ->
                    { rendering
                        | u = cycleCubieRendering colors rendering.u
                        , ul = cycleCubieRendering colors rendering.ul
                        , l = cycleCubieRendering colors rendering.l
                        , dl = cycleCubieRendering colors rendering.dl
                        , d = cycleCubieRendering colors rendering.d
                        , dr = cycleCubieRendering colors rendering.dr
                        , r = cycleCubieRendering colors rendering.r
                        , ur = cycleCubieRendering colors rendering.ur
                    }

                E ->
                    { rendering
                        | f = cycleCubieRendering colors rendering.f
                        , fr = cycleCubieRendering colors rendering.fr
                        , r = cycleCubieRendering colors rendering.r
                        , br = cycleCubieRendering colors rendering.br
                        , b = cycleCubieRendering colors rendering.b
                        , bl = cycleCubieRendering colors rendering.bl
                        , l = cycleCubieRendering colors rendering.l
                        , fl = cycleCubieRendering colors rendering.fl
                    }
        )
        solvedCubeRendering
        slices


cycleCubieRendering : List Color -> Cube.Advanced.Types.CubieRendering -> Cube.Advanced.Types.CubieRendering
cycleCubieRendering colors cubie =
    { u = cycleColor colors cubie.u
    , d = cycleColor colors cubie.d
    , l = cycleColor colors cubie.l
    , r = cycleColor colors cubie.r
    , f = cycleColor colors cubie.f
    , b = cycleColor colors cubie.b
    }


cycleColor : List Color -> Color -> Color
cycleColor cycle color =
    let
        afterAndBefore =
            -- We go from the right as U -> F -> R means we need to do the following
            -- assignments R=F, F=U, U=R which goes from right to left
            List.foldr
                (\currentColor currentAfterAndBefore ->
                    if List.length currentAfterAndBefore == 1 then
                        -- The before color was found so this will be the after color
                        -- by definition
                        currentColor :: currentAfterAndBefore

                    else if color == currentColor then
                        -- We found the before color
                        [ currentColor ]

                    else
                        currentAfterAndBefore
                )
                []
                cycle
    in
    case afterAndBefore of
        after :: _ :: _ ->
            after

        [ _ ] ->
            -- If only one was found probably the match was the last
            -- element so then the after is the end of the list as we folded
            -- from the right
            cycle |> List.reverse |> List.head |> Maybe.withDefault color

        _ ->
            -- Any other colors we just leave unchanged
            color
