module Tests.Algorithm exposing (algorithmFuzzer, appendTests, fromStringTests, inverseAlgTests, toStringTests, turnDirectionFuzzer, turnFuzzer, turnLengthFuzzer, turnableFuzzer)

{-| This represents an Algorithm, which is an ordered sequence of moves to be applied
to a cube. Enjoy!
-}

import Algorithm exposing (Algorithm)
import Expect
import Fuzz
import List.Nonempty
import Test exposing (..)


fromStringTests : Test
fromStringTests =
    describe "fromString"
        [ fuzz validAlgorithmStringTwoCharacterWideMoves "successfully parses valid algorithm strings with wide moves like Rw" <|
            Algorithm.fromString
                >> Expect.ok
        , fuzz2 fromStringValidAlgorithmFuzzer turnSeparator "a rendered algorithm is correctly retrieved no matter the separator with wide moves like Rw" <|
            \alg separator ->
                renderAlgorithmTwoCharacterWideMoves alg separator
                    |> Algorithm.fromString
                    |> Expect.equal (Ok alg)
        , fuzz validAlgorithmLowercaseWideMoves "successfully parses valid algorithm strings with wide moves like r" <|
            Algorithm.fromString
                >> Expect.ok
        , fuzz2 fromStringValidAlgorithmFuzzer turnSeparator "a rendered algorithm is correctly retrieved no matter the separator with wide moves like r" <|
            \alg separator ->
                renderAlgorithmLowercaseWideMoves alg separator
                    |> Algorithm.fromString
                    |> Expect.equal (Ok alg)
        , test "handles differing whitespace separation between turns" <|
            \_ ->
                Algorithm.fromString "U B  U\tB   U  \t B    \t    U"
                    |> Expect.equal
                        (Ok <|
                            Algorithm.fromTurnList <|
                                (List.repeat 4 (Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise)
                                    |> List.intersperse (Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.Clockwise)
                                )
                        )
        , test "handles leading and trailing whitespace" <|
            \_ ->
                Algorithm.fromString "\t U \t"
                    |> Expect.ok
        , test "Confidence check that a simple example maps to what we would expect" <|
            \_ ->
                Algorithm.fromString "RU2B'"
                    |> Expect.equal
                        (Ok <|
                            Algorithm.fromTurnList
                                [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
                                , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
                                , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.CounterClockwise
                                ]
                        )
        , test "handles parentheses" <|
            \_ ->
                Algorithm.fromString "(U) B (U B)"
                    |> Expect.equal
                        (Ok <|
                            Algorithm.fromTurnList
                                [ Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
                                , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.Clockwise
                                , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
                                , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.Clockwise
                                ]
                        )
        , fuzz obviouslyInvalidAlgorithmString "errors on invalid algorithms, but never crashes" <|
            Algorithm.fromString
                >> Expect.all
                    [ Expect.err
                    , \result ->
                        case result of
                            -- We just pass here as we are expecting an error above
                            Ok _ ->
                                Expect.pass

                            Err error ->
                                case error of
                                    Algorithm.UnexpectedError { debugInfo } ->
                                        Expect.fail
                                            ("UnexpectedError should never occur. The debug info was `"
                                                ++ debugInfo
                                                ++ "`"
                                            )

                                    _ ->
                                        Expect.pass
                    ]
        , test "errors on empty string as in the real world an algorithm always has turns" <|
            \_ ->
                Algorithm.fromString ""
                    |> Expect.equal (Err Algorithm.EmptyAlgorithm)
        , test "pure whitespace errors as EmptyAlgorithm" <|
            \_ ->
                Algorithm.fromString "  \t\t \t  \t \t  "
                    |> Expect.equal (Err Algorithm.EmptyAlgorithm)
        , test "An unexpected alphabetic character errors as invalid turnable" <|
            \_ ->
                Algorithm.fromString "UB' AFM2'"
                    |> Expect.equal
                        (Err <|
                            Algorithm.InvalidTurnable
                                { inputString = "UB' AFM2'"
                                , errorIndex = 4
                                , invalidTurnable = "A"
                                }
                        )
        , test "errors on 2 apostrophes in a row expecting a turnable" <|
            \_ ->
                Algorithm.fromString "B U''"
                    |> Expect.equal
                        (Err <|
                            Algorithm.InvalidTurnable
                                { inputString = "B U''"
                                , errorIndex = 4
                                , invalidTurnable = "'"
                                }
                        )
        , test "errors on whitespace between the turnable and the apostrophe" <|
            \_ ->
                Algorithm.fromString "B U  \t '"
                    |> Expect.equal
                        (Err <|
                            Algorithm.TurnWouldWorkWithoutInterruption
                                { inputString = "B U  \t '"
                                , interruptionStart = 3
                                , interruptionEnd = 7
                                }
                        )
        , test "errors on space between turnable and turn length" <|
            \_ ->
                Algorithm.fromString "B U \t \t 2"
                    |> Expect.equal
                        (Err <|
                            Algorithm.TurnWouldWorkWithoutInterruption
                                { inputString = "B U \t \t 2"
                                , interruptionStart = 3
                                , interruptionEnd = 8
                                }
                        )
        , test "errors on apostrophe before turn length" <|
            \_ ->
                Algorithm.fromString "B U'2"
                    |> Expect.equal
                        (Err <|
                            Algorithm.ApostropheWrongSideOfLength
                                { inputString = "B U'2"
                                , errorIndex = 3
                                }
                        )
        , test "errors on turn length specified twice" <|
            \_ ->
                Algorithm.fromString "B U22'"
                    |> Expect.equal
                        (Err <|
                            Algorithm.InvalidTurnLength
                                { inputString = "B U22'"
                                , errorIndex = 3
                                , invalidLength = "22"
                                }
                        )
        , test "errors on newline between turns" <|
            \_ ->
                Algorithm.fromString "U2'\nB"
                    |> Expect.equal
                        (Err <|
                            Algorithm.SpansOverSeveralLines "U2'\nB"
                        )
        , test "errors on leading newline" <|
            \_ ->
                Algorithm.fromString "\nU2'B"
                    |> Expect.equal
                        (Err <|
                            Algorithm.SpansOverSeveralLines "\nU2'B"
                        )
        , test "errors on trailing newline" <|
            \_ ->
                Algorithm.fromString "U2'B\n"
                    |> Expect.equal
                        (Err <|
                            Algorithm.SpansOverSeveralLines "U2'B\n"
                        )
        , test "errors on newline between turns windows style" <|
            \_ ->
                -- \u{000D} is the carriage return character \r but elm
                -- format forces it to this style. See
                -- https://github.com/avh4/elm-format/issues/376
                Algorithm.fromString "U2'\u{000D}\nB"
                    |> Expect.equal
                        (Err <|
                            Algorithm.SpansOverSeveralLines "U2'\u{000D}\nB"
                        )
        , test "errors on leading newline windows style" <|
            \_ ->
                Algorithm.fromString "\u{000D}\nU2'B"
                    |> Expect.equal
                        (Err <|
                            Algorithm.SpansOverSeveralLines "\u{000D}\nU2'B"
                        )
        , test "errors on trailing newline windows style" <|
            \_ ->
                Algorithm.fromString "U2'B\u{000D}\n"
                    |> Expect.equal
                        (Err <|
                            Algorithm.SpansOverSeveralLines "U2'B\u{000D}\n"
                        )
        , test "errors on turn length 4" <|
            \_ ->
                Algorithm.fromString "B U4"
                    |> Expect.equal
                        (Err <|
                            Algorithm.InvalidTurnLength
                                { inputString = "B U4"
                                , errorIndex = 3
                                , invalidLength = "4"
                                }
                        )
        , test "errors on turn length 1" <|
            \_ ->
                Algorithm.fromString "B'F3 U1"
                    |> Expect.equal
                        (Err <|
                            Algorithm.InvalidTurnLength
                                { inputString = "B'F3 U1"
                                , errorIndex = 6
                                , invalidLength = "1"
                                }
                        )
        , test "The same turnable specified twice in a row errors" <|
            \_ ->
                Algorithm.fromString "U2' U'"
                    |> Expect.equal
                        (Err <|
                            Algorithm.RepeatedTurnable
                                { inputString = "U2' U'"
                                , errorIndex = 4
                                }
                        )
        , test "Never closing a set of parentheses errors" <|
            \_ ->
                Algorithm.fromString "B ( U2B'"
                    |> Expect.equal
                        (Err <|
                            Algorithm.UnclosedParenthesis
                                { inputString = "B ( U2B'"
                                , openParenthesisIndex = 2
                                }
                        )
        , test "Errors on an unmatched closing parenthesis" <|
            \_ ->
                Algorithm.fromString "U2 B ) F2"
                    |> Expect.equal
                        (Err <|
                            Algorithm.UnmatchedClosingParenthesis
                                { inputString = "U2 B ) F2"
                                , errorIndex = 5
                                }
                        )
        , test "Errors on starting algorithm with closing parenthesis" <|
            \_ ->
                Algorithm.fromString ")U2B"
                    |> Expect.equal
                        (Err <|
                            Algorithm.UnmatchedClosingParenthesis
                                { inputString = ")U2B"
                                , errorIndex = 0
                                }
                        )
        , test "Errors on empty parenthesis set" <|
            \_ ->
                Algorithm.fromString "U2 () B"
                    |> Expect.equal
                        (Err <|
                            Algorithm.EmptyParentheses
                                { inputString = "U2 () B"
                                , errorIndex = 3
                                }
                        )
        , test "Errors on empty parenthesis set at start of string" <|
            \_ ->
                Algorithm.fromString "() U2 B"
                    |> Expect.equal
                        (Err <|
                            Algorithm.EmptyParentheses
                                { inputString = "() U2 B"
                                , errorIndex = 0
                                }
                        )
        , test "Errors on nested sets of parentheses" <|
            \_ ->
                Algorithm.fromString "(U2 (B R) B)"
                    |> Expect.equal
                        (Err <|
                            Algorithm.NestedParentheses
                                { inputString = "(U2 (B R) B)"
                                , errorIndex = 4
                                }
                        )
        , test "Errors as expected on opening parenthesis breaking up a turn" <|
            \_ ->
                Algorithm.fromString "B U(2')"
                    |> Expect.equal
                        (Err <|
                            Algorithm.TurnWouldWorkWithoutInterruption
                                { inputString = "B U(2')"
                                , interruptionStart = 3
                                , interruptionEnd = 4
                                }
                        )
        , test "Errors as expected on closing parenthesis breaking up a turn" <|
            \_ ->
                Algorithm.fromString "B (U)2"
                    |> Expect.equal
                        (Err <|
                            Algorithm.TurnWouldWorkWithoutInterruption
                                { inputString = "B (U)2"
                                , interruptionStart = 4
                                , interruptionEnd = 5
                                }
                        )
        , test "Errors as expected on incomplete nested parentheses with open" <|
            \_ ->
                Algorithm.fromString "(U2 (B R B)"
                    |> Expect.equal
                        (Err <|
                            Algorithm.NestedParentheses
                                { inputString = "(U2 (B R B)"
                                , errorIndex = 4
                                }
                        )
        , test "Errors as expected on incomplete nested parentheses with close" <|
            \_ ->
                Algorithm.fromString "(U2 B) R B)"
                    |> Expect.equal
                        (Err <|
                            Algorithm.UnmatchedClosingParenthesis
                                { inputString = "(U2 B) R B)"
                                , errorIndex = 10
                                }
                        )
        , fuzz
            (Fuzz.oneOf
                (List.map
                    Fuzz.constant
                    [ '%'
                    , '+'
                    , '-'
                    , '~'
                    , '&'
                    , '*'
                    ]
                )
            )
            "Errors informatively when encountering unexpected symbol"
          <|
            \invalidSymbol ->
                let
                    inputString =
                        "U" ++ String.fromChar invalidSymbol ++ " B2"
                in
                Algorithm.fromString inputString
                    |> Expect.equal
                        (Err <|
                            Algorithm.InvalidSymbol
                                { inputString = inputString
                                , errorIndex = 1
                                , symbol = invalidSymbol
                                }
                        )
        , test "Errors when mixing wide move styles two characters before lowercase" <|
            \_ ->
                Algorithm.fromString "B Rw u"
                    |> Expect.equal
                        (Err <|
                            Algorithm.WideMoveStylesMixed
                                { inputString = "B Rw u"
                                , errorIndex = 5
                                , invalidWideMove = "u"
                                }
                        )
        , test "Errors when mixing wide move styles lowercase before two characters" <|
            \_ ->
                Algorithm.fromString "B r Uw"
                    |> Expect.equal
                        (Err <|
                            Algorithm.WideMoveStylesMixed
                                { inputString = "B r Uw"
                                , errorIndex = 4
                                , invalidWideMove = "Uw"
                                }
                        )
        , test "lowercase wide move before unmatched closing parenthesis regression" <|
            \_ ->
                Algorithm.fromString "U2 r B' ) F2"
                    |> Expect.equal
                        (Err <|
                            Algorithm.UnmatchedClosingParenthesis
                                { inputString = "U2 r B' ) F2"
                                , errorIndex = 8
                                }
                        )
        , test "two character wide move before unmatched closing parenthesis regression" <|
            \_ ->
                Algorithm.fromString "U2 Rw B' ) F2"
                    |> Expect.equal
                        (Err <|
                            Algorithm.UnmatchedClosingParenthesis
                                { inputString = "U2 Rw B' ) F2"
                                , errorIndex = 9
                                }
                        )
        ]


toStringTests : Test
toStringTests =
    describe "toString"
        [ fuzz fromStringValidAlgorithmFuzzer "stringified algorithm decodes back to original value" <|
            \algorithm ->
                algorithm
                    |> Algorithm.toString
                    |> Algorithm.fromString
                    |> Expect.equal (Ok algorithm)
        , test "Passes specific case that tries covering all types of turnables, lengths and directions" <|
            \_ ->
                Algorithm.fromTurnList
                    [ Algorithm.Turn
                        Algorithm.U
                        Algorithm.OneQuarter
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.D
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.F
                        Algorithm.Halfway
                        Algorithm.CounterClockwise
                    , Algorithm.Turn
                        Algorithm.B
                        Algorithm.OneQuarter
                        Algorithm.CounterClockwise
                    , Algorithm.Turn
                        Algorithm.R
                        Algorithm.ThreeQuarters
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.L
                        Algorithm.ThreeQuarters
                        Algorithm.CounterClockwise
                    , Algorithm.Turn
                        Algorithm.Uw
                        Algorithm.OneQuarter
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.Dw
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.Fw
                        Algorithm.Halfway
                        Algorithm.CounterClockwise
                    , Algorithm.Turn
                        Algorithm.Bw
                        Algorithm.OneQuarter
                        Algorithm.CounterClockwise
                    , Algorithm.Turn
                        Algorithm.Rw
                        Algorithm.ThreeQuarters
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.Lw
                        Algorithm.ThreeQuarters
                        Algorithm.CounterClockwise
                    , Algorithm.Turn
                        Algorithm.M
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.S
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.E
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.X
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.Y
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.Z
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    ]
                    |> Algorithm.toString
                    |> Expect.equal "U D2 F2' B' R3 L3' Uw Dw2 Fw2' Bw' Rw3 Lw3' M2 S2 E2 x2 y2 z2"
        , test "handles empty algorithm" <|
            \_ ->
                Algorithm.empty
                    |> Algorithm.toString
                    |> Expect.equal ""
        ]


inverseAlgTests : Test
inverseAlgTests =
    describe "inverseAlg"
        [ fuzz algorithmFuzzer "the inverse of the inverse should be the original algorithm" <|
            \alg ->
                alg
                    |> Algorithm.inverse
                    |> Algorithm.inverse
                    |> Expect.equal alg
        , fuzz2 algorithmFuzzer algorithmFuzzer "the inverse of an algorithm equals splitting the alg in two, inversing each part and swapping their order" <|
            \part1 part2 ->
                Algorithm.append (Algorithm.inverse part2) (Algorithm.inverse part1)
                    |> Expect.equal (Algorithm.inverse (Algorithm.append part1 part2))
        , test "correctly inverses simple example" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList
                            [ Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
                            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
                            ]

                    inversedAlg =
                        Algorithm.fromTurnList
                            [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
                            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
                            ]
                in
                alg |> Algorithm.inverse |> Expect.equal inversedAlg
        ]


appendTests : Test
appendTests =
    describe "appenders"
        [ describe "append"
            [ fuzz2 turnFuzzer turnFuzzer "Appending two algorithms each consisting of a turn equals an algorithm with those two turns in a row" <|
                \turn1 turn2 ->
                    Algorithm.append (Algorithm.fromTurnList [ turn1 ]) (Algorithm.fromTurnList [ turn2 ])
                        |> Expect.equal (Algorithm.fromTurnList [ turn1, turn2 ])
            , fuzz algorithmFuzzer "Appending to an empty algorithm equals the second algorithm" <|
                \algorithm ->
                    Algorithm.append Algorithm.empty algorithm
                        |> Expect.equal algorithm
            , fuzz algorithmFuzzer "Appending an empty algorithm to an algorithm equals the first algorithm" <|
                \algorithm ->
                    Algorithm.append algorithm Algorithm.empty
                        |> Expect.equal algorithm
            , test "Appending two empty algorithm equals an empty algorithm" <|
                \_ ->
                    Algorithm.append Algorithm.empty Algorithm.empty
                        |> Expect.equal Algorithm.empty
            ]
        , describe "reverseAppend"
            [ fuzz2 algorithmFuzzer algorithmFuzzer "is the opposite of append" <|
                \alg1 alg2 ->
                    Algorithm.reverseAppend alg1 alg2
                        |> Expect.equal (Algorithm.append alg2 alg1)
            , fuzz algorithmFuzzer "Appending an empty algorithm equals the second algorithm" <|
                \algorithm ->
                    Algorithm.reverseAppend Algorithm.empty algorithm
                        |> Expect.equal algorithm
            , fuzz algorithmFuzzer "Appending an algorithm to an empty algorithm equals the first algorithm" <|
                \algorithm ->
                    Algorithm.reverseAppend algorithm Algorithm.empty
                        |> Expect.equal algorithm
            , test "Appending two empty algorithm equals an empty algorithm" <|
                \_ ->
                    Algorithm.reverseAppend Algorithm.empty Algorithm.empty
                        |> Expect.equal Algorithm.empty
            ]
        ]


obviouslyInvalidAlgorithmString : Fuzz.Fuzzer String
obviouslyInvalidAlgorithmString =
    let
        notFaceOrSlice c =
            not <|
                List.Nonempty.member c
                    (List.Nonempty.append
                        (List.Nonempty.map
                            (renderTurnableTwoCharacterWideMoves >> String.uncons >> Maybe.map Tuple.first >> Maybe.withDefault 'c')
                            Algorithm.allTurnables
                        )
                        (List.Nonempty.map
                            (renderTurnableLowercaseWideMoves >> String.uncons >> Maybe.map Tuple.first >> Maybe.withDefault 'c')
                            Algorithm.allTurnables
                        )
                    )

        removeFaceAndSliceChars =
            String.filter notFaceOrSlice
    in
    Fuzz.map removeFaceAndSliceChars Fuzz.string


validAlgorithmStringTwoCharacterWideMoves : Fuzz.Fuzzer String
validAlgorithmStringTwoCharacterWideMoves =
    Fuzz.map2 renderAlgorithmTwoCharacterWideMoves fromStringValidAlgorithmFuzzer turnSeparator


validAlgorithmLowercaseWideMoves : Fuzz.Fuzzer String
validAlgorithmLowercaseWideMoves =
    Fuzz.map2 renderAlgorithmLowercaseWideMoves fromStringValidAlgorithmFuzzer turnSeparator


fromStringValidAlgorithmFuzzer : Fuzz.Fuzzer Algorithm
fromStringValidAlgorithmFuzzer =
    -- We remove any neighbouring duplicate
    -- turnables as those are invalid for fromString
    Fuzz.map
        (\algorithm ->
            Algorithm.toTurnList algorithm
                |> List.foldl
                    (\((Algorithm.Turn nextTurnable _ _) as turn) turnList ->
                        case turnList of
                            [] ->
                                [ turn ]

                            (Algorithm.Turn prevTurnable _ _) :: _ ->
                                if nextTurnable == prevTurnable then
                                    turnList

                                else
                                    turn :: turnList
                    )
                    []
                |> List.reverse
                |> Algorithm.fromTurnList
        )
        algorithmFuzzer


algorithmFuzzer : Fuzz.Fuzzer Algorithm
algorithmFuzzer =
    let
        nonEmptyTurnList =
            Fuzz.map2 (::) turnFuzzer <| Fuzz.list turnFuzzer
    in
    Fuzz.map Algorithm.fromTurnList nonEmptyTurnList


renderAlgorithmTwoCharacterWideMoves : Algorithm -> String -> String
renderAlgorithmTwoCharacterWideMoves alg separator =
    let
        renderedTurnList =
            Algorithm.toTurnList >> List.map renderTurnTwoCharacterWideMoves <| alg
    in
    String.join separator renderedTurnList


renderAlgorithmLowercaseWideMoves : Algorithm -> String -> String
renderAlgorithmLowercaseWideMoves alg separator =
    let
        renderedTurnList =
            Algorithm.toTurnList >> List.map renderTurnLowercaseWideMoves <| alg
    in
    String.join separator renderedTurnList


turnSeparator : Fuzz.Fuzzer String
turnSeparator =
    Fuzz.oneOf
        [ Fuzz.constant ""
        , Fuzz.constant " "
        , Fuzz.constant "  "
        , Fuzz.constant "   "
        , Fuzz.constant "\t"
        ]


turnFuzzer : Fuzz.Fuzzer Algorithm.Turn
turnFuzzer =
    Fuzz.map3 Algorithm.Turn turnableFuzzer turnLengthFuzzer turnDirectionFuzzer


renderTurnTwoCharacterWideMoves : Algorithm.Turn -> String
renderTurnTwoCharacterWideMoves (Algorithm.Turn x length direction) =
    -- For double/triple turns clockwise we format it as U2' / U3' as these are used in some
    -- algorithms for explanations of fingertricks, also notice it's not U'2 or U'3. This
    -- decision was made based on "use in the wild" specifically the Youtuber Jperm's use.
    renderTurnableTwoCharacterWideMoves x ++ renderLength length ++ renderDirection direction


renderTurnLowercaseWideMoves : Algorithm.Turn -> String
renderTurnLowercaseWideMoves (Algorithm.Turn x length direction) =
    -- For double/triple turns clockwise we format it as U2' / U3' as these are used in some
    -- algorithms for explanations of fingertricks, also notice it's not U'2 or U'3. This
    -- decision was made based on "use in the wild" specifically the Youtuber Jperm's use.
    renderTurnableLowercaseWideMoves x ++ renderLength length ++ renderDirection direction


turnableFuzzer : Fuzz.Fuzzer Algorithm.Turnable
turnableFuzzer =
    Algorithm.allTurnables
        |> List.Nonempty.map Fuzz.constant
        |> List.Nonempty.toList
        |> Fuzz.oneOf


renderTurnableTwoCharacterWideMoves : Algorithm.Turnable -> String
renderTurnableTwoCharacterWideMoves x =
    case x of
        Algorithm.U ->
            "U"

        Algorithm.D ->
            "D"

        Algorithm.L ->
            "L"

        Algorithm.R ->
            "R"

        Algorithm.F ->
            "F"

        Algorithm.B ->
            "B"

        Algorithm.M ->
            "M"

        Algorithm.S ->
            "S"

        Algorithm.E ->
            "E"

        Algorithm.Uw ->
            "Uw"

        Algorithm.Dw ->
            "Dw"

        Algorithm.Rw ->
            "Rw"

        Algorithm.Lw ->
            "Lw"

        Algorithm.Fw ->
            "Fw"

        Algorithm.Bw ->
            "Bw"

        Algorithm.X ->
            "x"

        Algorithm.Y ->
            "y"

        Algorithm.Z ->
            "z"


renderTurnableLowercaseWideMoves : Algorithm.Turnable -> String
renderTurnableLowercaseWideMoves x =
    case x of
        Algorithm.U ->
            "U"

        Algorithm.D ->
            "D"

        Algorithm.L ->
            "L"

        Algorithm.R ->
            "R"

        Algorithm.F ->
            "F"

        Algorithm.B ->
            "B"

        Algorithm.M ->
            "M"

        Algorithm.S ->
            "S"

        Algorithm.E ->
            "E"

        Algorithm.Uw ->
            "u"

        Algorithm.Dw ->
            "d"

        Algorithm.Rw ->
            "r"

        Algorithm.Lw ->
            "l"

        Algorithm.Fw ->
            "f"

        Algorithm.Bw ->
            "b"

        Algorithm.X ->
            "x"

        Algorithm.Y ->
            "y"

        Algorithm.Z ->
            "z"


turnLengthFuzzer : Fuzz.Fuzzer Algorithm.TurnLength
turnLengthFuzzer =
    Algorithm.allTurnLengths
        |> List.Nonempty.map Fuzz.constant
        |> List.Nonempty.toList
        |> Fuzz.oneOf


renderLength : Algorithm.TurnLength -> String
renderLength length =
    case length of
        Algorithm.OneQuarter ->
            ""

        Algorithm.Halfway ->
            "2"

        Algorithm.ThreeQuarters ->
            "3"


turnDirectionFuzzer : Fuzz.Fuzzer Algorithm.TurnDirection
turnDirectionFuzzer =
    Algorithm.allTurnDirections
        |> List.Nonempty.map Fuzz.constant
        |> List.Nonempty.toList
        |> Fuzz.oneOf


renderDirection : Algorithm.TurnDirection -> String
renderDirection dir =
    case dir of
        Algorithm.Clockwise ->
            ""

        Algorithm.CounterClockwise ->
            "'"
