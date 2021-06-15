module Algorithm exposing (Algorithm, Turn(..), TurnDirection(..), TurnLength(..), Turnable(..), allTurnDirections, allTurnLengths, allTurnables, allTurns, append, appendTo, build, empty, toTurnList, fromString, inverse, toString, FromStringError(..))

{-| Documentation to come

@docs Algorithm, Turn, TurnDirection, TurnLength, Turnable, allTurnDirections, allTurnLengths, allTurnables, allTurns, append, appendTo, build, empty, toTurnList, fromString, inverse, toString, FromStringError

-}

import Monads.ListM as ListM
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Set
import Utils.Enumerator



-- ALGORITHM MODEL


{-| Placeholder
-}
type Algorithm
    = Algorithm (List Turn)


{-| Placeholder
-}
type Turn
    = Turn Turnable TurnLength TurnDirection


{-| Placeholder
-}
type Turnable
    = -- Single face turns
      U
    | D
    | L
    | R
    | F
    | B
      -- Slice turns
    | M
    | S
    | E
      -- Whole cube rotations (lowercase type constructors not allowed in Elm)
    | X
    | Y
    | Z


{-| Placeholder
-}
type TurnLength
    = OneQuarter
    | Halfway
    | ThreeQuarters


{-| Placeholder
-}
type TurnDirection
    = Clockwise
    | CounterClockwise



-- HELPERS


{-| Placeholder
-}
toTurnList : Algorithm -> List Turn
toTurnList alg =
    case alg of
        Algorithm turnList ->
            turnList


{-| Placeholder
-}
build : List Turn -> Algorithm
build =
    Algorithm


{-| Placeholder
-}
empty : Algorithm
empty =
    Algorithm []


{-| We append to the first argument, so a ++ b
-}
appendTo : Algorithm -> Algorithm -> Algorithm
appendTo (Algorithm a) (Algorithm b) =
    Algorithm (a ++ b)


{-| We append the first argument, so b ++ a
-}
append : Algorithm -> Algorithm -> Algorithm
append (Algorithm a) (Algorithm b) =
    Algorithm (b ++ a)


{-| Placeholder
-}
inverse : Algorithm -> Algorithm
inverse =
    let
        map f (Algorithm turnList) =
            Algorithm (f turnList)

        flipDirection direction =
            case direction of
                Clockwise ->
                    CounterClockwise

                CounterClockwise ->
                    Clockwise

        flipTurn (Turn a b direction) =
            Turn a b (flipDirection direction)
    in
    map <| List.reverse >> List.map flipTurn


{-| Placeholder
-}
toString : Algorithm -> String
toString (Algorithm turnList) =
    String.join " " <| List.map turnToString turnList


turnToString : Turn -> String
turnToString (Turn turnable length direction) =
    -- For double/triple turns clockwise we format it as U2' / U3'
    -- as these are used in some algorithms for explanations of
    -- fingertricks, also notice it's not U'2 or U'3. This decision
    -- was made based on "use in the wild" specifically the
    -- Youtuber J Perm's use.
    turnableToString turnable
        ++ turnLengthToString length
        ++ turnDirectionToString direction


turnableToString : Turnable -> String
turnableToString x =
    case x of
        U ->
            "U"

        D ->
            "D"

        L ->
            "L"

        R ->
            "R"

        F ->
            "F"

        B ->
            "B"

        M ->
            "M"

        S ->
            "S"

        E ->
            "E"

        X ->
            "x"

        Y ->
            "y"

        Z ->
            "z"


turnLengthToString : TurnLength -> String
turnLengthToString length =
    case length of
        OneQuarter ->
            ""

        Halfway ->
            "2"

        ThreeQuarters ->
            "3"


turnDirectionToString : TurnDirection -> String
turnDirectionToString dir =
    case dir of
        Clockwise ->
            ""

        CounterClockwise ->
            "'"


{-| Placeholder
-}
type FromStringError
    = EmptyAlgorithm
    | InvalidTurnable
        { inputString : String
        , errorIndex : Int
        , invalidTurnable : String
        }
    | InvalidTurnLength
        { inputString : String
        , errorIndex : Int
        , invalidLength : String
        }
    | RepeatedTurnable
        { inputString : String
        , errorIndex : Int
        }
    | TurnWouldWorkWithoutInterruption
        { inputString : String
        , interruptionStart : Int
        , interruptionEnd : Int
        }
    | InvalidTurnApostropheWrongSideOfLength
        { inputString : String
        , errorIndex : Int
        }
    | UnclosedParentheses
        { inputString : String
        , openParenthesisIndex : Int
        }
    | UnmatchedClosingParenthesis
        { inputString : String
        , errorIndex : Int
        }
    | EmptyParentheses
        { inputString : String
        , errorIndex : Int
        }
    | NestedParentheses
        { inputString : String
        , errorIndex : Int
        }
    | SpansOverSeveralLines String
    | InvalidSymbol
        { inputString : String
        , errorIndex : Int
        , symbol : Char
        }
    | UnexpectedError
        { inputString : String
        , errorIndex : Int
        , debugInfo : String
        }


{-| Placeholder
-}
fromString : String -> Result FromStringError Algorithm
fromString string =
    Parser.run algorithmParser string
        |> Result.mapError (parserErrorToFromStringError string)


parserErrorToFromStringError :
    String
    -> List (Parser.DeadEnd Never ParsingProblem)
    -> FromStringError
parserErrorToFromStringError string deadends =
    let
        deadendsInfo =
            parseDeadEnds deadends string
    in
    case deadendsInfo of
        Err error ->
            error

        Ok { problem, col, unexpectedString } ->
            problemToFromStringError
                { problem = problem
                , index = col - 1
                , inputString = string
                , unexpectedString = unexpectedString
                }


parseDeadEnds :
    List (Parser.DeadEnd Never ParsingProblem)
    -> String
    ->
        Result
            FromStringError
            { problem : ParsingProblem
            , col : Int
            , unexpectedString : String
            }
parseDeadEnds deadends inputString =
    let
        hasWillNeverOccur =
            List.member WillNeverOccur (List.map .problem deadends)

        erroredOnNonFirstRow =
            not <|
                List.isEmpty <|
                    (List.filter (.row >> (/=) 1) deadends
                        |> List.map .problem
                        |> List.filter
                            (\problem ->
                                case problem of
                                    -- We are okay with SpansOverSeveralLines erroring on
                                    UserReadyError (SpansOverSeveralLines _) ->
                                        False

                                    _ ->
                                        True
                            )
                    )

        uniqueColValues =
            List.map .col deadends
                -- Filter same values
                |> Set.fromList
                |> Set.toList

        colResult =
            case uniqueColValues of
                [ x ] ->
                    Ok x

                [] ->
                    Err "No col values found in deadends"

                _ ->
                    Err
                        ("Found several different col values in"
                            ++ " deadends which we didn't expect"
                        )

        relevantProblemResult =
            Result.map Tuple.first <|
                case List.filterMap getRelevantProblem deadends of
                    [] ->
                        Err "No relevant problems were found in deadends"

                    x :: xs ->
                        List.foldl
                            (\problem result ->
                                case result of
                                    Err err ->
                                        Err err

                                    Ok y ->
                                        if Tuple.first y == Tuple.first problem then
                                            Ok y

                                        else if Tuple.second y > Tuple.second problem then
                                            Ok y

                                        else if Tuple.second y < Tuple.second problem then
                                            Ok problem

                                        else
                                            Err
                                                ("Several different relevant problems with same"
                                                    ++ " importance level encountered in deadends unexpectedly"
                                                )
                            )
                            (Ok x)
                            xs
    in
    Result.mapError
        (\debugInfo ->
            UnexpectedError
                { inputString = inputString
                , errorIndex = Result.withDefault 0 colResult
                , debugInfo = debugInfo
                }
        )
    <|
        if hasWillNeverOccur then
            Err
                "An error we though would never occur occurred unexpectedly"

        else if erroredOnNonFirstRow then
            Err
                ("An error occurred on a non first row unexpectedly when there"
                    ++ " should only ever be one row parsed"
                )

        else
            Result.map2 Tuple.pair
                colResult
                relevantProblemResult
                |> Result.andThen
                    (\( col, relevantProblem ) ->
                        let
                            unexpectedString =
                                String.slice (col - 1) col inputString
                        in
                        if
                            unexpectedString
                                == ""
                                && unexpectedStringExpected relevantProblem
                        then
                            Err
                                ("dead end parser finished without"
                                    ++ " finding an unexpected string"
                                )

                        else
                            Ok
                                { col = col
                                , problem = relevantProblem
                                , unexpectedString = unexpectedString
                                }
                    )


unexpectedStringExpected : ParsingProblem -> Bool
unexpectedStringExpected problem =
    case problem of
        UserReadyError _ ->
            False

        ExpectingTurnable _ ->
            True

        ExpectingUnwantedString _ ->
            True

        ExpectingClosingParenthesis ->
            True

        ExpectingOpeningParenthesis ->
            True

        WillNeverOccur ->
            True

        ExpectingEnd ->
            True


getRelevantProblem : Parser.DeadEnd Never ParsingProblem -> Maybe ( ParsingProblem, Int )
getRelevantProblem { problem } =
    case problem of
        UserReadyError _ ->
            Just ( problem, 3 )

        ExpectingTurnable _ ->
            Just ( problem, 1 )

        ExpectingOpeningParenthesis ->
            Nothing

        ExpectingUnwantedString _ ->
            Nothing

        ExpectingClosingParenthesis ->
            Nothing

        ExpectingEnd ->
            Nothing

        WillNeverOccur ->
            Nothing


problemToFromStringError :
    { inputString : String
    , problem : ParsingProblem
    , index : Int
    , unexpectedString : String
    }
    -> FromStringError
problemToFromStringError { inputString, problem, index, unexpectedString } =
    case problem of
        UserReadyError userProblem ->
            userProblem

        ExpectingTurnable Nothing ->
            if unexpectedString == "(" then
                NestedParentheses
                    { inputString = inputString
                    , errorIndex = index
                    }

            else if Tuple.first <| wasUnexpectedCharacter unexpectedString then
                InvalidSymbol
                    { inputString = inputString
                    , errorIndex = index
                    , symbol =
                        Tuple.second <|
                            wasUnexpectedCharacter unexpectedString
                    }

            else
                InvalidTurnable
                    { inputString = inputString
                    , errorIndex = index
                    , invalidTurnable = String.slice index (index + 1) inputString
                    }

        ExpectingTurnable (Just { previousTurnString, previousTurnstartIndex }) ->
            if
                turnWouldHaveWorkedWithoutWhitespace
                    { unexpectedString = unexpectedString
                    , previousTurnString = previousTurnString
                    }
            then
                TurnWouldWorkWithoutInterruption
                    { inputString = inputString
                    , interruptionStart = previousTurnstartIndex + String.length previousTurnString
                    , interruptionEnd = index
                    }

            else if
                turnWouldHaveWorkedWithLengthAndDirectionSwapped
                    { unexpectedString = unexpectedString
                    , previousTurnString = previousTurnString
                    }
            then
                InvalidTurnApostropheWrongSideOfLength
                    { inputString = inputString
                    , errorIndex = index - 1
                    }

            else if unexpectedString == "(" then
                NestedParentheses
                    { inputString = inputString
                    , errorIndex = index
                    }

            else if
                wasInvalidTurnLength
                    { unexpectedString = unexpectedString
                    , previousTurnString = previousTurnString
                    }
            then
                InvalidTurnLength
                    { inputString = inputString
                    , errorIndex = index
                    , invalidLength = unexpectedString
                    }

            else if Tuple.first <| wasUnexpectedCharacter unexpectedString then
                InvalidSymbol
                    { inputString = inputString
                    , errorIndex = index
                    , symbol =
                        Tuple.second <|
                            wasUnexpectedCharacter unexpectedString
                    }

            else
                InvalidTurnable
                    { inputString = inputString
                    , errorIndex = index
                    , invalidTurnable = String.slice index (index + 1) inputString
                    }

        ExpectingEnd ->
            UnexpectedError
                { inputString = inputString
                , errorIndex = index
                , debugInfo = "We Expected UnexpectedEnd Problems To Have Been Filtered Out"
                }

        ExpectingClosingParenthesis ->
            UnexpectedError
                { inputString = inputString
                , errorIndex = index
                , debugInfo = "We expected ExpectingClosingParenthesis to be filtered it"
                }

        ExpectingOpeningParenthesis ->
            UnexpectedError
                { inputString = inputString
                , errorIndex = index
                , debugInfo = "We expected ExpectingOpeningParenthesis to be filtered it"
                }

        ExpectingUnwantedString _ ->
            UnexpectedError
                { inputString = inputString
                , errorIndex = index
                , debugInfo = "We expected ExpectingUnwantedString to be filtered it"
                }

        WillNeverOccur ->
            UnexpectedError
                { inputString = inputString
                , errorIndex = index
                , debugInfo = "A problem we never expected to happen happened anyway"
                }


turnWouldHaveWorkedWithoutWhitespace :
    { previousTurnString : String
    , unexpectedString : String
    }
    -> Bool
turnWouldHaveWorkedWithoutWhitespace { previousTurnString, unexpectedString } =
    resultToBool (Parser.run algorithmParser (previousTurnString ++ unexpectedString))


turnWouldHaveWorkedWithLengthAndDirectionSwapped :
    { previousTurnString : String
    , unexpectedString : String
    }
    -> Bool
turnWouldHaveWorkedWithLengthAndDirectionSwapped { previousTurnString, unexpectedString } =
    String.right 1 previousTurnString
        == "'"
        && resultToBool
            (Parser.run algorithmParser
                (String.dropRight 1 previousTurnString
                    ++ unexpectedString
                    ++ String.right 1 previousTurnString
                )
            )


wasInvalidTurnLength :
    { previousTurnString : String
    , unexpectedString : String
    }
    -> Bool
wasInvalidTurnLength { previousTurnString, unexpectedString } =
    -- Check that a turn length was attempted to be specified by
    -- checking if the unexpected string was an integer
    maybeToBool
        (String.toInt unexpectedString)
        -- If this was so we attempt to see if a valid turn length
        -- added to what was parsed as the previous turn succeeds.
        -- If it succeeds we know it was an invalid turn length
        && resultToBool
            (Parser.run algorithmParser (previousTurnString ++ "2"))


wasUnexpectedCharacter : String -> ( Bool, Char )
wasUnexpectedCharacter string =
    case String.uncons string of
        Nothing ->
            ( False, ' ' )

        Just ( char, _ ) ->
            if
                Char.isAlphaNum char
                    || char
                    == '('
                    || char
                    == ')'
                    || char
                    == '\''
                    || resultToBool
                        (Parser.run algorithmParser (String.fromChar char))
            then
                ( False, ' ' )

            else
                ( True, char )


maybeToBool : Maybe a -> Bool
maybeToBool =
    Maybe.map (always True) >> Maybe.withDefault False


resultToBool : Result a b -> Bool
resultToBool =
    Result.map (always True) >> Result.withDefault False



-- PARSER


type alias OurParser result =
    Parser Never ParsingProblem result


type ParsingProblem
    = UserReadyError FromStringError
    | ExpectingTurnable
        (Maybe
            { previousTurnString : String
            , previousTurnstartIndex : Int
            }
        )
    | ExpectingOpeningParenthesis
    | ExpectingClosingParenthesis
    | ExpectingEnd
    | ExpectingUnwantedString String
    | WillNeverOccur


type alias ParserState =
    { turnList :
        List
            { turn : Turn
            , string : String
            , startIndex : Int
            }
    , maybeStartParenthesisIndex : Maybe ParenthesisIndex
    }


{-| A type safety helper for passing it around to functions
-}
type ParenthesisIndex
    = ParenthesisIndex Int


toInt : ParenthesisIndex -> Int
toInt (ParenthesisIndex int) =
    int


{-| Serves as a bit of a configuration for what whitespace
we accept. This implementation of space and tab for example
only allows algorithms written on a single line
-}
isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t'


algorithmParser : OurParser Algorithm
algorithmParser =
    Parser.succeed Algorithm
        |= Parser.loop
            { turnList = []
            , maybeStartParenthesisIndex = Nothing
            }
            buildTurnListLoop
        |> Parser.andThen verifyNotEmptyParser


buildTurnListLoop :
    ParserState
    ->
        OurParser
            (Parser.Step
                ParserState
                (List Turn)
            )
buildTurnListLoop state =
    Parser.succeed identity
        -- Deal with any whitespace between turns or parentheses
        |. whitespaceParser
        |. Parser.oneOf
            [ Parser.token (Parser.Token "\n" (ExpectingUnwantedString "\n"))
                |> andThenWithInputStringAndOffset
                    (\( _, inputString, _ ) ->
                        Parser.problem <|
                            UserReadyError (SpansOverSeveralLines inputString)
                    )

            -- \u{000D} is the carriage return character \r but elm format forces it to
            -- this style. see https://github.com/avh4/elm-format/issues/376
            , Parser.token (Parser.Token "\u{000D}" (ExpectingUnwantedString "\u{000D}"))
                |> andThenWithInputStringAndOffset
                    (\( _, inputString, _ ) ->
                        Parser.problem <|
                            UserReadyError (SpansOverSeveralLines inputString)
                    )

            -- It will never fail to find an empty string
            , Parser.token (Parser.Token "" WillNeverOccur)
            ]
        |= (case state.maybeStartParenthesisIndex of
                -- We're not currently inside a set of parentheses
                Nothing ->
                    Parser.oneOf
                        [ -- Handle start of parentheses and save start index
                          Parser.succeed
                            (\index ->
                                Parser.Loop { state | maybeStartParenthesisIndex = (Just << ParenthesisIndex) index }
                            )
                            |= Parser.getOffset
                            |. Parser.symbol (Parser.Token "(" ExpectingOpeningParenthesis)

                        -- If no parenthesis try parsing a turn
                        , parseTurnLoop state

                        -- If there is no turn check if we're at the end
                        , Parser.end ExpectingEnd
                            |> Parser.map
                                -- We reverse here because the list is built with :: in reverse
                                (\_ -> Parser.Done (state.turnList |> List.map .turn |> List.reverse))

                        -- Error appropriately if we encounter a closing parenthesis when we're not
                        -- inside a set of parentheses
                        , Parser.token (Parser.Token ")" (ExpectingUnwantedString ")"))
                            |> andThenWithInputStringAndOffset
                                (\( _, inputString, offset ) ->
                                    Parser.problem <|
                                        UserReadyError
                                            (UnmatchedClosingParenthesis
                                                { inputString = inputString
                                                , errorIndex = offset - 1
                                                }
                                            )
                                )
                        ]

                -- We're currently inside a set of parentheses
                Just startParenthesisIndex ->
                    Parser.oneOf
                        [ -- Exiting the set of parentheses
                          Parser.symbol (Parser.Token ")" ExpectingClosingParenthesis)
                            |> Parser.map
                                (always
                                    (Parser.Loop { state | maybeStartParenthesisIndex = Nothing })
                                )
                            |> errorIfNoTurnsInParentheses
                                state
                                startParenthesisIndex

                        -- If no closing parenthesis we just keep parsing turns
                        , parseTurnLoop state

                        -- If no turns, we error informatively if this is the end as
                        -- we are inside a set of parentheses so it shouldn't end yet
                        , Parser.end ExpectingEnd
                            |> andThenWithInputStringAndOffset
                                (\( _, inputString, _ ) ->
                                    Parser.problem
                                        (UserReadyError <|
                                            UnclosedParentheses
                                                { inputString = inputString
                                                , openParenthesisIndex = toInt startParenthesisIndex
                                                }
                                        )
                                )
                        ]
           )


parseTurnLoop : ParserState -> OurParser (Parser.Step ParserState a)
parseTurnLoop state =
    Parser.succeed
        (\turnStartIndex ( turnString, turn ) ->
            Parser.Loop
                { turnList =
                    { turn = turn
                    , string = turnString
                    , startIndex = turnStartIndex
                    }
                        :: state.turnList
                , maybeStartParenthesisIndex = state.maybeStartParenthesisIndex
                }
        )
        |= Parser.getOffset
        -- Parse turn and also return the string that was parsed
        |= Parser.mapChompedString Tuple.pair
            (turnParser state
                |> errorIfRepeatedTurnables state
            )


turnParser : ParserState -> OurParser Turn
turnParser state =
    Parser.succeed Turn
        |= turnableParser state
        |= turnLengthParser
        |= directionParser


turnableParser : ParserState -> OurParser Turnable
turnableParser state =
    let
        turnableToTokenParser turnable =
            let
                token =
                    Parser.token
                        (Parser.Token
                            (turnableToString turnable)
                            (ExpectingTurnable
                                (Maybe.map
                                    (\{ startIndex, string } ->
                                        { previousTurnString = string
                                        , previousTurnstartIndex = startIndex
                                        }
                                    )
                                    (List.head state.turnList)
                                )
                            )
                        )
            in
            Parser.map (\_ -> turnable) token
    in
    Parser.oneOf (List.map turnableToTokenParser allTurnables)


turnLengthParser : OurParser TurnLength
turnLengthParser =
    Parser.oneOf
        -- WillNeverOccur here because we include the empty string which will always
        -- succeed
        [ Parser.map (\_ -> Halfway) <| Parser.token (Parser.Token "2" WillNeverOccur)
        , Parser.map (\_ -> ThreeQuarters) <| Parser.token (Parser.Token "3" WillNeverOccur)
        , Parser.map (\_ -> OneQuarter) <| Parser.token (Parser.Token "" WillNeverOccur)
        ]


directionParser : OurParser TurnDirection
directionParser =
    Parser.oneOf
        -- WillNeverOccur here because we include the empty string which will always
        -- succeed
        [ Parser.map (\_ -> CounterClockwise) <| Parser.token (Parser.Token "'" WillNeverOccur)
        , Parser.map (\_ -> Clockwise) <| Parser.token (Parser.Token "" WillNeverOccur)
        ]



-- PARSER ERROR HANDLING


verifyNotEmptyParser : Algorithm -> OurParser Algorithm
verifyNotEmptyParser (Algorithm turnList) =
    case List.length turnList of
        0 ->
            Parser.problem
                (UserReadyError EmptyAlgorithm)

        _ ->
            Parser.succeed (Algorithm turnList)


errorIfNoTurnsInParentheses :
    ParserState
    -> ParenthesisIndex
    -> OurParser a
    -> OurParser a
errorIfNoTurnsInParentheses state startParenthesisIndex parser =
    parser
        |> andThenWithInputStringAndOffset
            (\( previousValue, inputString, _ ) ->
                let
                    problem =
                        Parser.problem
                            (UserReadyError <|
                                EmptyParentheses
                                    { inputString = inputString
                                    , errorIndex = toInt startParenthesisIndex
                                    }
                            )
                in
                case state.turnList |> List.head |> Maybe.map .startIndex of
                    Nothing ->
                        problem

                    Just lastTurnstartIndex ->
                        if lastTurnstartIndex > toInt startParenthesisIndex then
                            Parser.succeed previousValue

                        else
                            problem
            )


errorIfRepeatedTurnables : ParserState -> OurParser Turn -> OurParser Turn
errorIfRepeatedTurnables state =
    andThenWithInputStringAndOffset
        (\( (Turn turnable _ _) as previousResult, inputString, _ ) ->
            case
                state.turnList
                    |> List.head
                    |> Maybe.map (\x -> ( x.turn, x.startIndex, x.string ))
            of
                Just ( Turn previousTurnable _ _, lastTurnStartIndex, lastTurnString ) ->
                    if previousTurnable == turnable then
                        Parser.problem
                            (UserReadyError <|
                                RepeatedTurnable
                                    { inputString = inputString
                                    , errorIndex =
                                        lastTurnStartIndex
                                            + String.length lastTurnString
                                    }
                            )

                    else
                        Parser.succeed previousResult

                Nothing ->
                    Parser.succeed previousResult
        )



-- PARSER HELPERS


andThenWithInputStringAndOffset : (( a, String, Int ) -> OurParser b) -> OurParser a -> OurParser b
andThenWithInputStringAndOffset fn parser =
    Parser.map (\a b c -> ( a, b, c )) parser
        |= Parser.getSource
        |= Parser.getOffset
        |> Parser.andThen fn


whitespaceParser : OurParser ()
whitespaceParser =
    Parser.chompWhile isWhitespace



-- TYPE ENUMERATORS (as lists)


{-| All possible combinations of turnables, lengths and directions

    List.length allTurns
    --> List.length allTurnables * List.length allTurnLengths * List.length allTurnDirections

-}
allTurns : List Turn
allTurns =
    ListM.return Turn
        |> ListM.applicative (ListM.fromList allTurnables)
        |> ListM.applicative (ListM.fromList allTurnLengths)
        |> ListM.applicative (ListM.fromList allTurnDirections)
        |> ListM.toList


{-| All possible turnables

    List.length allTurnables --> 12

-}
allTurnables : List Turnable
allTurnables =
    let
        fromU layer =
            case layer of
                U ->
                    Just D

                D ->
                    Just L

                L ->
                    Just R

                R ->
                    Just F

                F ->
                    Just B

                B ->
                    Just M

                M ->
                    Just S

                S ->
                    Just E

                E ->
                    Just X

                X ->
                    Just Y

                Y ->
                    Just Z

                Z ->
                    Nothing
    in
    Utils.Enumerator.from U fromU


{-| All possible turn lengths

    List.length allTurnLengths --> 3

-}
allTurnLengths : List TurnLength
allTurnLengths =
    let
        fromOneQuarter length =
            case length of
                OneQuarter ->
                    Just Halfway

                Halfway ->
                    Just ThreeQuarters

                ThreeQuarters ->
                    Nothing
    in
    Utils.Enumerator.from OneQuarter fromOneQuarter


{-| All possible turn directions

    List.length allTurnDirections --> 2

-}
allTurnDirections : List TurnDirection
allTurnDirections =
    let
        fromClockwise direction =
            case direction of
                Clockwise ->
                    Just CounterClockwise

                CounterClockwise ->
                    Nothing
    in
    Utils.Enumerator.from Clockwise fromClockwise
