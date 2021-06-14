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
    | InvalidTurnWouldWorkWithoutSpace
        { inputString : String
        , wrongWhitespaceStart : Int
        , wrongWhitespaceEnd : Int
        }
    | InvalidTurnApostropheWrongSideOfLength
        { inputString : String
        , errorIndex : Int
        }
    | UnclosedParentheses
    | UnmatchedClosingParenthesis
    | EmptyParentheses
    | NestedParentheses
    | SpansOverSeveralLines String
    | InvalidSymbol Char
    | UnexpectedError String


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
                    List.filter ((/=) 1) (List.map .row deadends)

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
    Result.mapError UnexpectedError <|
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
        EmptyAlgorithmParsingProblem ->
            False

        RepeatedTurnableParsingProblem _ ->
            False

        UnclosedParenthesesParsingProblem ->
            False

        EmptyParenthesesParsingProblem ->
            False

        ExpectingTurnable _ ->
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
        ExpectingTurnable _ ->
            Just ( problem, 1 )

        EmptyAlgorithmParsingProblem ->
            Just ( problem, 2 )

        RepeatedTurnableParsingProblem _ ->
            Just ( problem, 2 )

        UnclosedParenthesesParsingProblem ->
            Just ( problem, 2 )

        EmptyParenthesesParsingProblem ->
            Just ( problem, 2 )

        ExpectingOpeningParenthesis ->
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
        EmptyAlgorithmParsingProblem ->
            EmptyAlgorithm

        UnclosedParenthesesParsingProblem ->
            UnclosedParentheses

        EmptyParenthesesParsingProblem ->
            EmptyParentheses

        ExpectingTurnable { previousTurnString, previousTurnStartCol } ->
            if
                turnWouldHaveWorkedWithoutWhitespace
                    { unexpectedString = unexpectedString
                    , previousTurnString = previousTurnString
                    }
            then
                InvalidTurnWouldWorkWithoutSpace
                    { inputString = inputString
                    , wrongWhitespaceStart = previousTurnStartCol + String.length previousTurnString - 1
                    , wrongWhitespaceEnd = index
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
                -- \u{000D} is the carriage return character \r but elm format forces it to
                -- this style. see https://github.com/avh4/elm-format/issues/376

            else if unexpectedString == "\n" || unexpectedString == "\u{000D}" then
                SpansOverSeveralLines inputString

            else if unexpectedString == ")" then
                UnmatchedClosingParenthesis

            else if unexpectedString == "(" then
                NestedParentheses

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
                InvalidSymbol <|
                    Tuple.second <|
                        wasUnexpectedCharacter unexpectedString

            else
                InvalidTurnable
                    { inputString = inputString
                    , errorIndex = index
                    , invalidTurnable = String.slice index (index + 1) inputString
                    }

        RepeatedTurnableParsingProblem { previousTurnString, previousTurnStartCol } ->
            RepeatedTurnable
                { inputString = inputString
                , errorIndex = previousTurnStartCol + String.length previousTurnString - 1
                }

        ExpectingEnd ->
            UnexpectedError "We Expected UnexpectedEnd Problems To Have Been Filtered Out"

        WillNeverOccur ->
            UnexpectedError "A problem we never expected to happen happened anyway"

        _ ->
            UnexpectedError "TBD"


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
    = ExpectingTurnable
        { previousTurnString : String
        , previousTurnStartCol : Int
        }
    | RepeatedTurnableParsingProblem
        { previousTurnString : String
        , previousTurnStartCol : Int
        }
    | ExpectingOpeningParenthesis
    | ExpectingClosingParenthesis
    | UnclosedParenthesesParsingProblem
    | EmptyParenthesesParsingProblem
    | EmptyAlgorithmParsingProblem
    | ExpectingEnd
    | WillNeverOccur


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
            , lastTurn = ( "", 0 )
            , insideParentheses = Nothing
            }
            buildTurnListLoop
        |> Parser.andThen verifyNotEmptyParser


type alias ParserState =
    { turnList : List Turn
    , lastTurn : ( String, Int )
    , insideParentheses : Maybe Int
    }


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
        |. whitespaceParser
        |= (case state.insideParentheses of
                Nothing ->
                    Parser.oneOf
                        [ Parser.succeed (\col -> Parser.Loop { state | insideParentheses = Just col })
                            |= Parser.getCol
                            |. Parser.symbol (Parser.Token "(" ExpectingOpeningParenthesis)
                        , parseTurnLoop state
                        , Parser.end ExpectingEnd
                            |> Parser.map
                                (\_ -> Parser.Done (List.reverse state.turnList))
                        ]

                Just parenthesesStartCol ->
                    Parser.oneOf
                        [ Parser.symbol (Parser.Token ")" ExpectingClosingParenthesis)
                            |> Parser.andThen
                                (\_ ->
                                    if Tuple.second state.lastTurn > parenthesesStartCol then
                                        Parser.succeed
                                            (Parser.Loop { state | insideParentheses = Nothing })

                                    else
                                        Parser.problem EmptyParenthesesParsingProblem
                                )
                        , parseTurnLoop state
                        , Parser.end ExpectingEnd
                            |> Parser.andThen (always <| Parser.problem UnclosedParenthesesParsingProblem)
                        ]
           )


parseTurnLoop : ParserState -> OurParser (Parser.Step ParserState a)
parseTurnLoop state =
    Parser.succeed
        (\turnStartCol ( turnString, turn ) ->
            Parser.Loop
                { turnList = turn :: state.turnList
                , lastTurn = ( turnString, turnStartCol )
                , insideParentheses = state.insideParentheses
                }
        )
        |= Parser.getCol
        |= (Parser.mapChompedString Tuple.pair (turnParser state.lastTurn)
                |> Parser.andThen
                    (\(( _, Turn turnable _ _ ) as parserResult) ->
                        case List.head state.turnList of
                            Just (Turn previousTurnable _ _) ->
                                if previousTurnable == turnable then
                                    Parser.problem
                                        (RepeatedTurnableParsingProblem
                                            { previousTurnString = Tuple.first state.lastTurn
                                            , previousTurnStartCol = Tuple.second state.lastTurn
                                            }
                                        )

                                else
                                    Parser.succeed parserResult

                            Nothing ->
                                Parser.succeed parserResult
                    )
           )


whitespaceParser : OurParser ()
whitespaceParser =
    Parser.chompWhile isWhitespace


turnParser : ( String, Int ) -> OurParser Turn
turnParser previousTurn =
    Parser.succeed Turn
        |= turnableParser previousTurn
        |= turnLengthParser
        |= directionParser


turnableParser : ( String, Int ) -> OurParser Turnable
turnableParser previousTurn =
    let
        turnableToTokenParser turnable =
            let
                token =
                    Parser.token
                        (Parser.Token
                            (turnableToString turnable)
                            (ExpectingTurnable
                                { previousTurnString = Tuple.first previousTurn
                                , previousTurnStartCol = Tuple.second previousTurn
                                }
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


verifyNotEmptyParser : Algorithm -> OurParser Algorithm
verifyNotEmptyParser (Algorithm turnList) =
    case List.length turnList of
        0 ->
            Parser.problem EmptyAlgorithmParsingProblem

        _ ->
            Parser.succeed (Algorithm turnList)



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
