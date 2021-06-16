module Algorithm exposing (Algorithm, Turn(..), TurnDirection(..), TurnLength(..), Turnable(..), allTurnDirections, allTurnLengths, allTurnables, allTurns, append, appendTo, build, empty, toTurnList, fromString, inverse, toString, FromStringError(..))

{-| Documentation to come

@docs Algorithm, Turn, TurnDirection, TurnLength, Turnable, allTurnDirections, allTurnLengths, allTurnables, allTurns, append, appendTo, build, empty, toTurnList, fromString, inverse, toString, FromStringError

-}

import Monads.ListM as ListM
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
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
        userReadyErrors =
            deadends
                |> List.map .problem
                |> List.filterMap
                    (\problem ->
                        case problem of
                            UserReadyError error ->
                                Just error

                            _ ->
                                Nothing
                    )
    in
    case userReadyErrors of
        [] ->
            UnexpectedError
                { inputString = string
                , errorIndex =
                    List.head deadends
                        |> Maybe.map .col
                        |> Maybe.map (\x -> x - 1)
                        |> Maybe.withDefault 0
                , debugInfo = "No user ready errors were found"
                }

        _ :: _ :: _ ->
            UnexpectedError
                { inputString = string
                , errorIndex =
                    List.head deadends
                        |> Maybe.map .col
                        |> Maybe.map (\x -> x - 1)
                        |> Maybe.withDefault 0
                , debugInfo = "Several user ready errors were found"
                }

        error :: _ ->
            error



-- PARSER


type alias OurParser result =
    Parser Never ParsingProblem result


type ParsingProblem
    = UserReadyError FromStringError
    | ExpectingTurnable
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
        |> Parser.andThen verifyNotEmpty


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
        |= Parser.oneOf
            [ case state.maybeStartParenthesisIndex of
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
                        , errorIfTokenEncountered ")"
                            (\{ string, tokenStartIndex } ->
                                UnmatchedClosingParenthesis
                                    { inputString = string
                                    , errorIndex = tokenStartIndex
                                    }
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
                        , errorIfEndEncountered
                            (\{ string } ->
                                UnclosedParentheses
                                    { inputString = string
                                    , openParenthesisIndex = toInt startParenthesisIndex
                                    }
                            )

                        -- Error appropriately if we see another opening parenthesis inside a set
                        -- of parentheses
                        , errorIfTokenEncountered "("
                            (\{ string, tokenStartIndex } ->
                                NestedParentheses
                                    { inputString = string
                                    , errorIndex = tokenStartIndex
                                    }
                            )
                        ]
            , classifyError state
            ]


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
        |= (turnParser
                |> errorIfRepeatedTurnables state
                |> Parser.mapChompedString Tuple.pair
           )


turnParser : OurParser Turn
turnParser =
    Parser.succeed Turn
        |= turnableParser
        |= turnLengthParser
        |= directionParser


turnableParser : OurParser Turnable
turnableParser =
    let
        turnableToTokenParser turnable =
            let
                token =
                    Parser.token
                        (Parser.Token
                            (turnableToString turnable)
                            ExpectingTurnable
                        )
            in
            Parser.map (\_ -> turnable) token
    in
    Parser.oneOf (List.map turnableToTokenParser allTurnables)


turnLengthParser : OurParser TurnLength
turnLengthParser =
    Parser.succeed (\a b c -> ( a, b, c ))
        |= Parser.getSource
        |= Parser.getOffset
        |= Parser.getChompedString
            (Parser.chompWhile Char.isDigit)
        |> Parser.andThen
            (\( inputString, startIndex, intString ) ->
                case intString of
                    "" ->
                        Parser.succeed OneQuarter

                    "2" ->
                        Parser.succeed Halfway

                    "3" ->
                        Parser.succeed ThreeQuarters

                    _ ->
                        Parser.problem <|
                            UserReadyError <|
                                InvalidTurnLength
                                    { inputString = inputString
                                    , invalidLength = intString
                                    , errorIndex = startIndex
                                    }
            )


directionParser : OurParser TurnDirection
directionParser =
    Parser.oneOf
        -- WillNeverOccur here because we include the empty string which will always
        -- succeed
        [ Parser.map (\_ -> CounterClockwise) <| Parser.token (Parser.Token "'" WillNeverOccur)
        , Parser.map (\_ -> Clockwise) <| Parser.token (Parser.Token "" WillNeverOccur)
        ]


classifyError : ParserState -> OurParser a
classifyError state =
    Parser.oneOf
        [ errorIfTokenEncountered "\n"
            (\{ string } ->
                SpansOverSeveralLines string
            )

        -- \u{000D} is the carriage return character \r but elm format forces it to
        -- this style. see https://github.com/avh4/elm-format/issues/376
        , errorIfTokenEncountered "\u{000D}"
            (\{ string } ->
                SpansOverSeveralLines string
            )
        , detectIfTurnWasInterrupted state
        , detectIfApostrophePlacedOnWrongSide state

        -- If it's an alphanumeric character or ' we just log it as
        -- an invalid turnable as those are the type of characters
        -- we expect in an algorithm
        , errorIfChar (\c -> Char.isAlphaNum c || c == '\'')
            (\{ char, string, tokenStartIndex } ->
                InvalidTurnable
                    { inputString = string
                    , errorIndex = tokenStartIndex
                    , invalidTurnable = String.fromChar char
                    }
            )

        -- If it's any other type of character we error it as an invalid symbol
        , errorIfChar (always True)
            (\{ char, string, tokenStartIndex } ->
                InvalidSymbol
                    { inputString = string
                    , errorIndex = tokenStartIndex
                    , symbol = char
                    }
            )
        ]



-- PARSER ERROR HANDLING


verifyNotEmpty : Algorithm -> OurParser Algorithm
verifyNotEmpty ((Algorithm turnList) as previousResult) =
    if List.isEmpty turnList then
        Parser.problem
            (UserReadyError EmptyAlgorithm)

    else
        Parser.succeed previousResult


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


detectIfTurnWasInterrupted : ParserState -> OurParser a
detectIfTurnWasInterrupted state =
    case state.turnList of
        [] ->
            Parser.problem (ExpectingUnwantedString "")

        previousTurn :: _ ->
            Parser.succeed String.slice
                |= Parser.getOffset
                |= Parser.map ((+) 1) Parser.getOffset
                |= Parser.getSource
                |> Parser.andThen
                    (\nextChar ->
                        if
                            resultToBool <|
                                Parser.run
                                    (turnParser |. Parser.end ExpectingEnd)
                                    (previousTurn.string ++ nextChar)
                        then
                            alwaysError
                                (\{ string, tokenStartIndex } ->
                                    TurnWouldWorkWithoutInterruption
                                        { inputString = string
                                        , interruptionStart = previousTurn.startIndex + String.length previousTurn.string
                                        , interruptionEnd = tokenStartIndex - 1
                                        }
                                )

                        else
                            Parser.problem (ExpectingUnwantedString "")
                    )


detectIfApostrophePlacedOnWrongSide : ParserState -> OurParser a
detectIfApostrophePlacedOnWrongSide state =
    case state.turnList of
        [] ->
            Parser.problem (ExpectingUnwantedString "")

        previousTurn :: _ ->
            Parser.succeed String.slice
                |= Parser.getOffset
                |= Parser.map ((+) 1) Parser.getOffset
                |= Parser.getSource
                |> Parser.andThen
                    (\nextChar ->
                        if
                            resultToBool
                                (Parser.run
                                    (turnParser |. Parser.end ExpectingEnd)
                                    (String.dropRight 1 previousTurn.string
                                        ++ nextChar
                                        ++ String.right 1 previousTurn.string
                                    )
                                )
                        then
                            alwaysError
                                (\{ string, tokenStartIndex } ->
                                    InvalidTurnApostropheWrongSideOfLength
                                        { inputString = string
                                        , errorIndex = tokenStartIndex - 2
                                        }
                                )

                        else
                            Parser.problem (ExpectingUnwantedString "")
                    )



-- PARSER HELPERS


andThenWithInputStringAndOffset : (( a, String, Int ) -> OurParser b) -> OurParser a -> OurParser b
andThenWithInputStringAndOffset fn parser =
    Parser.map (\a b c -> ( a, b, c )) parser
        |= Parser.getSource
        |= Parser.getOffset
        |> Parser.andThen fn


resultToBool : Result a b -> Bool
resultToBool =
    Result.map (always True) >> Result.withDefault False


whitespaceParser : OurParser ()
whitespaceParser =
    Parser.chompWhile isWhitespace


errorIfTokenEncountered :
    String
    -> ({ string : String, tokenStartIndex : Int } -> FromStringError)
    -> OurParser a
errorIfTokenEncountered token fn =
    Parser.token (Parser.Token token (ExpectingUnwantedString token))
        |> andThenWithInputStringAndOffset
            (\( _, inputString, offset ) ->
                Parser.problem <|
                    UserReadyError <|
                        fn
                            { string = inputString
                            , tokenStartIndex = offset - String.length token
                            }
            )


errorIfEndEncountered :
    ({ string : String, tokenStartIndex : Int } -> FromStringError)
    -> OurParser a
errorIfEndEncountered fn =
    Parser.end (ExpectingUnwantedString "")
        |> andThenWithInputStringAndOffset
            (\( _, inputString, offset ) ->
                Parser.problem <|
                    UserReadyError <|
                        fn
                            { string = inputString
                            , tokenStartIndex = offset
                            }
            )


errorIfChar :
    (Char -> Bool)
    -> ({ char : Char, string : String, tokenStartIndex : Int } -> FromStringError)
    -> OurParser a
errorIfChar condition fn =
    (Parser.getChompedString <|
        Parser.chompIf condition (ExpectingUnwantedString "")
    )
        |> andThenWithInputStringAndOffset
            (\( charString, inputString, offset ) ->
                case String.toList charString of
                    [ char ] ->
                        Parser.problem <|
                            UserReadyError <|
                                fn
                                    { char = char
                                    , string = inputString
                                    , tokenStartIndex = offset - 1
                                    }

                    _ ->
                        -- Getting chomped string for chompIf should always
                        -- be a single string
                        Parser.problem WillNeverOccur
            )


alwaysError :
    ({ string : String, tokenStartIndex : Int } -> FromStringError)
    -> OurParser a
alwaysError fn =
    Parser.succeed Tuple.pair
        -- This is what ensures no other options in a Parser.oneOf
        -- will run, as it stops looking as soon as one option has
        -- chomped a character
        |. Parser.chompIf (always True) (ExpectingUnwantedString "")
        |= Parser.getSource
        |= Parser.getOffset
        |> Parser.andThen
            (\( inputString, offset ) ->
                Parser.problem <|
                    UserReadyError <|
                        fn
                            { string = inputString
                            , tokenStartIndex = offset
                            }
            )



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
