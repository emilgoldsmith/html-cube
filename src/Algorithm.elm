module Algorithm exposing
    ( Algorithm, Turn(..), Turnable(..), TurnLength(..), TurnDirection(..)
    , fromTurnList, empty
    , toString, fromString, FromStringError(..)
    , inverse, append, reverseAppend
    , allTurns, allTurnables, allTurnLengths, allTurnDirections
    , toTurnList
    )

{-|


# Definition

@docs Algorithm, Turn, Turnable, TurnLength, TurnDirection


# Constructors

@docs fromTurnList, empty


# (De)Serialization

@docs toString, fromString, FromStringError


# Helpers

@docs inverse, append, reverseAppend


# Enumerations

@docs allTurns, allTurnables, allTurnLengths, allTurnDirections


# Advanced

@docs toTurnList

-}

import List.Nonempty
import Monads.ListM as ListM
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import Utils.Enumerator



-- ALGORITHM MODEL / DEFINTION


{-| Any sequence of turns on a 3x3 Rubik's Cube.
Usually used to describe an
[algorithm](https://www.speedsolving.com/wiki/index.php/Algorithm)
used to solve a specific case for speedcubing.

The notation used is based on:
<https://www.speedsolving.com/wiki/index.php/Notation>

It is meant to be used for speedcubing so it includes
options such as cube rotations, wide moves and
three-quarter turns even if those aren't necessary to
be able to solve the cube, but can be important in
describing the fastest way to solve a given case for
a human

-}
type Algorithm
    = Algorithm (List Turn)


{-| Describes a single turn, which is rotating any turnable
in a given direction for a given amount of degrees
-}
type Turn
    = Turn Turnable TurnLength TurnDirection


{-| Describes anything that can be turned on a Rubik's Cube
such as a face, a slice, a rotation of the whole cube or
several slices together also known as a wide move
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
      -- Wide Moves
    | Uw
    | Dw
    | Lw
    | Rw
    | Fw
    | Bw
      -- Whole cube rotations (lowercase type constructors not allowed in Elm)
    | X
    | Y
    | Z


{-| Describes how much to turn a turnable in a turn
-}
type TurnLength
    = OneQuarter
    | Halfway
    | ThreeQuarters


{-| Describes which direction to turn a turnable in a turn
-}
type TurnDirection
    = Clockwise
    | CounterClockwise



-- CONSTRUCTORS


{-| Create an Algorithm from a list of turns

    fromTurnList
        [ Turn U OneQuarter CounterClockwise
        , Turn B Halfway Clockwise
        ]

-}
fromTurnList : List Turn -> Algorithm
fromTurnList =
    Algorithm


{-| An empty algorithm
-}
empty : Algorithm
empty =
    Algorithm []



-- HELPERS


{-| We append the two arguments, so a ++ b

    Result.map2 append (fromString "U") (fromString "B'")
    --> fromString "UB'"

-}
append : Algorithm -> Algorithm -> Algorithm
append (Algorithm a) (Algorithm b) =
    Algorithm (a ++ b)


{-| We append the two algorithms in reverse, so b ++ a

    Result.map2 reverseAppend (fromString "U") (fromString "B'")
    --> fromString "B'U"

-}
reverseAppend : Algorithm -> Algorithm -> Algorithm
reverseAppend (Algorithm a) (Algorithm b) =
    Algorithm (b ++ a)


{-| Get the inverse of the algorithm.

By definition this means that if one was to
apply the algorithm followed by its inverse
one would arrive at the same state one started at.

    Result.map inverse <| fromString "UB'"
    --> fromString "BU'"

-}
inverse : Algorithm -> Algorithm
inverse algorithm =
    algorithm
        |> toTurnList
        |> List.reverse
        |> List.map inverseTurn
        |> fromTurnList


inverseTurn : Turn -> Turn
inverseTurn (Turn a b direction) =
    Turn a b (inverseDirection direction)


inverseDirection : TurnDirection -> TurnDirection
inverseDirection direction =
    case direction of
        Clockwise ->
            CounterClockwise

        CounterClockwise ->
            Clockwise



-- (DE)SERIALIZATION


{-| Display an algorithm in a standard format readable by humans

    fromTurnList
        [ Turn U Halfway CounterClockwise
        , Turn F OneQuarter Clockwise
        ]
        |> toString
    --> "U2' F"

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
    turnableToStringWideMovesTwoCharacters turnable
        ++ turnLengthToString length
        ++ turnDirectionToString direction


turnableToStringWideMovesTwoCharacters : Turnable -> String
turnableToStringWideMovesTwoCharacters x =
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

        Uw ->
            "Uw"

        Dw ->
            "Dw"

        Rw ->
            "Rw"

        Lw ->
            "Lw"

        Fw ->
            "Fw"

        Bw ->
            "Bw"

        X ->
            "x"

        Y ->
            "y"

        Z ->
            "z"


turnableToStringLowercaseWideMoves : Turnable -> String
turnableToStringLowercaseWideMoves x =
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

        Uw ->
            "u"

        Dw ->
            "d"

        Rw ->
            "r"

        Lw ->
            "l"

        Fw ->
            "f"

        Bw ->
            "b"

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


{-| The different descriptions of in which way a string is
not a valid algorithm string. Note that all these errors
assume that the string is user input, and so makes some
opinionated decisions about when and how to error based
on that.

If you want to programatically create arbitrary algorithms
you should use constructors such as
[fromTurnList](#fromTurnList)

For an example of how to handle and display these errors to
a user on user input, make sure to check out this
[full user input example](https://github.com/emilgoldsmith/elm-speedcubing/blob/main/examples/src/AlgorithmFromString.elm)

  - **EmptyAlgorithm**: There were no turns in the string and
    this does not make sense for user input.

    If you need behaviour like this just allow the user to not
    input any algorithm at all

  - **InvalidTurnable**: A turnable such as U or x was expected
    but not found

  - **InvalidTurnLength**: It seems like a turn length such as 2
    or 3 was attempted to be specified but wasn't valid

  - **RepeatedTurnable**: The same turnable was repeated twice in
    a row which would never make sense in an algorithm. The correct
    way to describe this is by combining the two into one such as
    UU becoming U2, or UU' just not being there at all

  - **TurnWouldWorkWithoutInterruption**: It looks like an otherwise
    correct turn was specified but a parenthesis, some whitespace
    or something similar came in the way making it invalid

  - **ApostropheWrongSideOfLength**: It looks like the apostrophe
    was put on the wrong side of the length, such as U'2 instead
    of U2' in an otherwise correct turn, just swapping these
    would make it valid

  - **UnclosedParenthesis**: There is an opening parenthesis that
    was never closed

  - **UnmatchedClosingParenthesis**: There is a closing parenthesis
    that doesn't have an opening match

  - **EmptyParentheses**: There is a set of parentheses that aren't
    enclosing any turns, which does not make sense as parentheses are
    used to group turns to help memorization and execution

  - **NestedParentheses**: A second set of parentheses were started
    within a set of parentheses. Nested triggers or other nested grouping
    hasn't seemed to be a relevant need anywhere in the community
    so it is not allowed until need has been proven

  - **SpansOverSeveralLines**: An algorithm is not allowed to span over
    several different lines, the string input should just be a single line

  - **InvalidSymbol**: A symbol was encountered that does not make any
    sense in an algorithm anywhere.

  - **UnexpectedError**: The parsing code behaved in an unexpected way.
    This should never happen unless we have a bug in our code. If you get
    this error in production code just either tell the user that the
    algorithm wasn't valid without any further explanation or tell them
    something unexpected happen. The debugInfo key is not helpful to show
    to users, but only for figuring out how to fix the Elm Package to avoid
    this happening in the future, and logging a
    [Github Issue](https://github.com/emilgoldsmith/elm-speedcubing/issues/new)
    with the debugInfo would be very appreciated

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
    | WideMoveStylesMixed
    | TurnWouldWorkWithoutInterruption
        { inputString : String
        , interruptionStart : Int
        , interruptionEnd : Int
        }
    | ApostropheWrongSideOfLength
        { inputString : String
        , errorIndex : Int
        }
    | UnclosedParenthesis
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


{-| Parses user input and either returns the algorithm
described by the string or a detailed error description of
why it failed

    fromString "" --> Err EmptyAlgorithm

    fromString "U" --> Ok (fromTurnList [Turn U OneQuarter Clockwise])

-}
fromString : String -> Result FromStringError Algorithm
fromString string =
    let
        twoCharacterWideMovesResult =
            Parser.run (algorithmParser turnableToStringWideMovesTwoCharacters) string
                |> Result.mapError (parserErrorToFromStringError string)

        lowercaseWideMovesResult =
            Parser.run (algorithmParser turnableToStringLowercaseWideMoves) string
                |> Result.mapError (parserErrorToFromStringError string)
    in
    case ( twoCharacterWideMovesResult, lowercaseWideMovesResult ) of
        ( Ok _, _ ) ->
            twoCharacterWideMovesResult

        ( _, Ok _ ) ->
            lowercaseWideMovesResult

        ( Err (InvalidTurnable twoCharacter), Err (InvalidTurnable lowercase) ) ->
            let
                latestError =
                    if twoCharacter.errorIndex > lowercase.errorIndex then
                        twoCharacter

                    else
                        lowercase
            in
            if stringIsWideMove latestError.invalidTurnable then
                Err WideMoveStylesMixed

            else
                twoCharacterWideMovesResult

        _ ->
            twoCharacterWideMovesResult


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
    | CheckingForSpecificErrorType { debugInfo : String }
    | WillNeverOccur { debugInfo : String }


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


algorithmParser : (Turnable -> String) -> OurParser Algorithm
algorithmParser turnableToString =
    Parser.succeed Algorithm
        |= Parser.loop
            { turnList = []
            , maybeStartParenthesisIndex = Nothing
            }
            (buildTurnListLoop turnableToString)
        |> Parser.andThen verifyNotEmpty


buildTurnListLoop :
    (Turnable -> String)
    -> ParserState
    ->
        OurParser
            (Parser.Step
                ParserState
                (List Turn)
            )
buildTurnListLoop turnableToString state =
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
                        , parseTurnLoop turnableToString state

                        -- If there is no turn check if we're at the end
                        , Parser.end ExpectingEnd
                            |> Parser.map
                                -- We reverse here because the list is built with :: in reverse
                                (\_ -> Parser.Done (state.turnList |> List.map .turn |> List.reverse))

                        -- Error appropriately if we encounter a closing parenthesis when we're not
                        -- inside a set of parentheses
                        , errorIfToken ")"
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
                        , parseTurnLoop turnableToString state

                        -- If no turns, we error informatively if this is the end as
                        -- we are inside a set of parentheses so it shouldn't end yet
                        , errorIfEndEncountered
                            (\{ string } ->
                                UnclosedParenthesis
                                    { inputString = string
                                    , openParenthesisIndex = toInt startParenthesisIndex
                                    }
                            )

                        -- Error appropriately if we see another opening parenthesis inside a set
                        -- of parentheses
                        , errorIfToken "("
                            (\{ string, tokenStartIndex } ->
                                NestedParentheses
                                    { inputString = string
                                    , errorIndex = tokenStartIndex
                                    }
                            )
                        ]
            , classifyError turnableToString state
            ]


parseTurnLoop : (Turnable -> String) -> ParserState -> OurParser (Parser.Step ParserState a)
parseTurnLoop turnableToString state =
    Parser.succeed
        (\turnStartIndex ( turnString, turn ) ->
            Parser.Loop
                { state
                    | turnList =
                        { turn = turn
                        , string = turnString
                        , startIndex = turnStartIndex
                        }
                            :: state.turnList
                }
        )
        |= Parser.getOffset
        -- Parse turn and also return the string that was parsed
        |= (turnParser turnableToString
                |> errorIfRepeatedTurnables state
                |> Parser.mapChompedString Tuple.pair
           )


turnParser : (Turnable -> String) -> OurParser Turn
turnParser turnableToString =
    Parser.succeed Turn
        |= turnableParser turnableToString
        |= turnLengthParser
        |= directionParser


turnableParser : (Turnable -> String) -> OurParser Turnable
turnableParser turnableToString =
    allTurnables
        |> List.Nonempty.toList
        -- Make sure we try the longest tokens first, as otherwise
        -- "U" will be matched before "Uw" etc.
        |> List.sortBy (turnableToString >> String.length)
        |> List.reverse
        |> List.map (turnableToTokenParser turnableToString)
        |> Parser.oneOf


turnableToTokenParser : (Turnable -> String) -> Turnable -> OurParser Turnable
turnableToTokenParser turnableToString turnable =
    Parser.map (always turnable) <|
        Parser.token
            (Parser.Token
                (turnableToString turnable)
                ExpectingTurnable
            )


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
        [ Parser.map (\_ -> CounterClockwise) <|
            Parser.token (Parser.Token "'" <| WillNeverOccur { debugInfo = "counter clockwise token" })
        , Parser.map (\_ -> Clockwise) <|
            Parser.token (Parser.Token "" <| WillNeverOccur { debugInfo = "clockwise token" })
        ]


classifyError : (Turnable -> String) -> ParserState -> OurParser a
classifyError turnableToString state =
    Parser.oneOf
        [ errorIfToken "\n"
            (\{ string } ->
                SpansOverSeveralLines string
            )

        -- \u{000D} is the carriage return character \r but elm format forces it to
        -- this style. see https://github.com/avh4/elm-format/issues/376
        , errorIfToken "\u{000D}"
            (\{ string } ->
                SpansOverSeveralLines string
            )
        , detectIfTurnWasInterrupted turnableToString state
        , detectIfApostrophePlacedOnWrongSide turnableToString state
        , throwInvalidTurnableIfValidCharacters

        -- If it's any other type of character we error it as an invalid symbol.
        -- We use this function to get the character as well as opposed to alwaysError
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
verifyNotEmpty algorithm =
    if algorithm == empty then
        Parser.problem
            (UserReadyError EmptyAlgorithm)

    else
        Parser.succeed algorithm


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

                    Just lastTurnStartIndex ->
                        if lastTurnStartIndex >= toInt startParenthesisIndex then
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
                    -- Change this to a tuple so we can destructure the turn in the
                    -- below case statement
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


detectIfTurnWasInterrupted : (Turnable -> String) -> ParserState -> OurParser a
detectIfTurnWasInterrupted turnableToString state =
    case state.turnList of
        [] ->
            Parser.problem
                (CheckingForSpecificErrorType
                    { debugInfo = "turn interrupted: No previous turns" }
                )

        previousTurn :: _ ->
            Parser.succeed
                (\offset source ->
                    ( offset
                    , String.slice offset (offset + 1) source
                    )
                )
                |= Parser.getOffset
                |= Parser.getSource
                |> Parser.andThen
                    (\( currentIndex, nextChar ) ->
                        let
                            interruptionStart =
                                previousTurn.startIndex + String.length previousTurn.string

                            interuptionEnd =
                                currentIndex
                        in
                        if
                            interruptionStart
                                < interuptionEnd
                                && (resultToBool <|
                                        Parser.run
                                            (turnParser turnableToString |. Parser.end ExpectingEnd)
                                            (previousTurn.string ++ nextChar)
                                   )
                        then
                            alwaysError
                                (\{ string } ->
                                    TurnWouldWorkWithoutInterruption
                                        { inputString = string
                                        , interruptionStart = interruptionStart
                                        , interruptionEnd = interuptionEnd
                                        }
                                )

                        else
                            Parser.problem
                                (CheckingForSpecificErrorType
                                    { debugInfo =
                                        "turn interrupted: Wasn't valid turn without interruption"
                                    }
                                )
                    )


detectIfApostrophePlacedOnWrongSide : (Turnable -> String) -> ParserState -> OurParser a
detectIfApostrophePlacedOnWrongSide turnableToString state =
    case state.turnList of
        [] ->
            Parser.problem
                (CheckingForSpecificErrorType
                    { debugInfo =
                        "wrong apostrophe: No previous turns"
                    }
                )

        previousTurn :: _ ->
            Parser.succeed String.slice
                |= Parser.getOffset
                |= Parser.map ((+) 1) Parser.getOffset
                |= Parser.getSource
                |> Parser.andThen
                    (\nextChar ->
                        if
                            String.right 1 previousTurn.string
                                == "'"
                                && (nextChar == "2" || nextChar == "3")
                                && resultToBool
                                    (Parser.run
                                        (turnParser turnableToString |. Parser.end ExpectingEnd)
                                        (String.dropRight 1 previousTurn.string
                                            ++ nextChar
                                            ++ String.right 1 previousTurn.string
                                        )
                                    )
                        then
                            alwaysError
                                (\{ string, tokenStartIndex } ->
                                    ApostropheWrongSideOfLength
                                        { inputString = string

                                        -- Minus 1 to point back at the apostrophe we already passed
                                        , errorIndex = tokenStartIndex - 1
                                        }
                                )

                        else
                            Parser.problem
                                (CheckingForSpecificErrorType
                                    { debugInfo =
                                        "wrong apostrophe: Wasn't valid after swap"
                                    }
                                )
                    )


throwInvalidTurnableIfValidCharacters : OurParser a
throwInvalidTurnableIfValidCharacters =
    Parser.succeed String.slice
        |= Parser.map (\x -> x - 1) Parser.getOffset
        |= Parser.getOffset
        |= Parser.getSource
        |> Parser.andThen
            (\previousCharString ->
                case String.toList previousCharString of
                    -- We're at the start of the string as there is no previous character
                    [] ->
                        -- We already checked for numbers and apostrophes at the start
                        errorIfValidCharacterEncountered

                    [ previousChar ] ->
                        -- It resembles an attempt at a two character wide move so we include
                        -- that in the invalidTurnable
                        if Char.isAlpha previousChar then
                            errorIfToken "w"
                                (\{ string, tokenStartIndex } ->
                                    InvalidTurnable
                                        { inputString = string
                                        , errorIndex = tokenStartIndex - 1
                                        , invalidTurnable = String.cons previousChar "w"
                                        }
                                )

                        else
                            errorIfValidCharacterEncountered

                    _ ->
                        Parser.problem
                            (WillNeverOccur
                                { debugInfo = "throwInvalidTurnable: single char string wasn't a single char"
                                }
                            )
            )


errorIfValidCharacterEncountered : OurParser a
errorIfValidCharacterEncountered =
    errorIfChar (\c -> Char.isAlphaNum c || c == '\'')
        (\{ string, char, tokenStartIndex } ->
            InvalidTurnable
                { inputString = string
                , errorIndex = tokenStartIndex
                , invalidTurnable = String.fromChar char
                }
        )



-- PARSER HELPERS


andThenWithInputStringAndOffset : (( a, String, Int ) -> OurParser b) -> OurParser a -> OurParser b
andThenWithInputStringAndOffset fn parser =
    Parser.map (\a b c -> ( a, b, c )) parser
        |= Parser.getSource
        |= Parser.getOffset
        |> Parser.andThen fn


stringIsWideMove : String -> Bool
stringIsWideMove string =
    let
        wideMoves =
            allTurnables
                |> List.Nonempty.toList
                |> List.filter turnableIsWideMove
    in
    List.member string (List.map turnableToStringWideMovesTwoCharacters wideMoves)
        || List.member string (List.map turnableToStringLowercaseWideMoves wideMoves)


turnableIsWideMove : Turnable -> Bool
turnableIsWideMove turnable =
    case turnable of
        Uw ->
            True

        Dw ->
            True

        Lw ->
            True

        Rw ->
            True

        Fw ->
            True

        Bw ->
            True

        U ->
            False

        D ->
            False

        L ->
            False

        R ->
            False

        F ->
            False

        B ->
            False

        M ->
            False

        S ->
            False

        E ->
            False

        X ->
            False

        Y ->
            False

        Z ->
            False


resultToBool : Result a b -> Bool
resultToBool =
    Result.map (always True) >> Result.withDefault False


whitespaceParser : OurParser ()
whitespaceParser =
    Parser.chompWhile isWhitespace


errorIfToken :
    String
    -> ({ string : String, tokenStartIndex : Int } -> FromStringError)
    -> OurParser a
errorIfToken token fn =
    Parser.token
        (Parser.Token token
            (CheckingForSpecificErrorType
                { debugInfo =
                    "error if token = `" ++ token ++ "`"
                }
            )
        )
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
    Parser.end
        (CheckingForSpecificErrorType
            { debugInfo = "error if end encountered" }
        )
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
        Parser.chompIf condition
            (CheckingForSpecificErrorType
                { debugInfo =
                    "errorIfChar: not found"
                }
            )
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
                        Parser.problem
                            (WillNeverOccur
                                { debugInfo = "errorIfChar chompIf returned string of length different from 1"
                                }
                            )
            )


alwaysError :
    ({ string : String, tokenStartIndex : Int } -> FromStringError)
    -> OurParser a
alwaysError fn =
    Parser.succeed Tuple.pair
        |= Parser.getOffset
        |= Parser.getSource
        -- This is what ensures no other options in a Parser.oneOf
        -- will run, as it stops looking as soon as one option has
        -- chomped a character
        |. Parser.oneOf
            [ Parser.chompIf (always True)
                (CheckingForSpecificErrorType
                    { debugInfo = "alwaysError chompIf" }
                )
            , Parser.end
                (CheckingForSpecificErrorType
                    { debugInfo = "alwaysError end" }
                )
            ]
        |> Parser.andThen
            (\( offset, inputString ) ->
                Parser.problem <|
                    UserReadyError <|
                        fn
                            { string = inputString
                            , tokenStartIndex = offset
                            }
            )



-- TYPE ENUMERATORS (as lists)


{-| All possible combinations of turnables, lengths and directions

Can for example be used if you ever need to list all possible turns to
a user, or if you need to select a turn at random

    import List.Nonempty

    List.Nonempty.sample allTurns

    List.Nonempty.length allTurns
    --> List.Nonempty.length allTurnables
    -->     * List.Nonempty.length allTurnLengths
    -->     * List.Nonempty.length allTurnDirections

-}
allTurns : List.Nonempty.Nonempty Turn
allTurns =
    ListM.return Turn
        |> ListM.applicative
            (ListM.fromNonemptyList allTurnables)
        |> ListM.applicative
            (ListM.fromNonemptyList allTurnLengths)
        |> ListM.applicative
            (ListM.fromNonemptyList allTurnDirections)
        |> ListM.toNonemptyList


{-| All possible turnables

Can for example be used if you ever need to list all possible
turnables to a user, or if you need to select a turnable at random

    import List.Nonempty

    List.Nonempty.sample allTurnables

    List.Nonempty.length allTurnables --> 18

-}
allTurnables : List.Nonempty.Nonempty Turnable
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
                    Just Uw

                Uw ->
                    Just Dw

                Dw ->
                    Just Rw

                Rw ->
                    Just Lw

                Lw ->
                    Just Fw

                Fw ->
                    Just Bw

                Bw ->
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

Can for example be used if you ever need to list all possible
turn lengths to a user, or if you need to select a turn length
at random

    import List.Nonempty

    List.Nonempty.sample allTurnLengths

    List.Nonempty.length allTurnLengths --> 3

-}
allTurnLengths : List.Nonempty.Nonempty TurnLength
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

Can for example be used if you ever need to list all possible
turn directions to a user, or if you need to select a turn
direction at random

    import List.Nonempty

    List.Nonempty.sample allTurnDirections

    List.Nonempty.length allTurnDirections --> 2

-}
allTurnDirections : List.Nonempty.Nonempty TurnDirection
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



-- ADVANCED


{-| You should not need this for by far most use cases.

It will let you introspect the algorithm which can be useful
in some advanced usecases, but in general you should avoid this
and just pass around the algorithm type

-}
toTurnList : Algorithm -> List Turn
toTurnList (Algorithm turnList) =
    turnList
