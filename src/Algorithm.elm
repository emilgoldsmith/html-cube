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
    | InvalidCharacter Char
    | RepeatedTurnable String
    | InvalidTurnWouldWorkWithoutSpace
        { inputString : String
        , wrongWhitespaceStart : Int
        , wrongWhitespaceEnd : Int
        }
    | InvalidTurnApostropheWrongSideOfLength
        { inputString : String
        , errorIndex : Int
        }
    | SpansOverSeveralLines String
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


type DeadEndsStateHelper
    = NoDeadEndsYet
    | Success Int (Maybe ParsingProblem)
    | Error FromStringError


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
        parseResult =
            List.foldl
                (\deadend state ->
                    if deadend.problem == WillNeverOccur then
                        Error
                            (UnexpectedError
                                ("An unexpected type of error occured we"
                                    ++ " did not expect ever to trigger"
                                )
                            )

                    else
                        case state of
                            NoDeadEndsYet ->
                                if deadend.row /= 1 then
                                    Error
                                        (UnexpectedError
                                            ("An error occured on a non-first row when there should"
                                                ++ " only ever be one row"
                                            )
                                        )

                                else
                                    Success
                                        deadend.col
                                        (getRelevantProblem deadend)

                            Success col relevantProblem ->
                                if deadend.row /= 1 then
                                    Error
                                        (UnexpectedError
                                            ("An error occured on a non-first row when there should"
                                                ++ " only ever be one row"
                                            )
                                        )

                                else if deadend.col /= col then
                                    Error
                                        (UnexpectedError
                                            ("An error occured on a different column"
                                                ++ " when we expected all errors to always be"
                                                ++ " on the same column"
                                            )
                                        )

                                else
                                    case
                                        ( relevantProblem, getRelevantProblem deadend )
                                    of
                                        ( Nothing, x ) ->
                                            Success col x

                                        ( x, Nothing ) ->
                                            Success col x

                                        ( x, y ) ->
                                            if x == y then
                                                Success col x

                                            else
                                                Error
                                                    (UnexpectedError
                                                        ("Didn't expect several different relevant problems"
                                                            ++ " in dead ends list"
                                                        )
                                                    )

                            Error x ->
                                Error x
                )
                NoDeadEndsYet
                deadends
    in
    case parseResult of
        Error error ->
            Err error

        NoDeadEndsYet ->
            Err
                (UnexpectedError
                    ("dead end parser finished without"
                        ++ " encountering any dead ends"
                    )
                )

        Success _ Nothing ->
            Err
                (UnexpectedError
                    ("dead end parser finished without"
                        ++ " finding a relevant problem"
                    )
                )

        Success col (Just problem) ->
            let
                unexpectedString =
                    String.slice (col - 1) col inputString
            in
            if unexpectedString == "" && problem /= EmptyAlgorithmParsingProblem then
                Err
                    (UnexpectedError
                        ("dead end parser finished without"
                            ++ " finding an unexpected string"
                        )
                    )

            else
                Ok { col = col, problem = problem, unexpectedString = unexpectedString }


getRelevantProblem : Parser.DeadEnd Never ParsingProblem -> Maybe ParsingProblem
getRelevantProblem { problem } =
    case problem of
        ExpectingTurnable ->
            Just problem

        EmptyAlgorithmParsingProblem ->
            Just problem

        UnexpectedEnd ->
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

        ExpectingTurnable ->
            let
                workedWithoutWhitespace =
                    turnWouldHaveWorkedWithoutWhitespace
                        inputString
                        index
            in
            if workedWithoutWhitespace.worked then
                InvalidTurnWouldWorkWithoutSpace
                    { inputString = inputString
                    , wrongWhitespaceStart = workedWithoutWhitespace.whitespaceStart
                    , wrongWhitespaceEnd = workedWithoutWhitespace.whitespaceEnd
                    }

            else if
                turnWouldHaveWorkedWithLengthAndDirectionSwapped
                    inputString
                    index
            then
                InvalidTurnApostropheWrongSideOfLength
                    { inputString = inputString
                    , errorIndex = index - 1
                    }
                -- \u{000D} is the carriage return character \r but elm format forces it to
                -- this style. see https://github.com/avh4/elm-format/issues/376

            else if unexpectedString == "\n" || unexpectedString == "\u{000D}" then
                SpansOverSeveralLines inputString

            else if
                wasInvalidTurnLength
                    { inputString = inputString
                    , index = index
                    , unexpectedString = unexpectedString
                    }
            then
                InvalidTurnLength
                    { inputString = inputString
                    , errorIndex = index
                    , invalidLength = unexpectedString
                    }

            else
                InvalidTurnable
                    { inputString = inputString
                    , errorIndex = index
                    , invalidTurnable = String.slice index (index + 1) inputString
                    }

        UnexpectedEnd ->
            UnexpectedError "We Expected UnexpectedEnd Problems To Have Been Filtered Out"

        WillNeverOccur ->
            UnexpectedError "A problem we never expected to happen happened anyway"


type StateHelper
    = BeforeInvalidTurnable Int
    | WhitespaceBeforeNeighbour
        { invalidTurnable : String
        , whitespaceEnd : Int
        , curIndex : Int
        }
    | Done
        { invalidTurnable : String
        , neighbour : String
        , whitespaceStart : Int
        , whitespaceEnd : Int
        }


turnWouldHaveWorkedWithoutWhitespace :
    String
    -> Int
    ->
        { worked : Bool
        , whitespaceStart : Int
        , whitespaceEnd : Int
        }
turnWouldHaveWorkedWithoutWhitespace string index =
    let
        -- Note that we fold from the right
        failedTurnableToLeftNeighbour =
            String.foldr
                (\char state ->
                    case state of
                        BeforeInvalidTurnable curIndex ->
                            if curIndex > index then
                                BeforeInvalidTurnable (curIndex - 1)

                            else
                                WhitespaceBeforeNeighbour
                                    { invalidTurnable = String.fromChar char
                                    , whitespaceEnd = curIndex
                                    , curIndex = curIndex - 1
                                    }

                        WhitespaceBeforeNeighbour localState ->
                            if isWhitespace char then
                                WhitespaceBeforeNeighbour
                                    { localState
                                        | curIndex = localState.curIndex - 1
                                    }

                            else
                                Done
                                    { invalidTurnable = localState.invalidTurnable
                                    , neighbour = String.fromChar char
                                    , whitespaceStart = localState.curIndex + 1
                                    , whitespaceEnd = localState.whitespaceEnd
                                    }

                        Done localState ->
                            Done localState
                )
                (BeforeInvalidTurnable (String.length string - 1))
                string
    in
    case failedTurnableToLeftNeighbour of
        Done { invalidTurnable, neighbour, whitespaceStart, whitespaceEnd } ->
            case Parser.run algorithmParser (neighbour ++ invalidTurnable) of
                Ok _ ->
                    { worked = True
                    , whitespaceStart = whitespaceStart
                    , whitespaceEnd = whitespaceEnd
                    }

                Err _ ->
                    { worked = False
                    , whitespaceStart = -1
                    , whitespaceEnd = -1
                    }

        _ ->
            { worked = False
            , whitespaceStart = -1
            , whitespaceEnd = -1
            }


turnWouldHaveWorkedWithLengthAndDirectionSwapped :
    String
    -> Int
    -> Bool
turnWouldHaveWorkedWithLengthAndDirectionSwapped string errorIndex =
    let
        turnable =
            String.slice (errorIndex - 2) (errorIndex - 1) string

        apostrophe =
            String.slice (errorIndex - 1) errorIndex string

        length =
            String.slice errorIndex (errorIndex + 1) string
    in
    case Parser.run algorithmParser (turnable ++ length ++ apostrophe) of
        Ok _ ->
            True

        Err _ ->
            False


wasInvalidTurnLength :
    { inputString : String
    , index : Int
    , unexpectedString : String
    }
    -> Bool
wasInvalidTurnLength { inputString, index, unexpectedString } =
    if index == 0 || String.toInt unexpectedString == Nothing then
        False

    else
        let
            possibleTurnWithValidTurn =
                String.slice (index - 1) index inputString ++ "2"
        in
        case Parser.run algorithmParser possibleTurnWithValidTurn of
            -- If the turn was valid if the number was changed to 2
            -- it was indeed an invalid turn length
            Ok _ ->
                True

            -- If this didn't work either invalid turn length wasn't the only
            -- problem at least
            Err _ ->
                False



-- PARSER


type alias OurParser result =
    Parser Never ParsingProblem result


type ParsingProblem
    = ExpectingTurnable
    | EmptyAlgorithmParsingProblem
    | UnexpectedEnd
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
        -- Ignore leading whitespace
        |. Parser.chompWhile isWhitespace
        |= Parser.loop [] buildTurnListLoop
        |> Parser.andThen verifyNotEmptyParser


buildTurnListLoop : List Turn -> OurParser (Parser.Step (List Turn) (List Turn))
buildTurnListLoop currentTurnList =
    Parser.oneOf
        [ Parser.succeed (\turn -> Parser.Loop (turn :: currentTurnList))
            |. Parser.chompWhile (\c -> c == '(')
            |= turnParser
            |. Parser.chompWhile (\c -> isWhitespace c || c == ')')
        , Parser.succeed ()
            |. Parser.oneOf
                [ Parser.end UnexpectedEnd
                ]
            |> Parser.map
                (\_ -> Parser.Done (List.reverse currentTurnList))
        ]


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
                    Parser.token (Parser.Token (turnableToString turnable) ExpectingTurnable)
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
