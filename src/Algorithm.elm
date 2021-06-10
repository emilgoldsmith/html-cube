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
    | InvalidTurnLength String
    | InvalidCharacter Char
    | RepeatedTurnable String
    | UnexpectedSpace
    | ParserCrashed String


{-| Placeholder
-}
fromString : String -> Result FromStringError Algorithm
fromString string =
    Parser.run algParser string
        |> Result.mapError (parserErrorToFromStringError string)


parserErrorToFromStringError :
    String
    -> List (Parser.DeadEnd Never ParsingProblem)
    -> FromStringError
parserErrorToFromStringError string deadends =
    case deadends of
        [] ->
            ParserCrashed "Unexpected empty deadends at an error"

        { problem, col, row } :: _ ->
            if row /= 1 then
                ParserCrashed "An error occurred not at row 1, when we only allow a single row"

            else
                case problem of
                    EmptyAlgorithmParsingProblem ->
                        EmptyAlgorithm

                    _ ->
                        InvalidTurnable
                            { inputString = string
                            , errorIndex = col - 1
                            , invalidTurnable = String.slice (col - 1) col string
                            }



-- PARSER


{-| Placeholder
-}
type ParsingProblem
    = ExpectingTurnable
    | ExpectingNumQuarterTurns
    | ExpectingTurnDirection
    | UnexpectedCharacter
    | EmptyAlgorithmParsingProblem


{-| Placeholder
-}
algParser : Parser Never ParsingProblem Algorithm
algParser =
    let
        looper currentAlgorithm =
            Parser.succeed identity
                |. Parser.chompWhile isWhiteSpace
                |= Parser.oneOf
                    [ Parser.succeed (\turn -> Parser.Loop (turn :: currentAlgorithm))
                        |. Parser.chompWhile (\c -> c == '(')
                        |= turnParser
                        |. Parser.chompWhile (\c -> isWhiteSpace c || c == ')')
                    , Parser.succeed ()
                        |. Parser.end UnexpectedCharacter
                        |> Parser.map
                            (\_ -> Parser.Done (List.reverse currentAlgorithm))
                    ]

        turnParser =
            Parser.succeed Turn
                |= turnableParser
                |= turnLengthParser
                |= directionParser

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

        turnLengthParser =
            Parser.oneOf
                [ Parser.map (\_ -> Halfway) <| Parser.token (Parser.Token "2" ExpectingNumQuarterTurns)
                , Parser.map (\_ -> ThreeQuarters) <| Parser.token (Parser.Token "3" ExpectingNumQuarterTurns)
                , Parser.map (\_ -> OneQuarter) <| Parser.token (Parser.Token "" ExpectingNumQuarterTurns)
                ]

        directionParser =
            Parser.oneOf
                [ Parser.map (\_ -> CounterClockwise) <| Parser.token (Parser.Token "'" ExpectingTurnDirection)
                , Parser.map (\_ -> Clockwise) <| Parser.token (Parser.Token "" ExpectingTurnDirection)
                ]

        verifyNotEmpty (Algorithm turnList) =
            case List.length turnList of
                0 ->
                    Parser.problem EmptyAlgorithmParsingProblem

                _ ->
                    Parser.succeed (Algorithm turnList)
    in
    Parser.succeed Algorithm |= Parser.loop [] looper |> Parser.andThen verifyNotEmpty


isWhiteSpace : Char -> Bool
isWhiteSpace c =
    c == ' ' || c == '\t'



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
