module AUF exposing
    ( AUF(..), all
    , toAlgorithm, toString, FromStringError(..), fromString
    )

{-| Types and helpers to deal with Adjust U Face (AUF), which
are the moves needed either to adjust the U face to the right
angle for executing your algorithms for several algorithm sets
such as OLL and PLL, or the last move needed to solve the cube
for example after PLL. See
<https://www.speedsolving.com/wiki/index.php/AUF>
for more information


# Definition and Constructors

@docs AUF, all


# Helpers

@docs toAlgorithm, toString, FromStringError, fromString

-}

import Algorithm exposing (Algorithm)
import List.Nonempty
import Utils.Enumerator



-- DEFINITION AND CONSTRUCTORS


{-| The 4 different AUFs. U, U', U2, and nothing.
Use these value constructors together with [@all](#all)
when you need to construct in different ways.
-}
type AUF
    = None
    | Clockwise
    | Halfway
    | CounterClockwise


{-| A nonempty list containing all the possible aufs.
Could for example be used to generate a random auf or
generate all the possible versions of an algorithm

    import List.Nonempty

    -- They are all there!
    List.Nonempty.length all --> 4

    -- Generate a random one
    List.Nonempty.sample all

    import PLL

    -- Get all versions of a Y perm
    all
        |> List.Nonempty.map (Tuple.pair PLL.Y)
        |> List.Nonempty.map (\(pll, postAuf) ->
            List.Nonempty.map
                (\preAuf -> (preAuf, pll, postAuf))
                all
        )

-}
all : List.Nonempty.Nonempty AUF
all =
    let
        fromNone auf =
            case auf of
                None ->
                    Just Clockwise

                Clockwise ->
                    Just Halfway

                Halfway ->
                    Just CounterClockwise

                CounterClockwise ->
                    Nothing
    in
    Utils.Enumerator.from None fromNone



-- HELPERS


{-| Get the algorithm that corresponds to the AUF

    import Algorithm

    toAlgorithm Halfway
    -->  Algorithm.fromTurnList
    -->    [ Algorithm.Turn
    -->        Algorithm.U
    -->        Algorithm.Halfway
    -->        Algorithm.Clockwise
    -->    ]

-}
toAlgorithm : AUF -> Algorithm
toAlgorithm auf =
    case auf of
        None ->
            Algorithm.empty

        Clockwise ->
            Algorithm.fromTurnList
                [ Algorithm.Turn
                    Algorithm.U
                    Algorithm.OneQuarter
                    Algorithm.Clockwise
                ]

        Halfway ->
            Algorithm.fromTurnList
                [ Algorithm.Turn
                    Algorithm.U
                    Algorithm.Halfway
                    Algorithm.Clockwise
                ]

        CounterClockwise ->
            Algorithm.fromTurnList
                [ Algorithm.Turn
                    Algorithm.U
                    Algorithm.OneQuarter
                    Algorithm.CounterClockwise
                ]


{-| Get an algorithm string representation of the AUF

    toString Halfway --> "U2"

-}
toString : AUF -> String
toString =
    toAlgorithm >> Algorithm.toString


{-| Explains an issue that occurred while parsing an AUF

**InvalidAUFAlgorithm**: The algorithm was parsed correctly but was not
either an empty move or a turn of the U face

**AlgorithmParsingProblem**: The string was not a valid algorithm string
and the contained [Algorithm.FromStringError](Algorithm#FromStringError)
has the problem with the string

-}
type FromStringError
    = InvalidAUFAlgorithm
    | AlgorithmParsingProblem Algorithm.FromStringError


{-| Attempts to parse an algorithmic representation of an AUF

    fromString "U'" --> Ok CounterClockwise

    fromString "" --> Ok None

    fromString "U B"
    --> Err InvalidAUFAlgorithm

-}
fromString : String -> Result FromStringError AUF
fromString stringValue =
    if
        -- Algorithm.fromString doesn't accept empty strings
        -- at the time of writing because it doesn't make sense
        -- from a user perspective. So we do handle that case here
        -- as AUFs can indeed be an empty string
        stringValue
            |> String.filter
                (\x -> x /= ' ' && x /= '\t')
            |> String.isEmpty
    then
        Ok None

    else
        stringValue
            |> Algorithm.fromString
            |> Result.mapError AlgorithmParsingProblem
            |> Result.map algorithmToAuf
            |> Result.andThen
                (Result.fromMaybe InvalidAUFAlgorithm)


algorithmToAuf : Algorithm -> Maybe AUF
algorithmToAuf algorithm =
    case Algorithm.toTurnList algorithm of
        [] ->
            Just None

        [ Algorithm.Turn Algorithm.U length direction ] ->
            case ( length, direction ) of
                ( Algorithm.OneQuarter, Algorithm.Clockwise ) ->
                    Just Clockwise

                ( Algorithm.Halfway, Algorithm.Clockwise ) ->
                    Just Halfway

                ( Algorithm.ThreeQuarters, Algorithm.Clockwise ) ->
                    Just CounterClockwise

                ( Algorithm.OneQuarter, Algorithm.CounterClockwise ) ->
                    Just CounterClockwise

                ( Algorithm.Halfway, Algorithm.CounterClockwise ) ->
                    Just Halfway

                ( Algorithm.ThreeQuarters, Algorithm.CounterClockwise ) ->
                    Just Clockwise

        _ ->
            Nothing
