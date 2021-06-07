module AUF exposing
    ( AUF(..), all
    , toAlgorithm, toString, fromString
    )

{-| Types and helpers to deal with Adjust U Face (AUF), which
are the moves needed to adjust the U face to the right angle
for executing your algorithms for several algorithm sets such
as OLL and PLL. See
<https://www.speedsolving.com/wiki/index.php/AUF>
for more information


# Definition and Constructors

@docs AUF, all


# Helpers

@docs toAlgorithm, toString, fromString

-}

import Algorithm exposing (Algorithm)
import List.Nonempty
import Utils.Enumerator



-- DEFINITION AND CONSTRUCTORS


{-| The 4 different AUFs. U, U', U2, and nothing.
Use these value constructors together with [all](@all)
when you need to construct in different ways.
-}
type AUF
    = None
    | Clockwise
    | Halfway
    | CounterClockwise


{-| A nonempty list containing all the possible aufs.
Could for example be used to generate a random auf

    import List.Nonempty

    -- They are all there!
    List.Nonempty.length all --> 4

    List.Nonempty.sample all

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
    case Utils.Enumerator.from None fromNone of
        [] ->
            -- This should not happen, and the length test in examples
            -- also verifies that it won't
            List.Nonempty.fromElement None

        x :: xs ->
            List.Nonempty.Nonempty x xs



-- HELPERS


{-| Get the algorithm that corresponds to the AUF

    import Algorithm

    toAlgorithm Halfway
    -->  Algorithm.build
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
            Algorithm.build
                [ Algorithm.Turn
                    Algorithm.U
                    Algorithm.OneQuarter
                    Algorithm.Clockwise
                ]

        Halfway ->
            Algorithm.build
                [ Algorithm.Turn
                    Algorithm.U
                    Algorithm.Halfway
                    Algorithm.Clockwise
                ]

        CounterClockwise ->
            Algorithm.build
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


{-| Attempts to parse an algorithmic representation of an AUF

    fromString "U'" --> Ok CounterClockwise
    fromString "" --> Ok None
    fromString "U B"
    --> Err "An AUF must be no move or a single turn of the U layer"

-}
fromString : String -> Result String AUF
fromString stringValue =
    if
        stringValue
            |> String.filter
                (\x -> x /= ' ' && x /= '\t')
            |> String.isEmpty
    then
        Ok None

    else
        stringValue
            |> Algorithm.fromString
            |> Result.map algorithmToAuf
            |> Result.map (Maybe.map Ok)
            |> Result.andThen
                (Maybe.withDefault
                    (Err
                        "An AUF must be no move or a single turn of the U layer"
                    )
                )


algorithmToAuf : Algorithm -> Maybe AUF
algorithmToAuf algorithm =
    case Algorithm.toTurnList algorithm of
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
