module AUF exposing (AUF, all, fromString, toAlgorithm, toString)

{-| Types and helpers to deal with Adjust U Face (AUF), which
are the moves needed to adjust the U face to the right angle
for executing your algorithms for several algorithm sets such
as OLL and PLL. See
<https://www.speedsolving.com/wiki/index.php/AUF>
for more information
-}

import Algorithm exposing (Algorithm)
import Json.Decode
import List.Nonempty
import Utils.Enumerator


type AUF
    = None
    | Clockwise
    | Halfway
    | CounterClockwise


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


toString : AUF -> String
toString =
    toAlgorithm >> Algorithm.toString


fromString : String -> Result String AUF
fromString stringValue =
    if String.isEmpty stringValue then
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
    let
        aufAlgorithms =
            List.Nonempty.map (\auf -> ( auf, toAlgorithm auf )) all
                -- We need it as a list to filter
                |> List.Nonempty.toList

        matches =
            List.filter (Tuple.second >> (==) algorithm) aufAlgorithms
    in
    case matches of
        [ ( matchingAuf, _ ) ] ->
            Just matchingAuf

        _ ->
            Nothing