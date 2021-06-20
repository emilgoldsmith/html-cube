module VerifyExamples.PLL.SolvedBy0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec0 : Test.Test
spec0 =
    Test.test "#solvedBy: \n\n    Algorithm.fromString \"U\"\n        |> Result.map (\\alg -> solvedBy alg Aa)\n    --> Ok False" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "U"
                    |> Result.map (\alg -> solvedBy alg Aa)
                )
                (
                Ok False
                )