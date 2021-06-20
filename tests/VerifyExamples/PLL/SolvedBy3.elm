module VerifyExamples.PLL.SolvedBy3 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec3 : Test.Test
spec3 =
    Test.test "#solvedBy: \n\n    Algorithm.fromString \"(x) R' U R' D2 R U' R' D2 R2 (x')\"\n        |> Result.map (\\alg -> solvedBy alg Aa)\n    --> Ok True" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "(x) R' U R' D2 R U' R' D2 R2 (x')"
                    |> Result.map (\alg -> solvedBy alg Aa)
                )
                (
                Ok True
                )