module VerifyExamples.PLL.ReferenceAlgorithms10 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec10 : Test.Test
spec10 =
    Test.test "#referenceAlgorithms: \n\n    Algorithm.fromString \"R2' D' F U' F U F' D R2 B U' B'\"\n    --> Ok referenceAlgorithms.gc" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "R2' D' F U' F U F' D R2 B U' B'"
                )
                (
                Ok referenceAlgorithms.gc
                )