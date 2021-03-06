module VerifyExamples.PLL.ReferenceAlgorithms14 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec14 : Test.Test
spec14 =
    Test.test "#referenceAlgorithms: \n\n    Algorithm.fromString \"D R' D2 F' D L D' F D2 R D' F' L' F\"\n    --> Ok referenceAlgorithms.e" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "D R' D2 F' D L D' F D2 R D' F' L' F"
                )
                (
                Ok referenceAlgorithms.e
                )