module VerifyExamples.PLL.ReferenceAlgorithms6 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec6 : Test.Test
spec6 =
    Test.test "#referenceAlgorithms: \n\n    Algorithm.fromString \"L U' R U2 L' U R' L U' R U2 L' U R'\"\n    --> Ok referenceAlgorithms.na" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "L U' R U2 L' U R' L U' R U2 L' U R'"
                )
                (
                Ok referenceAlgorithms.na
                )