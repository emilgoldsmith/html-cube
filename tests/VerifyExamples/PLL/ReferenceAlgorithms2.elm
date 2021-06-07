module VerifyExamples.PLL.ReferenceAlgorithms2 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec2 : Test.Test
spec2 =
    Test.test "#referenceAlgorithms: \n\n    Algorithm.fromString \"F2 D R2 U' R2 F2 D' L2 U L2\"\n    --> Ok referenceAlgorithms.t" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "F2 D R2 U' R2 F2 D' L2 U L2"
                )
                (
                Ok referenceAlgorithms.t
                )