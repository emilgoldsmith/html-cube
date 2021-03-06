module VerifyExamples.PLL.ReferenceAlgorithms12 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec12 : Test.Test
spec12 =
    Test.test "#referenceAlgorithms: \n\n    Algorithm.fromString \"F2' D (R' U R' U' R) D' F2 L' U L\"\n    --> Ok referenceAlgorithms.ga" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "F2' D (R' U R' U' R) D' F2 L' U L"
                )
                (
                Ok referenceAlgorithms.ga
                )