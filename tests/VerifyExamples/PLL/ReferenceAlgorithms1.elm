module VerifyExamples.PLL.ReferenceAlgorithms1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec1 : Test.Test
spec1 =
    Test.test "#referenceAlgorithms: \n\n    Algorithm.fromString \"R' U R' U' B' R' B2 U' B' U B' R B R\"\n    --> Ok referenceAlgorithms.v" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "R' U R' U' B' R' B2 U' B' U B' R B R"
                )
                (
                Ok referenceAlgorithms.v
                )