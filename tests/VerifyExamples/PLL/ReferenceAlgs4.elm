module VerifyExamples.PLL.ReferenceAlgs4 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec4 : Test.Test
spec4 =
    Test.test "#referenceAlgs: \n\n    Algorithm.fromString \"F2 R' F' U' F' U F R F' U2 F U2 F'\"\n    --> Ok referenceAlgs.ra" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "F2 R' F' U' F' U F R F' U2 F U2 F'"
                )
                (
                Ok referenceAlgs.ra
                )