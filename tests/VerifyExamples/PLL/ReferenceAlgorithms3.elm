module VerifyExamples.PLL.ReferenceAlgorithms3 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec3 : Test.Test
spec3 =
    Test.test "#referenceAlgorithms: \n\n    Algorithm.fromString \"R2 F R U R U' R' F' R U2 R' U2 R\"\n    --> Ok referenceAlgorithms.rb" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "R2 F R U R U' R' F' R U2 R' U2 R"
                )
                (
                Ok referenceAlgorithms.rb
                )