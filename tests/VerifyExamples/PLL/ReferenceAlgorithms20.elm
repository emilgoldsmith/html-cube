module VerifyExamples.PLL.ReferenceAlgorithms20 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec20 : Test.Test
spec20 =
    Test.test "#referenceAlgorithms: \n\n    Algorithm.fromString \"R2 U2 R U2 R2 U2 R2 U2 R U2 R2\"\n    --> Ok referenceAlgorithms.h" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "R2 U2 R U2 R2 U2 R2 U2 R U2 R2"
                )
                (
                Ok referenceAlgorithms.h
                )