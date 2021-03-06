module VerifyExamples.PLL.ReferenceAlgorithms7 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec7 : Test.Test
spec7 =
    Test.test "#referenceAlgorithms: \n\n    Algorithm.fromString \"B2 (L U L') B2 (R D' R D) R2\"\n    --> Ok referenceAlgorithms.jb" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "B2 (L U L') B2 (R D' R D) R2"
                )
                (
                Ok referenceAlgorithms.jb
                )