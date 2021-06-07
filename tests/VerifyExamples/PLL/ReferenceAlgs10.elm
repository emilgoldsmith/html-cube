module VerifyExamples.PLL.ReferenceAlgs10 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec10 : Test.Test
spec10 =
    Test.test "#referenceAlgs: \n\n    Algorithm.fromString \"R2' D' F U' F U F' D R2 B U' B'\"\n    --> Ok referenceAlgs.gc" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "R2' D' F U' F U F' D R2 B U' B'"
                )
                (
                Ok referenceAlgs.gc
                )