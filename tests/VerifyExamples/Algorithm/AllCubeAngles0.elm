module VerifyExamples.Algorithm.AllCubeAngles0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Algorithm exposing (..)
import List.Nonempty







spec0 : Test.Test
spec0 =
    Test.test "#allCubeAngles: \n\n    List.Nonempty.length allCubeAngles\n    --> 24" <|
        \() ->
            Expect.equal
                (
                List.Nonempty.length allCubeAngles
                )
                (
                24
                )