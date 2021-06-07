module VerifyExamples.AUF.ToAlgorithm0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import AUF exposing (..)
import Algorithm







spec0 : Test.Test
spec0 =
    Test.test "#toAlgorithm: \n\n    toAlgorithm Halfway\n    -->  Algorithm.build\n    -->    [ Algorithm.Turn\n    -->        Algorithm.U\n    -->        Algorithm.Halfway\n    -->        Algorithm.Clockwise\n    -->    ]" <|
        \() ->
            Expect.equal
                (
                toAlgorithm Halfway
                )
                (
                 Algorithm.build
                   [ Algorithm.Turn
                       Algorithm.U
                       Algorithm.Halfway
                       Algorithm.Clockwise
                   ]
                )