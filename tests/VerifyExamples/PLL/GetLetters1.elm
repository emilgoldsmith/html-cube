module VerifyExamples.PLL.GetLetters1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)







spec1 : Test.Test
spec1 =
    Test.test "#getLetters: \n\n    -- Format is always first letter capitalized and\n    -- the second one lower case if applicable\n    getLetters Ua\n    --> \"Ua\"" <|
        \() ->
            Expect.equal
                (
                -- Format is always first letter capitalized and
                -- the second one lower case if applicable
                getLetters Ua
                )
                (
                "Ua"
                )