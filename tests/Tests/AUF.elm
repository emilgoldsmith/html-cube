module Tests.AUF exposing (fromStringTests)

import AUF exposing (AUF)
import Expect
import Fuzz
import List.Nonempty
import Test exposing (..)


fromStringTests : Test
fromStringTests =
    describe "fromString"
        [ fuzz aufFuzzer "stringified auf decodes to original value" <|
            \auf ->
                auf
                    |> AUF.toString
                    |> AUF.fromString
                    |> Expect.equal (Ok auf)
        ]


aufFuzzer : Fuzz.Fuzzer AUF
aufFuzzer =
    Fuzz.oneOf <|
        List.map Fuzz.constant (List.Nonempty.toList AUF.all)
