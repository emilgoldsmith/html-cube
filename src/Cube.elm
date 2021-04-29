module Cube exposing (Cube(..), applyAlgorithm, solved, viewUBLWithLetters, viewUFRNoLetters, viewUFRWithLetters)

{-| Documentation to come

@docs Cube, applyAlgorithm, solved, viewUBLWithLetters, viewUFRNoLetters, viewUFRWithLetters

-}

import Algorithm
import Html
import Internal.Cube


{-| Placeholder
-}
type Cube
    = Cube Internal.Cube.Cube


{-| Placeholder
-}
map : (Internal.Cube.Cube -> Internal.Cube.Cube) -> Cube -> Cube
map fn (Cube cube) =
    Cube <| fn cube


{-| Placeholder
-}
applyAlgorithm : Algorithm.Algorithm -> Cube -> Cube
applyAlgorithm algorithm =
    map <| Internal.Cube.applyAlgorithm algorithm


{-| Placeholder
-}
solved : Cube
solved =
    Cube Internal.Cube.solved


{-| Placeholder
-}
viewUBLWithLetters : Int -> Cube -> Html.Html msg
viewUBLWithLetters size (Cube cube) =
    Internal.Cube.viewUBLWithLetters size cube


{-| Placeholder
-}
viewUFRNoLetters : Int -> Cube -> Html.Html msg
viewUFRNoLetters size (Cube cube) =
    Internal.Cube.viewUFRNoLetters size cube


{-| Placeholder
-}
viewUFRWithLetters : Int -> Cube -> Html.Html msg
viewUFRWithLetters size (Cube cube) =
    Internal.Cube.viewUFRWithLetters size cube
