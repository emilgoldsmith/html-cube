module Cube exposing (Cube(..), applyAlgorithm, solved, viewUBLWithLetters, viewUFRWithLetters)

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
viewUBLWithLetters : List (Html.Attribute msg) -> Int -> Cube -> Html.Html msg
viewUBLWithLetters attributes size (Cube cube) =
    Internal.Cube.viewUBLWithLetters attributes size cube


{-| Placeholder
-}
viewUFRNoLetters : List (Html.Attribute msg) -> Int -> Cube -> Html.Html msg
viewUFRNoLetters attributes size (Cube cube) =
    Internal.Cube.viewUFRNoLetters attributes size cube


{-| Placeholder
-}
viewUFRWithLetters : List (Html.Attribute msg) -> Int -> Cube -> Html.Html msg
viewUFRWithLetters attributes size (Cube cube) =
    Internal.Cube.viewUFRWithLetters attributes size cube
