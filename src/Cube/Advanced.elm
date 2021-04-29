module Cube.Advanced exposing (render)

{-| Documentation to come

@docs render

-}

import Cube exposing (Cube(..))
import Cube.Advanced.Types exposing (Rendering)
import Internal.Cube


{-| Placeholder
-}
render : Cube -> Rendering
render (Cube cube) =
    Internal.Cube.render cube
