module Utils.Enumerator exposing (Order, from)

import List.Nonempty



-- Type enumerator
-- Modified from https://discourse.elm-lang.org/t/enumerate-function-for-non-infinite-custom-types-proposal/2636/7


type alias Order a =
    a -> Maybe a


from : a -> Order a -> List.Nonempty.Nonempty a
from current toNext =
    case toNext current of
        Just next ->
            List.Nonempty.cons current <| from next toNext

        Nothing ->
            List.Nonempty.singleton current
