module Monads.ListM exposing (ListM, applicative, fromNonemptyList, return, toNonemptyList)

import List.Nonempty


type ListM a
    = ListM (List.Nonempty.Nonempty a)


return : a -> ListM a
return =
    List.Nonempty.singleton >> ListM


applicative : ListM a -> ListM (a -> b) -> ListM b
applicative (ListM list) (ListM functions) =
    ListM (List.Nonempty.concatMap (\fn -> List.Nonempty.map fn list) functions)


toNonemptyList : ListM a -> List.Nonempty.Nonempty a
toNonemptyList (ListM list) =
    list


fromNonemptyList : List.Nonempty.Nonempty a -> ListM a
fromNonemptyList =
    ListM
