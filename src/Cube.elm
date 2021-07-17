module Cube exposing (Cube(..), applyAlgorithm, solved, viewUBLWithLetters, viewUFRNoLetters, viewUFRWithLetters, algorithmResultsAreEquivalent, algorithmResultsAreEquivalentIndependentOfFinalRotation, AnimationState, AnimationMsg, viewAnimatable, noAnimation, animateAlgorithm, handleAnimationMsg)

{-| Documentation to come

@docs Cube, applyAlgorithm, solved, viewUBLWithLetters, viewUFRNoLetters, viewUFRWithLetters, algorithmResultsAreEquivalent, algorithmResultsAreEquivalentIndependentOfFinalRotation, AnimationState, AnimationMsg, viewAnimatable, noAnimation, animateAlgorithm, handleAnimationMsg

-}

import Algorithm exposing (Algorithm)
import Html exposing (Html)
import Internal.Cube
import List.Nonempty


{-| Placeholder
-}
type Cube
    = Cube Internal.Cube.Cube


{-| Placeholder
-}
type alias AnimationState =
    Internal.Cube.AnimationState


{-| Placeholder
-}
type alias AnimationMsg =
    Internal.Cube.AnimationMsg


{-| Placeholder
-}
viewAnimatable : { cube : Cube, size : Int, animationState : AnimationState, toMsg : AnimationMsg -> msg, animationDoneMsg : msg } -> Html msg
viewAnimatable arguments =
    let
        (Cube internalCube) =
            arguments.cube
    in
    Internal.Cube.viewAnimatable { cube = internalCube, animationState = arguments.animationState, toMsg = arguments.toMsg, animationDoneMsg = arguments.animationDoneMsg, size = arguments.size }


{-| Placeholder
-}
noAnimation : AnimationState
noAnimation =
    Internal.Cube.noAnimation


{-| Placeholder
-}
handleAnimationMsg : AnimationState -> AnimationMsg -> ( AnimationState, Cmd AnimationMsg )
handleAnimationMsg =
    Internal.Cube.handleAnimationMsg


{-| Placeholder
-}
animateAlgorithm : Algorithm -> AnimationState
animateAlgorithm =
    Internal.Cube.animateAlgorithm


{-| Placeholder
-}
map : (Internal.Cube.Cube -> Internal.Cube.Cube) -> Cube -> Cube
map fn (Cube cube) =
    Cube <| fn cube


{-| Placeholder
-}
applyAlgorithm : Algorithm -> Cube -> Cube
applyAlgorithm algorithm =
    map <| Internal.Cube.applyAlgorithm algorithm


{-| Placeholder
-}
solved : Cube
solved =
    Cube Internal.Cube.solved


{-| Placeholder
-}
viewUBLWithLetters : List (Html.Attribute msg) -> Int -> Cube -> Html msg
viewUBLWithLetters attributes size (Cube cube) =
    Internal.Cube.viewUBLWithLetters attributes size cube


{-| Placeholder
-}
viewUFRNoLetters : List (Html.Attribute msg) -> Int -> Cube -> Html msg
viewUFRNoLetters attributes size (Cube cube) =
    Internal.Cube.viewUFRNoLetters attributes size cube


{-| Placeholder
-}
viewUFRWithLetters : List (Html.Attribute msg) -> Int -> Cube -> Html msg
viewUFRWithLetters attributes size (Cube cube) =
    Internal.Cube.viewUFRWithLetters attributes size cube


{-| Placeholder
-}
algorithmResultsAreEquivalent : Algorithm -> Algorithm -> Bool
algorithmResultsAreEquivalent a b =
    solved
        |> applyAlgorithm a
        |> applyAlgorithm (Algorithm.inverse b)
        |> (==) solved


{-| Placeholder
-}
algorithmResultsAreEquivalentIndependentOfFinalRotation : Algorithm -> Algorithm -> Bool
algorithmResultsAreEquivalentIndependentOfFinalRotation a b =
    List.Nonempty.map (Algorithm.append a) Algorithm.allCubeAngles
        |> List.Nonempty.any (algorithmResultsAreEquivalent b)
