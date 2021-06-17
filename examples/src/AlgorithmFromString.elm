module AlgorithmFromString exposing (main)

import Algorithm exposing (Algorithm)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- INIT


type Model
    = Initial
    | Valid String Algorithm
    | Invalid String Algorithm.FromStringError


init : Model
init =
    Initial


getText : Model -> String
getText model =
    case model of
        Initial ->
            ""

        Valid text _ ->
            text

        Invalid text _ ->
            text


getError : Model -> Maybe Algorithm.FromStringError
getError model =
    case model of
        Initial ->
            Nothing

        Valid _ _ ->
            Nothing

        Invalid _ error ->
            Just error



-- UPDATE


type Msg
    = UpdateInputField String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInputField newText ->
            let
                algorithmResult =
                    Algorithm.fromString newText
            in
            case algorithmResult of
                Ok algorithm ->
                    Valid newText algorithm

                Err error ->
                    Invalid newText error



-- VIEW


view : Model -> Html Msg
view model =
    div []
        ([ input
            [ type_ "text"
            , value (getText model)
            , onInput UpdateInputField
            ]
            []
         ]
            ++ (getError model
                    |> Maybe.map (\error -> [ div [ style "color" "red" ] [ viewError error ] ])
                    |> Maybe.withDefault []
               )
        )


viewError : Algorithm.FromStringError -> Html Msg
viewError error =
    case error of
        Algorithm.EmptyAlgorithm ->
            text "You must input an algorithm"

        Algorithm.InvalidTurnable { inputString, errorIndex, invalidTurnable } ->
            div []
                [ div []
                    [ text "Invalid turnable "
                    , em [] [ strong [] [ text invalidTurnable ] ]
                    , text ". I expected something like U or M or x"
                    ]
                , div [ style "white-space" "pre" ]
                    [ text inputString
                    ]
                , div [ style "white-space" "pre" ]
                    -- One of several ways to ensure the arrow is indented
                    -- exactly the right amount, as different characters have
                    -- different widths, so just using spaces etc. isn't enough
                    [ span [ style "visibility" "hidden" ]
                        [ text (String.slice 0 errorIndex inputString)
                        ]
                    , text "^"
                    ]
                ]

        Algorithm.TurnWouldWorkWithoutInterruption { inputString, interruptionStart, interruptionEnd } ->
            div []
                [ div []
                    [ text
                        ("Encountered a turn that couldn't be parsed. The turn would"
                            ++ " be valid if the below interruption was removed. Was that what you intended?"
                        )
                    ]
                , div [ style "white-space" "pre" ]
                    [ text inputString
                    ]
                , div [ style "white-space" "pre" ]
                    -- One of several ways to ensure the arrow is indented
                    -- exactly the right amount, as different characters have
                    -- different widths, so just using spaces etc. isn't enough
                    [ span
                        [ style "visibility" "hidden" ]
                        [ text (String.slice 0 (interruptionStart - 1) inputString)
                        ]
                    , text "^"
                    , span
                        [ style "visibility" "hidden" ]
                        [ text
                            (String.slice
                                interruptionStart
                                (interruptionEnd - 2)
                                inputString
                            )
                        ]
                    , text "^"
                    ]
                ]

        Algorithm.ApostropheWrongSideOfLength { inputString, errorIndex } ->
            div []
                [ div []
                    [ text
                        ("Invalid turn encountered, but it would be valid if you swapped"
                            ++ " the apostrophe and the number, so maybe try that"
                        )
                    ]
                , div [ style "white-space" "pre" ]
                    [ text inputString
                    ]
                , div [ style "white-space" "pre" ]
                    -- One of several ways to ensure the arrow is indented
                    -- exactly the right amount, as different characters have
                    -- different widths, so just using spaces etc. isn't enough
                    [ span [ style "visibility" "hidden" ] [ text (String.slice 0 errorIndex inputString) ]
                    , text "^"
                    ]
                ]

        Algorithm.SpansOverSeveralLines _ ->
            div []
                [ div []
                    [ text
                        ("Algorithm is spanning over several lines,"
                            ++ " but is only allowed to be all"
                            ++ " on a single line of text"
                        )
                    ]
                ]

        Algorithm.InvalidTurnLength { inputString, errorIndex, invalidLength } ->
            div []
                [ div []
                    [ text "Invalid turn length "
                    , em [] [ strong [] [ text invalidLength ] ]
                    , text ". I only allow 2, 3 or nothing as turn lengths"
                    ]
                , div [ style "white-space" "pre" ]
                    [ text inputString
                    ]
                , div [ style "white-space" "pre" ]
                    -- One of several ways to ensure the arrow is indented
                    -- exactly the right amount, as different characters have
                    -- different widths, so just using spaces etc. isn't enough
                    [ span [ style "visibility" "hidden" ]
                        [ text (String.slice 0 errorIndex inputString)
                        ]
                    , text "^"
                    ]
                ]

        Algorithm.RepeatedTurnable { inputString, errorIndex } ->
            div []
                [ div []
                    [ text
                        ("You repeated the same turnable twice in a row."
                            ++ " Try instead of things like UU to write U2 instead"
                        )
                    ]
                , div [ style "white-space" "pre" ]
                    [ text inputString
                    ]
                , div [ style "white-space" "pre" ]
                    -- One of several ways to ensure the arrow is indented
                    -- exactly the right amount, as different characters have
                    -- different widths, so just using spaces etc. isn't enough
                    [ span [ style "visibility" "hidden" ] [ text (String.slice 0 errorIndex inputString) ]
                    , text "^"
                    ]
                ]

        Algorithm.UnclosedParenthesis { inputString, openParenthesisIndex } ->
            div []
                [ div []
                    [ text
                        "This opening parenthesis was never closed"
                    ]
                , div [ style "white-space" "pre" ]
                    [ text inputString
                    ]
                , div [ style "white-space" "pre" ]
                    -- One of several ways to ensure the arrow is indented
                    -- exactly the right amount, as different characters have
                    -- different widths, so just using spaces etc. isn't enough
                    [ span [ style "visibility" "hidden" ] [ text (String.slice 0 openParenthesisIndex inputString) ]
                    , text "^"
                    ]
                ]

        Algorithm.UnmatchedClosingParenthesis { inputString, errorIndex } ->
            div []
                [ div []
                    [ text
                        "This closing parenthesis had no matching opening parenthesis"
                    ]
                , div [ style "white-space" "pre" ]
                    [ text inputString
                    ]
                , div [ style "white-space" "pre" ]
                    -- One of several ways to ensure the arrow is indented
                    -- exactly the right amount, as different characters have
                    -- different widths, so just using spaces etc. isn't enough
                    [ span [ style "visibility" "hidden" ] [ text (String.slice 0 errorIndex inputString) ]
                    , text "^"
                    ]
                ]

        Algorithm.EmptyParentheses { inputString, errorIndex } ->
            div []
                [ div []
                    [ text
                        "These parentheses weren't enclosing any turns"
                    ]
                , div [ style "white-space" "pre" ]
                    [ text inputString
                    ]
                , div [ style "white-space" "pre" ]
                    -- One of several ways to ensure the arrow is indented
                    -- exactly the right amount, as different characters have
                    -- different widths, so just using spaces etc. isn't enough
                    [ span [ style "visibility" "hidden" ] [ text (String.slice 0 errorIndex inputString) ]
                    , text "^"
                    ]
                ]

        Algorithm.NestedParentheses { inputString, errorIndex } ->
            div []
                [ div []
                    [ text
                        ("Nested parentheses are not normally used in algorithm"
                            ++ " so we don't allowe it."
                        )
                    ]
                , div [ style "white-space" "pre" ]
                    [ text inputString
                    ]
                , div [ style "white-space" "pre" ]
                    -- One of several ways to ensure the arrow is indented
                    -- exactly the right amount, as different characters have
                    -- different widths, so just using spaces etc. isn't enough
                    [ span [ style "visibility" "hidden" ] [ text (String.slice 0 errorIndex inputString) ]
                    , text "^"
                    ]
                ]

        Algorithm.WideMoveStylesMixed { inputString, errorIndex, invalidWideMove } ->
            div []
                [ div []
                    [ text
                        ("It seems that you have mixed the `w` and lowercase wide move styles."
                            ++ " This is not allowed, so please pick one and stick to it."
                            ++ " The invalid wide move we detected was `"
                            ++ invalidWideMove
                            ++ "`."
                        )
                    ]
                , div [ style "white-space" "pre" ]
                    [ text inputString
                    ]
                , div [ style "white-space" "pre" ]
                    -- One of several ways to ensure the arrow is indented
                    -- exactly the right amount, as different characters have
                    -- different widths, so just using spaces etc. isn't enough
                    [ span [ style "visibility" "hidden" ] [ text (String.slice 0 errorIndex inputString) ]
                    , text "^"
                    ]
                ]

        Algorithm.InvalidSymbol { inputString, errorIndex, symbol } ->
            div []
                [ div []
                    [ text
                        ("A symbol, `"
                            ++ String.fromChar symbol
                            ++ "`, which we never"
                            ++ " expected to see was encountered. Remove it"
                            ++ " or replace it if it was a typo"
                        )
                    ]
                , div [ style "white-space" "pre" ]
                    [ text inputString
                    ]
                , div [ style "white-space" "pre" ]
                    -- One of several ways to ensure the arrow is indented
                    -- exactly the right amount, as different characters have
                    -- different widths, so just using spaces etc. isn't enough
                    [ span [ style "visibility" "hidden" ] [ text (String.slice 0 errorIndex inputString) ]
                    , text "^"
                    ]
                ]

        Algorithm.UnexpectedError { inputString, errorIndex } ->
            div []
                [ div []
                    [ text
                        ("Congratulations! You provided a type of error"
                            ++ " we never considered as a possibility!"
                            ++ " This sadly means you'll also have to figure"
                            ++ " out the cause by yourself though"
                        )
                    ]
                , div [ style "white-space" "pre" ]
                    [ text inputString
                    ]
                , div [ style "white-space" "pre" ]
                    -- One of several ways to ensure the arrow is indented
                    -- exactly the right amount, as different characters have
                    -- different widths, so just using spaces etc. isn't enough
                    [ span [ style "visibility" "hidden" ] [ text (String.slice 0 errorIndex inputString) ]
                    , text "^"
                    ]
                ]
