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
            let
                before =
                    String.slice 0 errorIndex inputString

                invalidOne =
                    String.slice errorIndex (errorIndex + 1) inputString

                after =
                    String.slice (errorIndex + 1) (String.length inputString) inputString
            in
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
                    [ span [ style "visibility" "hidden" ] [ text before ]
                    , text "^"
                    ]
                ]

        _ ->
            text "error"
