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
                , div [style "white-space" "pre"]
                    [ text (String.slice 0 errorIndex inputString)
                    , span [ style "position" "relative", style "display" "inline-block" ]
                        [ text (String.slice errorIndex (errorIndex + 1) inputString)
                        , div [ style "position" "absolute", style "top" "18px", style "right" "1px" ] [ text "^" ]
                        ]
                    , text (String.slice (errorIndex + 1) (String.length inputString) inputString)
                    ]
                ]

        _ ->
            text "error"
