module Animation exposing (main)

import Algorithm exposing (Algorithm)
import Browser
import Cube exposing (Cube)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import PLL


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- INIT


type alias Model =
    { cube : Cube
    , animationState : Cube.AnimationState
    }


init : ( Model, Cmd Msg )
init =
    ( { cube = Cube.solved, animationState = Cube.noAnimation }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AnimationMsg Cube.AnimationMsg
    | StartAnimation
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationMsg animationMsg ->
            Cube.handleAnimationMsg model.animationState animationMsg
                |> Tuple.mapBoth
                    (\newAnimationState ->
                        { model
                            | animationState = newAnimationState
                        }
                    )
                    (Cmd.map AnimationMsg)

        StartAnimation ->
            ( { model
                | animationState =
                    Cube.animateAlgorithm <|
                        PLL.getAlgorithm PLL.referenceAlgorithms PLL.Ua
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "width" "100%"
        , style "height" "100vh"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "flex-direction" "column"
        ]
        [ Cube.viewAnimatable
            { cube = model.cube
            , animationState = model.animationState
            , toMsg = AnimationMsg
            , animationDoneMsg = NoOp
            , size = 500
            }
        , button [ onClick StartAnimation ] [ text "Start Animation" ]
        ]
