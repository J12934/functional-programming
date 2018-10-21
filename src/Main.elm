module App exposing (main)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Interpreter
import List
import Maybe


term =
    "cosx"


num =
    String.fromFloat (Interpreter.interprete 2 "2*x^2+2*x^2")


type alias Model =
    { f : String
    , x : Float
    , f_y : Float
    }


init =
    { f = ""
    , x = 0
    , f_y = 0
    }


type Action
    = FUNCTION_CHANGED String
    | X_CHANGED String


update : Action -> Model -> Model
update action model =
    case action of
        FUNCTION_CHANGED func ->
            { model
                | f = func
                , f_y = Interpreter.interprete model.x func
            }

        X_CHANGED xString ->
            let
                xVal =
                    log "X Changed to:" (Maybe.withDefault 1 (String.toFloat xString))
            in
            { model
                | x = xVal
                , f_y = Interpreter.interprete xVal model.f
            }


view model =
    div []
        [ div []
            [ label [] [ text "function" ]
            , input [ type_ "text", onInput FUNCTION_CHANGED, value model.f ] []
            ]
        , div []
            [ label [] [ text "x" ]
            , input [ type_ "number", onInput X_CHANGED, value (String.fromFloat model.x) ] []
            ]
        , div [] [ text (String.fromFloat model.f_y) ]
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
