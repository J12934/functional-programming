module App exposing (main)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Interpreter
import List
import Maybe


type alias Model =
    { f : String
    , x : Float
    , f_y : Float
    }


demoF =
    "12+x^3"


demoX =
    5


init =
    { f = demoF
    , x = demoX
    , f_y = Interpreter.interprete demoX demoF
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
    div [ class "container" ]
        [ div [ class "result" ] [ text ("f(" ++ String.fromFloat model.x ++ ")=" ++ String.fromFloat model.f_y) ]
        , div [ class "result" ] [ text ("f'(" ++ String.fromFloat model.x ++ ")=?") ]
        , div [ class "input" ]
            [ label [] [ text "f(x)=" ]
            , input [ type_ "text", onInput FUNCTION_CHANGED, value model.f ] []
            ]
        , div [ class "input" ]
            [ label [] [ text "x=" ]
            , input [ type_ "number", onInput X_CHANGED, value (String.fromFloat model.x) ] []
            ]
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
