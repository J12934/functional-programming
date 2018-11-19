module App exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, type_, value, action)
import Html.Events exposing (onClick, onInput, onSubmit)
import List
import Dict exposing (Dict)
import Maybe

type alias Model =
    {  notes : Dict String String,
        draft: String,
        counter: Int
    }

init =
    { notes = Dict.empty,
      draft = "",
      counter = 0
    }

type alias Note =
    {
    text : String,
    id : String
    }

type Action
    = ADD_NOTE
    | DRAFT_CHANGED String
    | REMOVE_NOTE String


update : Action -> Model -> Model
update action model =
    case action of
        ADD_NOTE ->
            let
                newCount = model.counter + 1
            in
                { model
                    | notes = Dict.update (String.fromInt newCount) (\_ -> Just model.draft) model.notes,
                    draft = "",
                    counter = newCount
                }
        DRAFT_CHANGED newDraft ->
            { model
                | draft = newDraft
            }

        REMOVE_NOTE key ->
            { model
                | notes = Dict.update key (\_ -> Nothing) model.notes
            }

view model =
    div [] [
        ul [] 
           (List.map (\(key, note) -> li [] [
               text note,
               button [onClick (REMOVE_NOTE key)] [ text "x" ]
            ]) (Dict.toList model.notes))
        ,
        form
            [ onSubmit ADD_NOTE, action "javascript:void(0);" ]
            [
                label [] [ text "New Note" ],
                input [ type_ "text", onInput DRAFT_CHANGED, value model.draft ] [],
                button [ type_ "submit" ] [ text "Create Note"]
            ]
    ]
    

main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
