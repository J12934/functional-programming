module Interpreter exposing (interprete)

import List
import Maybe
import Parser exposing (Expression(..), Term, parse)


calc : Float -> Term -> Float
calc x term =
    let
        multi =
            Maybe.withDefault 1 term.multiplicator
    in
    case term.expression of
        Just None ->
            multi

        Just Cos ->
            multi * cos x

        Just Sin ->
            multi * sin x

        Just (XToThePowerOf num) ->
            multi * x ^ num

        Just (ToThePowerOfX num) ->
            multi * num ^ x

        Nothing ->
            0


interprete : Float -> String -> Float
interprete x polynomial =
    parse polynomial
        |> List.map (calc x)
        |> List.sum
