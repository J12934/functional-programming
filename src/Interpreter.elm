module Interpreter exposing (interpreteDerivative, interpreteFunction)

import List
import Maybe
import Parser exposing (Expression(..), Term, parse)


calcFunction : Float -> Term -> Float
calcFunction x term =
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


calcDerivative : Float -> Term -> Float
calcDerivative x { multiplicator, expression } =
    case ( multiplicator, expression ) of
        ( Nothing, Nothing ) ->
            0

        ( _, Nothing ) ->
            0

        ( _, Just None ) ->
            -- f(x)=1 -> f'(x)=0
            0

        ( Just a, Just Cos ) ->
            -- f(x)=a*cos(x) -> f'(x)=-a*sin(x)
            -a * sin x

        ( Nothing, Just Cos ) ->
            -- f(x)=cos(x) -> f'(x)=-sin(x)
            -1 * sin x

        ( Just a, Just Sin ) ->
            -- f(x)=a*sin(x) -> f'(x)=a*cos(x)
            a * cos x

        ( Nothing, Just Sin ) ->
            -- f(x)=sin(x) -> f'(x)=cos(x)
            cos x

        ( Just a, Just (XToThePowerOf b) ) ->
            -- f(x)=a*x^b -> f'(x)=(a*b)x^(b-1)
            (a * b) * (x ^ (b - 1))

        ( Nothing, Just (XToThePowerOf b) ) ->
            -- f(x)=x^b -> f'(x)=b*x^(b-1)
            b * (x ^ (b - 1))

        ( Just a, Just (ToThePowerOfX b) ) ->
            -- f(x)=a*b^x -> f'(x)=(a*x)*b^(x-1)
            (a * x) * (b ^ (x - 1))

        ( Nothing, Just (ToThePowerOfX b) ) ->
            -- f(x)=x^b -> f'(x)=b*x^(b-1)
            x * (b ^ (x - 1))


interprete : (Float -> Term -> Float) -> Float -> String -> Float
interprete func x polynomial =
    parse polynomial
        |> List.map (func x)
        |> List.sum


interpreteDerivative =
    interprete calcDerivative


interpreteFunction =
    interprete calcFunction
