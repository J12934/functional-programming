module Parser exposing (Expression(..), Term, parse)

import List
import Regex


type Expression
    = None -- none indicates that there was just no expression. While Nothing indicates a malformed expression
    | Cos
    | Sin
    | ToThePowerOfX Float
    | XToThePowerOf Float


type alias Term =
    { multiplicator : Maybe Float
    , expression : Maybe Expression
    }



-- helper function to handle parse regex from string failure


parseRegex string =
    Maybe.withDefault Regex.never <|
        Regex.fromString string


numberPattern =
    "\\d+\\.{0,1}\\d*"


multiplicatorRegex =
    parseRegex ("^" ++ numberPattern)


parseMultiplicator : String -> Maybe Float
parseMultiplicator string =
    case Regex.find multiplicatorRegex string of
        [] ->
            Nothing

        match :: [] ->
            String.toFloat match.match

        match :: rest ->
            Nothing


isJustANumberRegex =
    parseRegex ("^" ++ numberPattern ++ "$")


isJustANumber : String -> Bool
isJustANumber string =
    Regex.contains isJustANumberRegex string


isCosRegex =
    parseRegex "cosx$"


isCos : String -> Bool
isCos string =
    Regex.contains isCosRegex string


isSinRegex =
    parseRegex "Sinx$"


isSin : String -> Bool
isSin string =
    Regex.contains isSinRegex string


isToThePowerOfXRegex =
    parseRegex (numberPattern ++ "\\^x$")


isJustXRegex =
    parseRegex ("^(" ++ numberPattern ++ "){0,1}(\\*){0,1}x$")


isJustX : String -> Bool
isJustX string =
    Regex.contains isJustXRegex string


isToThePowerOfX : String -> Bool
isToThePowerOfX string =
    Regex.contains isToThePowerOfXRegex string


parseNumberAfterPowerRegex =
    parseRegex ("x\\^(" ++ numberPattern ++ ")$")



-- helper function to parse the first group submatch from a string


getFloatFromFirstSubmatch : Regex.Regex -> String -> Maybe Float
getFloatFromFirstSubmatch regex string =
    case Regex.find regex string of
        [] ->
            Nothing

        match :: [] ->
            case match.submatches of
                [] ->
                    Nothing

                -- Proper case. Found a submatch
                (Just submatch) :: [] ->
                    String.toFloat submatch

                -- submatch is nothing parser failed
                Nothing :: [] ->
                    Nothing

                -- multiple submatches? Should hopefully not happen on a properly written regex.
                submatch :: rest ->
                    Nothing

        -- multiple matches? Should hopefully not happen on a properly written regex.
        match :: rest ->
            Nothing


parseNumberAfterPower : String -> Maybe Float
parseNumberAfterPower string =
    getFloatFromFirstSubmatch parseNumberAfterPowerRegex string


isXToThePowerOfRegex =
    parseRegex ("x\\^" ++ numberPattern ++ "$")


isXToThePowerOf : String -> Bool
isXToThePowerOf string =
    Regex.contains isXToThePowerOfRegex string


parseNumberBeforePowerRegex =
    parseRegex ("(" ++ numberPattern ++ ")\\^x$")


parseNumberBeforePower : String -> Maybe Float
parseNumberBeforePower string =
    getFloatFromFirstSubmatch parseNumberBeforePowerRegex string


parseExpression : String -> Maybe Expression
parseExpression string =
    if isJustANumber string then
        Just None

    else if isJustX string then
        Just (XToThePowerOf 1)

    else if isCos string then
        Just Cos

    else if isSin string then
        Just Sin

    else if isToThePowerOfX string then
        case parseNumberBeforePower string of
            Just number ->
                Just (ToThePowerOfX number)

            Nothing ->
                Nothing

    else if isXToThePowerOf string then
        case parseNumberAfterPower string of
            Just number ->
                Just (XToThePowerOf number)

            Nothing ->
                Nothing

    else
        Nothing


parseTerm : String -> Term
parseTerm string =
    { multiplicator = parseMultiplicator string
    , expression = parseExpression string
    }


parsePolynomial : String -> List String
parsePolynomial polynomial =
    polynomial
        |> String.toLower
        |> String.replace " " ""
        |> String.split "+"


parse polynomial =
    parsePolynomial polynomial
        |> List.map parseTerm
