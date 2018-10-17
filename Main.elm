module App exposing ( main )
import Html exposing (node, li, ul, text)
import List
import Debug exposing (log)

type TOKEN = INTEGER Int | FLOAT Float | PLUS | MINUS | TIMES | DIVIDE | EXPONENTIATE | SIN | COSIN | OPENING_BRACKET | CLOSING_BRACKET

renderList name = li [] [text name]

type LETTER_TYPE = NUMBER | LETTER
letterType : Char -> LETTER_TYPE
letterType letter = if Char.isDigit letter || letter == '.' then
        NUMBER
    else
        LETTER

toToken : String -> TOKEN
toToken string =
    if string == "+" then
        PLUS
    else if string == "-" then
        MINUS
    else if string == "*" then
        TIMES
    else if string == "/" then
        DIVIDE
    else if string == "^" then
        EXPONENTIATE
    else if string == "SIN" then
        SIN
    else if string == "COSIN" then
        COSIN
    else if string == "(" then
        OPENING_BRACKET
    else if string == ")" then
        CLOSING_BRACKET
    else if String.contains "." string then
        FLOAT (Maybe.withDefault 0 (String.toFloat string))
    else
        INTEGER (Maybe.withDefault 42 (String.toInt string))

type alias TokenizerState = { unparsedLetters : List Char, numberBuffer: String, letterBuffer: String, tokens: List TOKEN }

tokenize : TokenizerState -> TokenizerState
tokenize state =
    case state.unparsedLetters of
        [] -> if state.letterBuffer /= "" then
                { state | tokens = (toToken state.letterBuffer) :: state.tokens}
            else
                { state | tokens = (toToken state.numberBuffer) :: state.tokens}
        letter :: unparsedLetters -> case letterType letter of
            NUMBER ->
                case state.letterBuffer of
                    "" -> log "NUMBER Empty buffer" tokenize {
                        state |
                         numberBuffer = state.numberBuffer ++ (String.fromChar letter),
                         unparsedLetters = unparsedLetters
                         }
                    number -> log ("NUMBER Filled ("++ state.letterBuffer ++ ") buffer") tokenize{
                        state |
                        numberBuffer = state.numberBuffer ++ (String.fromChar letter),
                        letterBuffer = "",
                        tokens = (toToken state.letterBuffer) :: state.tokens,
                        unparsedLetters = unparsedLetters
                        }
            LETTER ->
                case state.numberBuffer of
                    "" -> log "LETTER Empty buffer" tokenize {
                        state |
                        letterBuffer = state.letterBuffer ++ (String.fromChar letter),
                        unparsedLetters = unparsedLetters
                        }
                    number -> log ("LETTER Filled ("++ state.numberBuffer ++ ") buffer") tokenize {
                        state |
                        letterBuffer = state.letterBuffer ++ (String.fromChar letter),
                        numberBuffer = "",
                        tokens = (toToken (log "number buffer" state.numberBuffer)) :: state.tokens,
                        unparsedLetters = unparsedLetters
                        }

printToken : TOKEN -> String
printToken token = case token of
    INTEGER num -> "Interger(" ++ (String.fromInt num) ++ ")"
    FLOAT num -> "Float(" ++ (String.fromFloat num) ++ ")"
    PLUS -> "PLUS"
    MINUS -> "MINUS"
    TIMES -> "TIMES"
    DIVIDE -> "DIVIDE"
    EXPONENTIATE -> "EXPONENTIATE"
    SIN -> "SIN"
    COSIN -> "COSIN"
    OPENING_BRACKET -> "OPENING_BRACKET"
    CLOSING_BRACKET -> "CLOSING_BRACKET"

parse : String -> List TOKEN
parse expression = (tokenize { unparsedLetters = (String.toList expression), numberBuffer = "", letterBuffer = "", tokens = []}).tokens

main = ul [] (List.map renderList (List.map printToken (parse "2+3*5+(32^12)")))