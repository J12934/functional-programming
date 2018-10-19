module App exposing ( main )
import Html exposing (node, li, ul, text)
import List
import Debug exposing (log)

type TOKEN = INTEGER Int | FLOAT Float | PLUS | MINUS | TIMES | DIVIDE | EXPONENTIATE | SIN | COSIN | OPENING_BRACKET | CLOSING_BRACKET

renderList name = li [] [text name]

toToken : String -> TOKEN
toToken string = case string of
    "+" -> PLUS
    "-" -> MINUS
    "*" -> TIMES
    "/" -> DIVIDE
    "^" -> EXPONENTIATE
    "SIN" -> SIN
    "COSIN" -> COSIN
    "(" -> OPENING_BRACKET
    ")" -> CLOSING_BRACKET
    number -> if String.contains "." number then
            FLOAT (Maybe.withDefault 42 (String.toFloat string))
        else
            INTEGER (Maybe.withDefault 42 (String.toInt string))

type alias TokenizerState = { unparsedLetters : List Char, bufferType: BufferType, buffer: String, tokens: List TOKEN }

type BufferType = LETTER_BUFFER | NUMBER_BUFFER | BRACKET_BUFFER | EMPTY_BUFFER

bufferTypeToBe : Char -> BufferType
bufferTypeToBe letter = if letter == '(' || letter == ')' then
        BRACKET_BUFFER
    else if Char.isDigit letter || letter == '.' then
        NUMBER_BUFFER
    else
        LETTER_BUFFER

tokenize : TokenizerState -> TokenizerState
tokenize state =
    case state.unparsedLetters of
        [] -> { state | tokens = (List.append state.tokens [(toToken state.buffer)])}
        letter :: unparsedLetters ->
            if state.bufferType /= (bufferTypeToBe letter) then
                -- different buffer type. Flush buffer to Token and create new buffer.
                tokenize {
                    state |
                    buffer = (String.fromChar letter),
                    bufferType = (bufferTypeToBe letter),
                    tokens = (List.append state.tokens [(toToken state.buffer)]),
                    unparsedLetters = unparsedLetters
                    }
            else
                -- empty or same buffer. Should create a new buffer add to buffer
                tokenize {
                    state |
                    buffer = (String.fromChar letter),
                    bufferType = (bufferTypeToBe letter),
                    unparsedLetters = unparsedLetters
                    }

printToken : TOKEN -> String
printToken token = case token of
    INTEGER num -> "Interger(" ++ (String.fromInt num) ++ ")"
    FLOAT num -> "Float(" ++ (String.fromFloat num) ++ ")"
    PLUS -> "+"
    MINUS -> "MINUS"
    TIMES -> "TIMES"
    DIVIDE -> "DIVIDE"
    EXPONENTIATE -> "EXPONENTIATE"
    SIN -> "SIN"
    COSIN -> "COSIN"
    OPENING_BRACKET -> "OPENING_BRACKET"
    CLOSING_BRACKET -> "CLOSING_BRACKET"

parse : String -> List TOKEN
parse expression = (tokenize { unparsedLetters = (String.toList expression), buffer = "", bufferType = EMPTY_BUFFER, tokens = []}).tokens

main = ul [] (List.map renderList (List.map printToken (parse "2+3*5+(32^12)")))