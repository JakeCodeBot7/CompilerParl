from enum import Enum
import re

# Define expanded token types
class TokenType(Enum):
    # Literals
    INTEGER_LITERAL = 1
    FLOAT_LITERAL = 2
    COLOUR_LITERAL = 3
    BOOLEAN_LITERAL = 4

    # Identifiers
    IDENTIFIER = 5

    # Keywords
    fun = 6
    if_ = 7
    else_ = 8
    while_ = 9
    for_ = 10
    return_ = 11
    let = 12
    int_ = 13
    float_ = 14
    bool = 15
    colour = 16
    true = 17
    false = 18

    # Operators
    OP_MUL = 19
    OP_DIV = 20
    OP_AND = 21
    OP_ADD = 22
    OP_SUB = 23
    OP_OR = 24
    OP_LT = 25
    OP_GT = 26
    OP_LE = 27
    OP_GE = 28
    OP_EQ = 29
    OP_NEQ = 30
    OP_NOT = 31
    OP_CAST = 32
    OP_ASSIGN = 33
    ARROW = 34

    # Delimiters
    LPAREN = 35
    RPAREN = 36
    LBRACE = 37
    RBRACE = 38
    LBRACKET = 39
    RBRACKET = 40
    COMMA = 41
    SEMICOLON = 42
    COLON = 43

    # Built-ins / Accessors
    PRINT = 44
    DELAY = 45
    WRITE = 46
    WRITE_BOX = 47
    CLEAR = 48
    WIDTH = 49
    HEIGHT = 50
    RANDOM_INT = 51
    READ = 52

    # Misc
    COMMENT = 53
    WHITESPACE = 54
    NEWLINE = 55
    EOF = 56
    ERROR = 57

# Basic Token structure
class Token:
    def __init__(self, token_type, value):
        self.type = token_type
        self.value = value

    def __repr__(self):
        return f"TokenType.{self.type.name} '{self.value}'"

# Lookup dictionaries for token categories
keyword_map = {
    "fun": TokenType.fun,
    "if": TokenType.if_,
    "else": TokenType.else_,
    "while": TokenType.while_,
    "for": TokenType.for_,
    "return": TokenType.return_,
    "let": TokenType.let,
    "int": TokenType.int_,
    "float": TokenType.float_,
    "bool": TokenType.bool,
    "colour": TokenType.colour,
    "true": TokenType.true,
    "false": TokenType.false
}

builtin_map = {
    "__print": TokenType.PRINT,
    "__delay": TokenType.DELAY,
    "__write": TokenType.WRITE,
    "__write_box": TokenType.WRITE_BOX,
    "__clear": TokenType.CLEAR,
    "__width": TokenType.WIDTH,
    "__height": TokenType.HEIGHT,
    "__read": TokenType.READ,
    "__random_int": TokenType.RANDOM_INT
}

operator_map = {
    "*": TokenType.OP_MUL,
    "/": TokenType.OP_DIV,
    "and": TokenType.OP_AND,
    "+": TokenType.OP_ADD,
    "-": TokenType.OP_SUB,
    "or": TokenType.OP_OR,
    "<": TokenType.OP_LT,
    ">": TokenType.OP_GT,
    "<=" : TokenType.OP_LE,
    ">=" : TokenType.OP_GE,
    "==" : TokenType.OP_EQ,
    "!=" : TokenType.OP_NEQ,
    "not": TokenType.OP_NOT,
    "as": TokenType.OP_CAST,
    "=": TokenType.OP_ASSIGN,
    "->": TokenType.ARROW
}

delimiters_map = {
    "(": TokenType.LPAREN,
    ")": TokenType.RPAREN,
    "{": TokenType.LBRACE,
    "}": TokenType.RBRACE,
    "[": TokenType.LBRACKET,
    "]": TokenType.RBRACKET,
    ",": TokenType.COMMA,
    ";": TokenType.SEMICOLON,
    ":": TokenType.COLON
}

class Lexer:
    def __init__(self, code):
        self.code = code
        self.position = 0
        self.tokens = []
        self.current_token_index = 0
        self.tokens = self.tokenize()  # fill token list on creation

    def GetNextToken(self):
        if self.current_token_index < len(self.tokens):
            tok = self.tokens[self.current_token_index]
            self.current_token_index += 1
            return tok
        return Token(TokenType.EOF, "EOF")

    def get_token_type(self, lexeme):
        # Determine token category
        if lexeme in keyword_map:
            return Token(keyword_map[lexeme], lexeme)
        elif lexeme in builtin_map:
            return Token(builtin_map[lexeme], lexeme)
        elif lexeme in operator_map:
            return Token(operator_map[lexeme], lexeme)
        elif lexeme in delimiters_map:
            return Token(delimiters_map[lexeme], lexeme)
        elif re.fullmatch(r"#[0-9a-fA-F]{6}", lexeme):
            return Token(TokenType.COLOUR_LITERAL, lexeme)
        elif re.fullmatch(r"\d+", lexeme):
            return Token(TokenType.INTEGER_LITERAL, lexeme)
        elif re.fullmatch(r"\d+\.\d+", lexeme):
            return Token(TokenType.FLOAT_LITERAL, lexeme)
        else:
            return Token(TokenType.IDENTIFIER, lexeme)

    def tokenize(self):
        tokens = []
        while self.position < len(self.code):
            char = self.code[self.position]

            # Whitespace and newline
            if char.isspace():
                if char == '\n':
                    tokens.append(Token(TokenType.NEWLINE, '\n'))
                else:
                    tokens.append(Token(TokenType.WHITESPACE, char))
                self.position += 1
                continue

            # Single-line comment
            if self.code[self.position:self.position+2] == "//":
                comment = ""
                self.position += 2
                while self.position < len(self.code) and self.code[self.position] != '\n':
                    comment += self.code[self.position]
                    self.position += 1
                tokens.append(Token(TokenType.COMMENT, comment))
                continue

            # Handle arrow (multi-char op)
            if self.code[self.position:self.position+2] == "->":
                tokens.append(Token(TokenType.ARROW, "->"))
                self.position += 2
                continue

            # Identifier / keyword / builtin
            match = re.match(r"[a-zA-Z_][a-zA-Z0-9_]*", self.code[self.position:])
            if match:
                word = match.group(0)
                tokens.append(self.get_token_type(word))
                self.position += len(word)
                continue

            # Colour literal (#RRGGBB)
            match = re.match(r"#[0-9a-fA-F]{6}", self.code[self.position:])
            if match:
                tokens.append(Token(TokenType.COLOUR_LITERAL, match.group(0)))
                self.position += 7
                continue

            # Float literal
            match = re.match(r"\d+\.\d+", self.code[self.position:])
            if match:
                tokens.append(Token(TokenType.FLOAT_LITERAL, match.group(0)))
                self.position += len(match.group(0))
                continue

            # Integer literal
            match = re.match(r"\d+", self.code[self.position:])
            if match:
                tokens.append(Token(TokenType.INTEGER_LITERAL, match.group(0)))
                self.position += len(match.group(0))
                continue

            # Delimiters (e.g. braces, commas, etc.)
            if char in delimiters_map:
                tokens.append(Token(delimiters_map[char], char))
                self.position += 1
                continue

            # Single-char operators (e.g. +, -, etc.)
            if char in operator_map:
                tokens.append(Token(operator_map[char], char))
                self.position += 1
                continue

            # Anything else is an error
            tokens.append(Token(TokenType.ERROR, char))
            self.position += 1

        # End-of-file marker
        tokens.append(Token(TokenType.EOF, "EOF"))
        return tokens
