from enum import Enum

# Token types
#
# EOF (end-of-file) token is used to indicate that
# there is no more input left for lexical analysis
class TokenType(Enum):
    PLUS = '+'
    MINUS = '-'
    MULTIPLY = '*'
    FLOAT_DIV = '/'
    REFERNECE = '&'
    SINGLE_QUOTE = '\''
    DOUBLE_QUOTE = '\"'
    DOT = '.'
    LPAREN = '('
    RPAREN = ')'
    SEMI = ';'
    COMMA = ','
    COLON = ':'
    LOGIC_AND = '&&'
    LOGIC_OR = '||'
    BIT_XOR = '^'
    NOT = '!'
    EQUAL = '=='
    INEQUAL = '!='
    LTE = '<='
    LT = '<'
    GTE = '>='
    GT = '>'
    LEFT_ARROW = '<-'
    RIGHT_ARROW = '->'
    LEFT_BRACKET = '['
    RIGHT_BRACKET = ']'
    # block of reserved words
    INTEGER = 'INTEGER'
    REAL = 'REAL'
    BOOL = 'BOOL'
    CHAR = 'CHAR'
    STRING = 'STRING'
    VOID = 'VOID'
    INTEGER_DIV = 'DIV'
    BEGIN = 'BEGIN'
    END = 'END'
    PROGRAM = 'PROGRAM'
    PROCEDURE = 'PROCEDURE'
    IF = 'IF'
    ELSE = 'ELSE'
    WHILE = 'WHILE'
    CONTINUE = 'CONTINUE'
    BREAK = 'BREAK'
    RETURN = 'RETURN'
    VAR = 'VAR'
    TRUE = 'TRUE'
    FALSE = 'FALSE'
    ENUM = 'ENUM'
    #misc
    IDENTIFIER = 'IDENTIFIER'

    # :=, +=, -=, *=, /=, =, <-
    ASSIGN = ':='
    EOF = 'EOF'

RESERVED_KEYWORDS = [TokenType.INTEGER, TokenType.REAL,        TokenType.BOOL,        TokenType.VOID, 
                     TokenType.CHAR,    TokenType.STRING,      TokenType.INTEGER_DIV, TokenType.BEGIN, 
                     TokenType.END,     TokenType.PROGRAM,     TokenType.PROCEDURE,   TokenType.IF,
                     TokenType.ELSE,    TokenType.WHILE,       TokenType.CONTINUE,    TokenType.BREAK, 
                     TokenType.RETURN,  TokenType.VAR,         TokenType.TRUE,        TokenType.FALSE, 
                     TokenType.ENUM]

BUILTIN_TYPES = [TokenType.INTEGER, TokenType.REAL, TokenType.BOOL, TokenType.VOID, TokenType.CHAR, TokenType.STRING, TokenType.ENUM]

class Token(object):
    def __init__(self, type:str, value, lineno=None, column=None):
        self.type = type
        self.value = value
        self.lineno = lineno
        self.column = column

    def __str__(self):
        """String representation of the class instance.

        Example:
            >>> Token(TokenType.INTEGER, 7, lineno=5, column=10)
            Token(TokenType.INTEGER, 7, position=5:10)
        """
        return 'Token({type}, {value}, position={lineno}:{column})'.format(
            type=self.type,
            value=repr(self.value),
            lineno=self.lineno,
            column=self.column,
        )

    def __repr__(self):
        return self.__str__()


def Compare(token:Token, token_type:TokenType):
    return token.type == token_type.name

