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
    DOT = '.'
    LPAREN = '('
    RPAREN = ')'
    SEMI = ';'
    COMMA = ','
    COLON = ':'
    # block of reserved words
    INTEGER = 'INTEGER'
    REAL = 'REAL'
    INTEGER_DIV = 'DIV '
    BEGIN = 'BEGIN'
    END = 'END'
    PROGRAM = 'PROGRAM'
    PROCEDURE = 'PROCEDURE'
    VAR = 'VAR'
    #misc
    IDENTIFIER = 'IDENTIFIER'
    ASSIGN = ':='
    EOF = 'EOF'

RESERVED_KEYWORDS = [TokenType.INTEGER, TokenType.REAL,    TokenType.INTEGER_DIV, TokenType.BEGIN,
                     TokenType.END,     TokenType.PROGRAM, TokenType.PROCEDURE,   TokenType.VAR]


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