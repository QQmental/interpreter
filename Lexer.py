from Token import TokenType
from Token import Token
from Error import Error

class Lexer(object):
    def __init__(self, text):
        # client string input, e.g. "3+5"
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        # current token instance
        self.current_char = self.text[self.pos]
        #current line number
        self.lineno = 1
        # current column number
        self.columnno = 1

    def error(self):
        s = "Lexer error on '{lexeme}' line: {lineno} column: {column}".format(
            lexeme=self.current_char,
            lineno=self.lineno,
            column=self.columnno,
        )
        raise Error.LexerError(message=s)


    def advance(self):
        self.pos += 1
        
        if self.pos >= len(self.text):
            self.current_char = None
        else:
            self.columnno += 1
            if self.current_char == '\n':
                self.columnno = 1
                self.lineno += 1
            self.current_char = self.text[self.pos]


    def overlook(self, dist:int):
        if self.pos + dist < len(self.text):
            return self.text[self.pos + dist]
        else:
            return None

    def check_reserved_word(self, val:str):
        val = val.upper()
        if val == TokenType.BEGIN.value:
            return Token(TokenType.BEGIN.name, val)
        elif (val == TokenType.END.value):
            return Token(TokenType.END.name, val)
        elif val == 'DIV':
            return Token(TokenType.INTEGER_DIV.name, val)
        elif val == TokenType.PROGRAM.value:
            return Token(TokenType.PROGRAM.name, val)
        elif val == TokenType.VAR.value:
            return Token(TokenType.VAR.name, val)
        elif val == TokenType.PROCEDURE.value:
            return Token(TokenType.PROCEDURE.name, val)
        elif val == TokenType.INTEGER.value:
            return Token(TokenType.INTEGER.name, val)
        elif val == TokenType.REAL.value:
            return Token(TokenType.REAL.name, val)
        else:
            return None

    def pick_identifier(self):
        val = ''
        while self.current_char != None and self.current_char.isalnum():
            val += self.current_char
            self.advance()

        maybe_token = self.check_reserved_word(val)

        if maybe_token != None:
            return maybe_token
        else:
            return Token(TokenType.IDENTIFIER.name, val)

        
    def pick_number_stream(self):
        ret = ''
        while self.current_char != None and self.current_char.isdigit():
            ret += self.current_char
            self.advance()
        return ret

    def pick_integer(self):
        val = int(0)
        while self.current_char != None and self.current_char.isdigit():
            val = 10*val + int(self.current_char) 
            self.advance()
        return Token(TokenType.INTEGER.name, val)


    def pick_number(self):
        val = 0.0
        val += self.pick_integer().value

        if self.pos < len(self.text) and self.text[self.pos] == '.':
            self.advance()
            if self.current_char != None and self.current_char.isdigit():
                floating_stream = self.pick_number_stream()
                val += float('0.' + floating_stream)       
            return Token(TokenType.REAL.name, val)
        else:
            return Token(TokenType.INTEGER.name, int(val))
    
    def skip_comment(self):
        while self.current_char != '}':
            self.advance()
        self.advance() # skip '}'

    def get_next_token(self):
        """Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        text = self.text

        while self.current_char != None:
            if self.current_char.isspace():
                self.advance()
                continue
            elif self.current_char == '{':
                self.skip_comment()
                continue
            else:
                break
            
        lineno = self.lineno
        columnno = self.columnno
        ret_wrap = lambda token : Token(token.type, token.value, lineno, columnno)    
        current_char = self.current_char

        if current_char == None:
            return ret_wrap(Token(TokenType.EOF.name, None))

        if current_char.isdigit():
            return ret_wrap(self.pick_number())

        if current_char.isalpha():
            return ret_wrap(self.pick_identifier())
        
        if current_char == '+':
            if self.overlook(1) == '=':
               self.advance()
               self.advance()
               return ret_wrap(Token(TokenType.ASSIGN.name, '+='))
            else:
                self.advance()
                return ret_wrap(Token(TokenType.PLUS.name, current_char))

        if current_char == '-':
            if self.overlook(1) == '=':
               self.advance()
               self.advance()
               return ret_wrap(Token(TokenType.ASSIGN.name, '-='))
            else:
                self.advance()
            return ret_wrap(Token(TokenType.MINUS.name, current_char))
        
        if current_char == '*':
            if self.overlook(1) == '=':
               self.advance()
               self.advance()
               return ret_wrap(Token(TokenType.ASSIGN.name, '*='))
            else:
                self.advance()
            return ret_wrap(Token(TokenType.MULTIPLY.name, current_char))

        if current_char == '/':
            if self.overlook(1) == '=':
               self.advance()
               self.advance()
               return ret_wrap(Token(TokenType.ASSIGN.name, '/='))
            else:
                self.advance()
            return ret_wrap(Token(TokenType.FLOAT_DIV.name, current_char))
        
        if current_char == '(':
            self.advance()
            return ret_wrap(Token(TokenType.LPAREN.name, current_char))
        
        if current_char == ')':
            self.advance()
            return ret_wrap(Token(TokenType.RPAREN.name, current_char))
        
        if (current_char == ':'):
            if self.overlook(1) == '=':
                self.advance()
                self.advance()
                return ret_wrap(Token(TokenType.ASSIGN.name, ':='))
            else:
                self.advance()
                return ret_wrap(Token(TokenType.COLON.name, TokenType.COLON.value))

        if (current_char == '.'):
            self.advance()
            return ret_wrap(Token(TokenType.DOT.name, TokenType.DOT.value))
        
        if (current_char == ';'):
            self.advance()
            return ret_wrap(Token(TokenType.SEMI.name, TokenType.SEMI.value))

        if (current_char == ','):
            self.advance()
            return ret_wrap(Token(TokenType.COMMA.name, TokenType.COMMA.value))
        
        self.error()