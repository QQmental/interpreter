import Token as nToken
from Token import TokenType
from Token import Token
import Error

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
            if self.text[self.pos + dist] == "\\":  #skip line seperator
                ret = self.text[self.pos + dist + 1]
                return ret
            else:
                ret = self.text[self.pos + dist]
                return ret
        else:
            return None

    def pick_identifier(self):
        val = ''
        while self.current_char != None and self.current_char.isalnum():
            val += self.current_char
            self.advance()

        maybe_token = nToken.Create_reserved_keyword_token(val)

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
        # skip "/*"
        self.advance()  
        self.advance()

        while self.current_char != None \
            and not (self.current_char == '*' and self.overlook(1) == '/') \
            and not (self.current_char == '/' and self.overlook(1) == '*'):
            self.advance()



        """
        while self.current_char != '}' \
              and self.current_char != None \
              and self.current_char != '{':
            self.advance() """

        if not (self.current_char == '*' and self.overlook(1) == '/'):
            self.error()

        """
        if self.current_char != '}':
            self.error() """

        #self.advance() # skip '}'

        # skip "*/"
        self.advance()  
        self.advance()


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
            elif self.current_char == '/' and self.overlook(1) == '*':
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
        
        if current_char == '\"':
            str_val = ""
            self.advance()
            while self.current_char != '\"':
                str_val += self.current_char
                self.advance()
            self.advance() #skip '\"'
            return ret_wrap(Token(TokenType.STRING.name, str_val))

        if current_char == '\'':
            char_val = '\0'
            self.advance()
            if self.current_char != '\'':
                char_val = self.current_char
                self.advance()
            if self.current_char != '\'':
                self.error()
            self.advance() #skip '\''
            return ret_wrap(Token(TokenType.CHAR.name, char_val))
        

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
            elif self.overlook(1) == '>':
                self.advance()
                self.advance()                
                return ret_wrap(Token(TokenType.RIGHT_ARROW.name, '->'))
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

        if current_char == '|':
            if self.overlook(1) == '|':
                self.advance()
                self.advance()
                return ret_wrap(Token(TokenType.LOGIC_OR.name, '||'))
            else:
                self.error()

        if current_char == '^':
            return ret_wrap(Token(TokenType.BIT_XOR.name, '^'))


        if (current_char == '!'):
            if (self.overlook(1) == '='):
                self.advance()
                self.advance()
                return ret_wrap(Token(TokenType.INEQUAL.name, '!='))
            else:
                self.advance()
                return ret_wrap(Token(TokenType.NOT.name, '!'))
        
        if (current_char == '>'):
            if (self.overlook(1) == '='):
                self.advance()
                self.advance()
                return ret_wrap(Token(TokenType.GTE.name, '>='))
            else:
                self.advance()
                return ret_wrap(Token(TokenType.GT.name, '>'))        

        if (current_char == '<'):
            if (self.overlook(1) == '='):
                self.advance()
                self.advance()
                return ret_wrap(Token(TokenType.LTE.name, '<='))
            elif self.overlook(1) == '-':
                self.advance()
                self.advance()
                return ret_wrap(Token(TokenType.ASSIGN.name, '<-'))                
            else:
                self.advance()
                return ret_wrap(Token(TokenType.LT.name, '<')) 

        if (current_char == '='):
            if self.overlook(1) == '=':
                self.advance()
                self.advance()
                return ret_wrap(Token(TokenType.EQUAL.name, '=='))
            else:
                self.advance()
                return ret_wrap(Token(TokenType.ASSIGN.name, '='))

        if current_char == '&':
            if self.overlook(1) == '&':
                self.advance()
                self.advance()
                return ret_wrap(Token(TokenType.LOGIC_AND.name, '&&'))
            else:
                self.advance()
                return ret_wrap(Token(TokenType.REFERNECE.name, '&'))

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
        
        if (current_char == '['):
            self.advance()
            return ret_wrap(Token(TokenType.LEFT_BRACKET.name, TokenType.LEFT_BRACKET.value))

        if (current_char == ']'):
            self.advance()
            return ret_wrap(Token(TokenType.RIGHT_BRACKET.name, TokenType.RIGHT_BRACKET.value))

        self.error()
    
    def overlook_token(self, next_n_token):
        pos = self.pos
        lineno = self.lineno
        columnno = self.columnno

        while next_n_token > 0:
            self.get_next_token()
            next_n_token -= 1
        
        ret = self.get_next_token()
        
        self.pos, self.lineno, self.columnno = pos, lineno, columnno
        self.current_char = self.text[self.pos]

        return ret


