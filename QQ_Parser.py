import Error
from Error import ErrorCode
from Error import Error
from Token import TokenType
from Token import Token
from Lexer import Lexer
import AST

class Parser(object):
    program_name = ''
    def __init__(self, lexer:Lexer):
        # client string input, e.g. "3+5"
        self.lexer = lexer

        # current token instance
        self.current_token = self.lexer.get_next_token()

    def error(self, error_code, token):
        raise Error.ParserError(
            error_code=error_code,
            token=token,
            message=f'{error_code.value} -> {token}',
        )

    def eat(self, token_types):
        # compare the current token type with the passed token
        # type and if they match then "eat" the current token,
        # otherwise raise an exception.
        if  token_types.name == self.current_token.type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )

    def variable(self):
        """
        variable : ID
        """
        node = AST.Var(self.current_token)
        self.eat(TokenType.IDENTIFIER)
        return node

    def L_variable(self):
        """
        L_variable : ID
        """
        node = AST.L_Var(self.current_token)
        self.eat(TokenType.IDENTIFIER)
        return node

    def R_variable(self):
        """
        R_variable : ID
        """
        node = AST.R_Var(self.current_token)
        self.eat(TokenType.IDENTIFIER)
        return node

    def factor(self):
        """integer | real | (PLUS/MINUS)factor | (expr) | R_variable"""
        token = self.current_token
        if token.type == TokenType.INTEGER.name:
            self.eat(TokenType.INTEGER)
            return AST.Num(token)
        elif token.type == TokenType.REAL.name:
            self.eat(TokenType.REAL)
            return AST.Num(token)
        elif token.type == TokenType.PLUS.name:
            self.eat(TokenType.PLUS)
            return AST.UnaryOp(token, self.factor())
        elif token.type == TokenType.MINUS.name:
            self.eat(TokenType.MINUS)
            return AST.UnaryOp(token, self.factor())   
        elif token.type == TokenType.LPAREN.name:
            self.eat(TokenType.LPAREN)
            node = self.expr()
            self.eat(TokenType.RPAREN)
            return node
        else:
            return self.R_variable()

    
    def term(self):
        """term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*"""
        node = self.factor()

        while self.current_token.type in (TokenType.MULTIPLY.name, TokenType.INTEGER_DIV.name, TokenType.FLOAT_DIV.name):
            token = self.current_token
            if token.type == TokenType.MULTIPLY.name:
                self.eat(TokenType.MULTIPLY)
                node = AST.BinOp(node, token, self.factor())
            elif token.type == TokenType.INTEGER_DIV.name:
                self.eat(TokenType.INTEGER_DIV)
                node = AST.BinOp(node, token, self.factor())
            elif token.type == TokenType.FLOAT_DIV.name:
                self.eat(TokenType.FLOAT_DIV)
                node = AST.BinOp(node, token, self.factor())

        return node

    def expr(self):
        """ expr   : term ((PLUS | MINUS) term)*
            term   : factor ((MUL | DIV) factor)*
            factor : INTEGER | LPAREN expr RPAREN
        """
        node = self.term()
        while self.current_token.type in (TokenType.PLUS.name, TokenType.MINUS.name):
            token = self.current_token

            if token.type == TokenType.PLUS.name:
                self.eat(TokenType.PLUS)
            elif token.type == TokenType.MINUS.name:
                self.eat(TokenType.MINUS)
            node = AST.BinOp(node, token, self.term())

        return node

    def assignment_statement(self):
        """
        assignment_statement : L_variable ASSIGN expr
        """
        left = self.L_variable()
        token = self.current_token
        self.eat(TokenType.ASSIGN)
        right = self.expr()
        node = AST.Assign(left, token, right)
        return node

    def empty(self):
        """An empty production"""
        return AST.NoOp()

    def statement(self):
        """
        statement : compound_statement
                  | proccall_statement
                  | assignment_statement
                  | empty
        """
        if self.current_token.type == TokenType.BEGIN.name:
            node = self.compound_statement()
        elif self.current_token.type == TokenType.IDENTIFIER.name \
             and self.lexer.current_char == TokenType.LPAREN.value:
            node = self.proc_call()
        elif self.current_token.type == TokenType.IDENTIFIER.name:
            node = self.assignment_statement()
        else:
            node = self.empty()
        return node

    def statement_list(self):
        """
        statement_list : statement
                       | statement SEMI statement_list
        """
        node = self.statement()

        results = [node]

        while self.current_token.type == TokenType.SEMI.name:
            self.eat(TokenType.SEMI)
            results.append(self.statement())

        if self.current_token.type == TokenType.IDENTIFIER.name:
            self.error()

        return results

    def compound_statement(self):
        """
        compound_statement: BEGIN statement_list END
        """
        self.eat(TokenType.BEGIN)
        nodes = self.statement_list()
        self.eat(TokenType.END)

        root = AST.Compound()
        for node in nodes:
            root.children.append(node)
        return root

    def type_spec(self)->AST.Type:
        token = self.current_token
        type_name = self.current_token.value.upper()
        
        if type_name == TokenType.INTEGER.value:
            self.eat(TokenType.INTEGER)
            return AST.Type(token)
        elif type_name == TokenType.REAL.value:
            self.eat(TokenType.REAL)
            return AST.Type(token)
        elif self.current_token.type != TokenType.IDENTIFIER.name:
            self.error()
        else:
            return AST.Type(token)

    def variable_declaration(self):
        """variable_declaration : ID (COMMA ID)* COLON type_spec"""
        var_list = [AST.Var(self.current_token)]  # first ID

        self.eat(TokenType.IDENTIFIER)
        while self.current_token.type == TokenType.COMMA.name:
            self.eat(TokenType.COMMA)
            var_list.append(AST.Var(self.current_token))
            self.eat(TokenType.IDENTIFIER)
        self.eat(TokenType.COLON)
        t = self.type_spec()
    
        return AST.VARs_decl(var_list, t)

    def formal_parameters(self):
        """ formal_parameters : ID (COMMA ID)* COLON type_spec """
        token_list = [self.current_token]  # first ID
        self.eat(TokenType.IDENTIFIER)
        while self.current_token.type == TokenType.COMMA.name:
            self.eat(TokenType.COMMA)
            token_list.append(self.current_token)
            self.eat(TokenType.IDENTIFIER)
        self.eat(TokenType.COLON)
        t = self.type_spec()
        
        param_list = []
        for param_token in token_list:
            param_node = AST.Param(AST.Var(param_token), t)
            param_list.append(param_node)

        return param_list

    def formal_parameter_list(self):
        """ formal_parameter_list : formal_parameters
                                | formal_parameters SEMI formal_parameter_list
        """
        param_list = self.formal_parameters()
        while self.current_token.type == TokenType.SEMI.name:
            self.eat(TokenType.SEMI)
            param_list.extend(self.formal_parameters())

        return param_list


    def declarations(self):
        declarations = []
        while self.current_token.type == TokenType.VAR.name:
            self.eat(TokenType.VAR)
            while self.current_token.type == TokenType.IDENTIFIER.name:
                var_decl = self.variable_declaration()
                declarations.append(var_decl)
                self.eat(TokenType.SEMI)
        declarations.extend(self.procedure_declarations())
        return AST.Declarations(declarations)


    def procedure_declarations(self):
        proc_declarations = []
        while self.current_token.type == TokenType.PROCEDURE.name:
            self.eat(TokenType.PROCEDURE)
            procedure_name = self.variable()
            
            param_list = []
            if self.current_token.type == TokenType.LPAREN.name:# parse parameters
                self.eat(TokenType.LPAREN)
                if self.current_token.type != TokenType.RPAREN.name:
                    param_list = self.formal_parameter_list()
                self.eat(TokenType.RPAREN)                  # complete parsing parameters
            
            self.eat(TokenType.SEMI)
            block_node = self.block()
            self.eat(TokenType.SEMI)
            proc_declarations.append(AST.ProcedureDecl(procedure_name, param_list, block_node))
        return proc_declarations

    def proc_call(self):
        """proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN"""
        token = self.current_token

        proc_name = self.current_token.value
        self.eat(TokenType.IDENTIFIER)
        self.eat(TokenType.LPAREN)
        actual_params = []
        while self.current_token != TokenType.RPAREN.value:
            actual_params.append(self.expr())
            if self.current_token.value == TokenType.COMMA.value:
                self.eat(TokenType.COMMA)
            else:
                break
        self.eat(TokenType.RPAREN)

        node = AST.ProcedureCall(
            proc_name=proc_name,
            actual_params=actual_params,
            token=token,
        )
        return node


    def block(self):
        """block : declarations compound_statement"""
        declaration_nodes = self.declarations()
        compound_statement_node = self.compound_statement()
        node = AST.Block(declaration_nodes, compound_statement_node)
        return node

    def program(self):
        """program : PROGRAM variable SEMI block DOT"""
        self.eat(TokenType.PROGRAM)
        node = self.variable()
        prog_name = node.value
        self.eat(TokenType.SEMI)
        block_node = self.block()
        program_node = AST.Program(prog_name, block_node)
        self.eat(TokenType.DOT)
        return program_node


    def parse(self):
        """
    program : PROGRAM variable SEMI block DOT

    block : declarations compound_statement


    declarations : VAR (variable_declaration SEMI)+
                    | (PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI)*
                    | empty


    variable_declaration : ID (COMMA ID)* COLON type_spec

    type_spec : INTEGER | REAL

    compound_statement : BEGIN statement_list END

    statement_list : statement
                   | statement SEMI statement_list

    statement : compound_statement
            | proccall_statement
            | assignment_statement
            | empty

    assignment_statement : L_VAR ASSIGN expr

    empty :

    expr : term ((PLUS | MINUS) term)*

    term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*

    factor : PLUS factor
           | MINUS factor
           | INTEGER_CONST
           | REAL_CONST
           | LPAREN expr RPAREN
           | R_VAR
    variable: ID
    L_VAR: ID
    R_VAR: ID
        """
        node = self.program()
        if self.current_token.type != TokenType.EOF.name:
            self.error()

        return node