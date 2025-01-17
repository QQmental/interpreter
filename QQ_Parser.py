import Error
from Error import ErrorCode
import Token as nToken
from Token import TokenType
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


    def factor_0(self, left):
        if (nToken.Compare(self.current_token, TokenType.LEFT_BRACKET)):
            self.eat(TokenType.LEFT_BRACKET)
            inside = self.expr()
            self.eat(TokenType.RIGHT_BRACKET)
            return self.factor_0(AST.Subscript(left, inside))
        else:
            return left

    def factor1(self):
        """  integer (subscript) 
           | real(subscript) 
           | TRUE(subscript) 
           | FALSE(subscript) 
           | (unary)factor1 
           | (expr) 
           | R_variable (subscript) 
           | function call (subscript) """
        
        token = self.current_token
        if token.type == TokenType.INTEGER.name:
            self.eat(TokenType.INTEGER)
            return self.factor_0(AST.Num(token, TokenType.INTEGER.name))
        elif token.type == TokenType.REAL.name:
            self.eat(TokenType.REAL)
            return self.factor_0(AST.Num(token, TokenType.REAL.name))
        elif token.type == TokenType.TRUE.name:
            self.eat(TokenType.TRUE)
            return self.factor_0(AST.BoolVal(token))
        elif token.type == TokenType.FALSE.name:
            self.eat(TokenType.FALSE)
            return self.factor_0(AST.BoolVal(token))
        elif token.type == TokenType.PLUS.name:
            self.eat(TokenType.PLUS)
            return AST.UnaryOp(token, self.factor1())
        elif token.type == TokenType.MINUS.name:
            self.eat(TokenType.MINUS)
            return AST.UnaryOp(token, self.factor1())
        elif token.type == TokenType.NOT.name:
            self.eat(TokenType.NOT)
            return AST.UnaryOp(token, self.factor1())        
        elif token.type == TokenType.LPAREN.name:
            self.eat(TokenType.LPAREN)
            node = self.expr()
            self.eat(TokenType.RPAREN)
            return self.factor_0(node)
        elif self.lexer.current_char == TokenType.LPAREN.value:
            node = self.proccall_statement()
            return self.factor_0(node)
        else:
            return self.factor_0(self.R_variable())

    
    def factor2(self):
        """factor2 : factor1 ((MUL | INTEGER_DIV | FLOAT_DIV) factor1)*"""
        node = self.factor1()

        while self.current_token.type in (TokenType.MULTIPLY.name, TokenType.INTEGER_DIV.name, TokenType.FLOAT_DIV.name):
            token = self.current_token
            if token.type == TokenType.MULTIPLY.name:
                self.eat(TokenType.MULTIPLY)
                node = AST.BinOp(node, token, self.factor1())
            elif token.type == TokenType.INTEGER_DIV.name:
                self.eat(TokenType.INTEGER_DIV)
                node = AST.BinOp(node, token, self.factor1())
            elif token.type == TokenType.FLOAT_DIV.name:
                self.eat(TokenType.FLOAT_DIV)
                node = AST.BinOp(node, token, self.factor1())

        return node

    def factor3(self):
        """ factor3   : factor2 ((PLUS | MINUS) factor2)* """
        node = self.factor2()
        while self.current_token.type in (TokenType.PLUS.name, TokenType.MINUS.name):
            token = self.current_token
            
            if token.type == TokenType.PLUS.name:
                self.eat(TokenType.PLUS)
            elif token.type == TokenType.MINUS.name:
                self.eat(TokenType.MINUS)

            node = AST.BinOp(node, token, self.factor2())

        return node


    def factor4(self):
        """ factor4   : factor3 ((LT | LTE | GT | GTE) factor3)* """
        node = self.factor3()
        while self.current_token.type in (TokenType.LT.name, TokenType.LTE.name, TokenType.GT.name, TokenType.GTE.name):
            token = self.current_token

            if token.type == TokenType.LT.name:
                self.eat(TokenType.LT)
            elif token.type == TokenType.LTE.name:
                self.eat(TokenType.LTE)
            elif token.type == TokenType.GT.name:
                self.eat(TokenType.GT)
            elif token.type == TokenType.GTE.name:
                self.eat(TokenType.GTE)                                
            node = AST.BinOp(node, token, self.factor3())

        return node

    def factor5(self):
        """ factor5   : factor4 ((EQUAL | INEQUAL) factor4)* """
        node = self.factor4()
        while self.current_token.type in (TokenType.INEQUAL.name, TokenType.EQUAL.name):
            token = self.current_token

            if token.type == TokenType.INEQUAL.name:
                self.eat(TokenType.INEQUAL)
            elif token.type == TokenType.EQUAL.name:
                self.eat(TokenType.EQUAL)
            node = AST.BinOp(node, token, self.factor4())

        return node

    def factor6(self):
        """ factor6   : factor5 (LOGIC_AND factor5)* """
        node = self.factor5()
        while self.current_token.type in (TokenType.LOGIC_AND.name):
            token = self.current_token
            
            if token.type == TokenType.LOGIC_AND.name:
                self.eat(TokenType.LOGIC_AND)

            node = AST.BinOp(node, token, self.factor5())

        return node
    
    def expr(self):
        """ expr   : SEMI | (factor6 ((LOGIC_OR | BIT_XOR) factor6))* """
        if self.current_token.type == TokenType.SEMI.name:
            return self.empty()
        
        node = self.factor6()
        while self.current_token.type in (TokenType.LOGIC_OR.name, TokenType.BIT_XOR.name):
            token = self.current_token

            if token.type == TokenType.LOGIC_OR.name:
                self.eat(TokenType.LOGIC_OR)
            elif token.type == TokenType.BIT_XOR.name:
                self.eat(TokenType.BIT_XOR)
            node = AST.BinOp(node, token, self.factor6())

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
        return AST.NoOp()

    def if_statement(self):
        """
        statement : IfBlock_statement (((ELSE)*IfBlock_statement)* (ELSE IfBlock_statement)*)
                  | IfBlock_statement 
        """
        if_blocks = []
        if_blocks.append(self.IfBlock_statement())
        while self.current_token.type == TokenType.ELSE.name:
            self.eat(TokenType.ELSE)
            if_blocks.append(self.IfBlock_statement())

        return AST.Cond_statements(if_blocks)
    
    def var_start_statement(self):
        if self.lexer.overlook(0) == ":" and self.lexer.overlook(1) == "=":
            return self.assignment_statement()
        else:
            return self.expr()

    def statement(self):
        """
        statement : compound_statement
                  | proccall_statement
                  | assignment_statement
                  | return (expr)*
                  | break
                  | continue
                  | expr
                  | empty
        """
        token = self.current_token
        if nToken.Compare(token, TokenType.BEGIN):
            node = self.compound_statement()
        elif nToken.Compare(token, TokenType.IDENTIFIER) \
             and self.lexer.current_char == TokenType.LPAREN.value:
            node = self.proccall_statement()
        elif nToken.Compare(token, TokenType.WHILE):
            node = self.WhileBlock()
        elif nToken.Compare(token, TokenType.IDENTIFIER):
            if self.lexer.overlook_token(0).type == TokenType.ASSIGN.name:
                node = self.assignment_statement()
            else:
                node = self.expr()
        elif nToken.Compare(token, TokenType.BREAK):
            self.eat(TokenType.BREAK)
            node = AST.Control_flow_statement(token, AST.Control_flow_statement.control_type.BREAK)
        elif nToken.Compare(token, TokenType.CONTINUE):
            self.eat(TokenType.CONTINUE)
            node = AST.Control_flow_statement(token, AST.Control_flow_statement.control_type.CONTINUE)
        elif nToken.Compare(token, TokenType.RETURN):
            self.eat(TokenType.RETURN)
            node = AST.Control_flow_statement(token, AST.Control_flow_statement.control_type.RETURN, self.expr())
        elif nToken.Compare(token, TokenType.END):
            node = self.empty()
        else:
            node = self.expr()
        return node

    def statement_list(self):
        """
        statement_list : (if_statement | statement)
                       | (if_statement | statement) SEMI statement_list
        """
        
        if nToken.Compare(self.current_token, TokenType.IF):
            node = self.if_statement()
        else:
            node = self.statement()

        results = [node]

        while nToken.Compare(self.current_token, TokenType.SEMI):
            self.eat(TokenType.SEMI)
            if nToken.Compare(self.current_token, TokenType.IF):
                node = self.if_statement()
            else:
                node = self.statement()
            results.append(node)

        if nToken.Compare(self.current_token, TokenType.IDENTIFIER):
            self.error()

        return results

    def IfBlock_statement(self):
        if self.current_token.type == TokenType.IF.name:
            self.eat(TokenType.IF)
            cond = self.expr()
        else:
            cond = None
        if self.current_token.type == TokenType.BEGIN.name:
            statement_list = self.compound_statement()
        else:
            statement_list = self.statement()
        ret = AST.IfBlock(self.current_token, statement_list, cond)
        return ret

    def WhileBlock(self):
        token = self.current_token
        self.eat(TokenType.WHILE)
        cond = self.expr()
        if self.current_token.type == TokenType.BEGIN.name:
            statement_list = self.compound_statement()
        else:
            statement_list = self.statement()
        return AST.WhileBlock(token, statement_list, cond)

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
        is_builtin = False

        for builtin_type in nToken.BUILTIN_TYPES:
            if type_name == builtin_type.value:
                self.eat(builtin_type)
                is_builtin = True
        
        if is_builtin == False:
            if nToken.Compare(token, TokenType.IDENTIFIER) == False:
                self.error()
            else:
                self.eat(TokenType.IDENTIFIER)
        dim_size = 0
        dim_size_list = []
        while nToken.Compare(self.current_token, nToken.TokenType.LEFT_BRACKET):
            self.eat(nToken.TokenType.LEFT_BRACKET)
            dim_size_list.append(self.expr())
            dim_size += 1
            self.eat(nToken.TokenType.RIGHT_BRACKET)
        return AST.Type(token, dim_size, dim_size_list)
        
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
            
            
            #parse return value type, if no 'return type' is represented, then 
            # return type is void 
            if nToken.Compare(self.current_token, TokenType.RIGHT_ARROW): 
                self.eat(TokenType.RIGHT_ARROW)
                return_type = self.type_spec()
            else:
                return_type = AST.Type(nToken.Token(TokenType.VOID.name, "VOID", lineno=-1, column=-1))

            self.eat(TokenType.SEMI)
            block_node = self.block()
            self.eat(TokenType.SEMI)
            proc_declarations.append(AST.ProcedureDecl(procedure_name, param_list, return_type, block_node))
        return proc_declarations

    def proccall_statement(self):
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

    expr : factor2 ((PLUS | MINUS) factor2)*

    factor2 : factor1 ((MUL | INTEGER_DIV | FLOAT_DIV) factor1)*

    factor1 : PLUS factor1
           | MINUS factor1
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