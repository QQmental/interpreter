import NodeVisitor as nNodeVisitor
import Symbol
import AST
import Token as nToken
from Token import TokenType
import Error as nError
from Error import ErrorCode
from Error import Error
from collections import OrderedDict
from LogOption import _SHOULD_LOG_SCOPE

"""
    (nToken.TokenType.MULTIPLY, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER),
    (nToken.TokenType.MULTIPLY, nToken.TokenType.INTEGER, nToken.TokenType.REAL, nToken.TokenType.REAL),
    (nToken.TokenType.MULTIPLY, nToken.TokenType.INTEGER, nToken.TokenType.BOOL, nToken.TokenType.INTEGER),
    (nToken.TokenType.MULTIPLY, nToken.TokenType.REAL, nToken.TokenType.INTEGER, nToken.TokenType.REAL),
    (nToken.TokenType.MULTIPLY, nToken.TokenType.REAL, nToken.TokenType.REAL, nToken.TokenType.REAL),
    (nToken.TokenType.MULTIPLY, nToken.TokenType.REAL, nToken.TokenType.BOOL, nToken.TokenType.REAL),
    (nToken.TokenType.MULTIPLY, nToken.TokenType.BOOL, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER),
    (nToken.TokenType.MULTIPLY, nToken.TokenType.BOOL, nToken.TokenType.REAL, nToken.TokenType.REAL),
    (nToken.TokenType.MULTIPLY, nToken.TokenType.BOOL, nToken.TokenType.BOOL, nToken.TokenType.BOOL)     
"""

BUILTIN_TYPE_BINOP_TABLE = {(nToken.TokenType.PLUS, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER),
                            (nToken.TokenType.PLUS, nToken.TokenType.REAL, nToken.TokenType.REAL, nToken.TokenType.REAL),
                            
                            (nToken.TokenType.MINUS, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER),
                            (nToken.TokenType.MINUS, nToken.TokenType.REAL, nToken.TokenType.REAL, nToken.TokenType.REAL),
                            
                            (nToken.TokenType.MULTIPLY, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER),
                            (nToken.TokenType.MULTIPLY, nToken.TokenType.REAL, nToken.TokenType.REAL, nToken.TokenType.REAL),


                            (nToken.TokenType.FLOAT_DIV, nToken.TokenType.REAL, nToken.TokenType.REAL, nToken.TokenType.REAL),

                            (nToken.TokenType.INTEGER_DIV, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER),
                            (nToken.TokenType.LOGIC_OR, nToken.TokenType.BOOL, nToken.TokenType.BOOL, nToken.TokenType.BOOL),
                            (nToken.TokenType.LOGIC_AND, nToken.TokenType.BOOL, nToken.TokenType.BOOL, nToken.TokenType.BOOL),
                            (nToken.TokenType.NOT, nToken.TokenType.BOOL, nToken.TokenType.BOOL, nToken.TokenType.BOOL),
                            (nToken.TokenType.EQUAL, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER),
                            (nToken.TokenType.EQUAL, nToken.TokenType.REAL, nToken.TokenType.REAL, nToken.TokenType.REAL),
                            (nToken.TokenType.EQUAL, nToken.TokenType.BOOL, nToken.TokenType.BOOL, nToken.TokenType.BOOL),
                            (nToken.TokenType.INEQUAL, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER),
                            (nToken.TokenType.INEQUAL, nToken.TokenType.REAL, nToken.TokenType.REAL, nToken.TokenType.REAL),
                            (nToken.TokenType.INEQUAL, nToken.TokenType.BOOL, nToken.TokenType.BOOL, nToken.TokenType.BOOL),
                            (nToken.TokenType.LT, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER),
                            (nToken.TokenType.LT, nToken.TokenType.REAL, nToken.TokenType.REAL, nToken.TokenType.REAL),
                            (nToken.TokenType.LT, nToken.TokenType.BOOL, nToken.TokenType.BOOL, nToken.TokenType.BOOL) ,
                            (nToken.TokenType.LTE, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER),
                            (nToken.TokenType.LTE, nToken.TokenType.REAL, nToken.TokenType.REAL, nToken.TokenType.REAL),
                            (nToken.TokenType.LTE, nToken.TokenType.BOOL, nToken.TokenType.BOOL, nToken.TokenType.BOOL),
                            (nToken.TokenType.GT, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER),
                            (nToken.TokenType.GT, nToken.TokenType.REAL, nToken.TokenType.REAL, nToken.TokenType.REAL),
                            (nToken.TokenType.GT, nToken.TokenType.BOOL, nToken.TokenType.BOOL, nToken.TokenType.BOOL),
                            (nToken.TokenType.GTE, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER, nToken.TokenType.INTEGER),
                            (nToken.TokenType.GTE, nToken.TokenType.REAL, nToken.TokenType.REAL, nToken.TokenType.REAL),
                            (nToken.TokenType.GTE, nToken.TokenType.BOOL, nToken.TokenType.BOOL, nToken.TokenType.BOOL),
                        }

class ScopedSymbolTable(object):
    def __init__(self, scope_name, scope_level, ar_type:nNodeVisitor.ARType, parent_scope = None):
        self._symbols = OrderedDict()
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.ar_tpye = ar_type
        self.parent_scope = parent_scope
        self._init_builtins()

    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)

    def _init_builtins(self):
        for builtin_type in nToken.BUILTIN_TYPES:
            self.insert(Symbol.BuiltinTypeSymbol(builtin_type.name, builtin_type.name))
        return

    def __str__(self):
        h1 = 'SCOPE (SCOPED SYMBOL TABLE)'
        lines = ['\n', h1, '=' * len(h1)]
        for header_name, header_value in (
            ('Scope name', self.scope_name),
            ('Scope level', self.scope_level),
        ):
            lines.append('%-15s: %s' % (header_name, header_value))
        h2 = 'Scope (Scoped symbol table) contents'
        lines.extend([h2, '-' * len(h2)])
        lines.extend(
            ('%7s: %r' % (key, value))
            for key, value in self._symbols.items()
        )
        lines.append('\n')
        s = '\n'.join(lines)
        return s

    __repr__ = __str__

    def insert(self, symbol):
        self.log('Insert: %s' % symbol.name)
        self._symbols[symbol.name] = symbol

    def lookup(self, name,  current_scope_only=False):
        # 'symbol' is either an instance of the Symbol class or None
        self.log('Lookup: %s' % name)
        
        symbol = self._symbols.get(name)

        if symbol != None:
            return symbol
        
        if current_scope_only == True or self.parent_scope == None:
            return None
        
        return self.parent_scope.lookup(name)
    
    def get_level(self, name):
        symbol = self._symbols.get(name)

        if symbol != None:
            return self.scope_level


class SemanticAnalyzer(nNodeVisitor.NodeVisitor):
    def __init__(self):
        self.current_scope = None

    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)
            
    def error(self, error_code, token):                
        raise nError.SemanticError(
            error_code=error_code,
            token=token,
            message=f'{error_code.value} -> {token}',
        )

    def visit_Subscript(self, node):
        pass

    def visit_BinOp(self, node):
        if node.op.type == TokenType.PLUS.name:
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == TokenType.MINUS.name:
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == TokenType.MULTIPLY.name:
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == TokenType.INTEGER_DIV.name:
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == TokenType.FLOAT_DIV.name:
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == TokenType.LOGIC_AND.name:
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == TokenType.LOGIC_OR.name:
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == TokenType.BIT_XOR.name:
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == TokenType.EQUAL.name:
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == TokenType.INEQUAL.name:
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == TokenType.LTE.name:
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == TokenType.LT.name:
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == TokenType.GTE.name:
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == TokenType.GT.name:
            self.visit(node.left)
            self.visit(node.right)

        if node.right.value_type == TokenType.VOID.name:
            self.error(error_code=ErrorCode.ASSIGNED_WITH_VOID, token=node.right.token)   

        node.type = self.detect_type_binop(node.op.type, node.left, node.right)

    def visit_Num(self, node):
        pass

    def visit_BoolVal(self, node):
        pass
    
    def visit_UnaryOp(self, node):
        if node.op.type == TokenType.PLUS.name:
            self.visit(node.expr)
            node.value_type = nToken.TokenType.INTEGER.name
        elif node.op.type == TokenType.MINUS.name:
            self.visit(node.expr)
            node.value_type = nToken.TokenType.INTEGER.name
        elif node.op.type == TokenType.NOT.name:
            self.visit(node.expr)
            node.value_type = nToken.TokenType.INTEGER.name
        if self.compare_type(node.expr, TokenType.VOID):
            self.error(error_code=ErrorCode.ASSIGNED_WITH_VOID, token=node.expr.token)

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node):
        if node.op.type != TokenType.ASSIGN.name:
            self.error(error_code=ErrorCode.UNEXPECTED_TOKEN, token=node.token)

        if node.op.value == ':=':
            self.visit(node.right)
            self.visit(node.left)
        elif node.op.value == '+=':
            self.visit(node.right)
            self.visit(AST.R_Var(node.left.token))
            self.visit(node.left)
        elif node.op.value == '-=':
            self.visit(node.right)
            self.visit(AST.R_Var(node.left.token))
            self.visit(node.left)
        elif node.op.value == '*=':
            self.visit(node.right)
            self.visit(AST.R_Var(node.left.token))
            self.visit(node.left)
        elif node.op.value == '/=':
            self.visit(node.right)
            self.visit(AST.R_Var(node.left.token))
            self.visit(node.left)
        else:
            self.error()
        if self.compare_type(node.right, TokenType.VOID):
            self.error(error_code=ErrorCode.ASSIGNED_WITH_VOID, token=node.right.token)

    def visit_Var(self, node):
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)

    def visit_L_Var(self, node:AST.L_Var):
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)

        if var_symbol.type == TokenType.BOOL.name: #cast the val into a BOOl value
            node.assign_method = lambda val : val != 0
        else:
            node.assign_method = lambda val : val

    def visit_R_Var(self, node):
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)
        """
        if var_symbol.is_initialized == False \
           and self.current_scope.lookup(var_name, current_scope_only = True) != None:
            raise Exception(
                "Error: access uninitialized variable'%s'" % var_name
            )
        """
        node.value_type = var_symbol.type
        
    def visit_NoOp(self, node):
        pass

    def visit_Declarations(self, node):
        for decl in node.decls:
            self.visit(decl)

    def visit_Type(self, node):
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code = ErrorCode.ID_NOT_FOUND, token=node.token)
        
        for dim_size_expr in node.dimension_size_list:
            self.visit(dim_size_expr)
            if nToken.Compare(dim_size_expr.token, nToken.TokenType.REAL):
                self.error(error_code = ErrorCode.INVALID_ARRAY_SIZE_REAL_DEF, token=node.token)
            elif dim_size_expr.value_type == nToken.TokenType.REAL.name:
                self.error(error_code = ErrorCode.INVALID_ARRAY_SIZE_REAL_DEF, token=node.token)

    def visit_Control_flow_statement(self, node):
        scope = self.current_scope
        if  node.type == AST.Control_flow_statement.control_type.BREAK or \
            node.type == AST.Control_flow_statement.control_type.CONTINUE:
            while scope != None and scope.ar_tpye != nNodeVisitor.ARType.LOOP:
                if scope.ar_tpye == nNodeVisitor.ARType.PROCEDURE or scope.ar_tpye == nNodeVisitor.ARType.PROGRAM:
                    break
                scope = scope.parent_scope
            if scope == None or scope.ar_tpye != nNodeVisitor.ARType.LOOP:
                self.error(error_code = ErrorCode.BREAK_ERROR, token=node.token)
        elif node.type == AST.Control_flow_statement.control_type.RETURN:
            self.visit(node.return_val)
            while scope != None and scope.ar_tpye != nNodeVisitor.ARType.PROCEDURE and scope.ar_tpye != nNodeVisitor.ARType.PROGRAM:
                scope = scope.parent_scope
            ret_type_name = scope.lookup(scope.scope_name).return_type_node.token.value
            if ret_type_name == nToken.TokenType.VOID.name and node.return_val.type != nToken.TokenType.VOID.name:
                self.error(error_code = ErrorCode.UNMATCHED_RETURN_VALUE, token=node.token)
            elif ret_type_name != nToken.TokenType.VOID.name and node.return_val.value_type == nToken.TokenType.VOID.name:
                self.error(error_code = ErrorCode.UNMATCHED_RETURN_VALUE, token=node.token)
            

    def visit_VARs_decl(self, node):
        self.visit(node.type_node)
        type_symbol = self.current_scope.lookup(node.type_node.value)

        if type_symbol.type == nToken.TokenType.VOID.name:
            self.error(
                error_code=ErrorCode.INVALID_TYPE_OF_OBJ_DECLARATION,
                token=node.type_node.token         
            )
        for var in node.var_list:
            var_name = var.value
            if self.current_scope.lookup(var_name, current_scope_only = True) != None:
                self.error(
                    error_code=ErrorCode.DUPLICATE_ID,
                    token=node.var_node.token,
                )
            
            var_symbol = Symbol.VarSymbol(var_name, type_symbol.type)

            self.current_scope.insert(var_symbol)
    
    def visit_IfBlock(self, node:AST.IfBlock):
        if node.condition != None:
            self.visit(node.condition)

        if_scope = ScopedSymbolTable(
            scope_name = str(node.token),
            scope_level= self.current_scope.scope_level + 1,
            ar_type = nNodeVisitor.ARType.IF,
            parent_scope = self.current_scope
        )
        self.current_scope = if_scope
        self.visit(node.statement_list)
        self.current_scope = self.current_scope.parent_scope        
        pass

    def visit_Cond_statements(self, node):
        for cond_block in node.if_blocks:
            self.visit(cond_block)

    def visit_WhileBlock(self, node):
        self.visit(node.condition)
        loop_scope = ScopedSymbolTable(
            scope_name = str(node.token),
            scope_level= self.current_scope.scope_level + 1,
            ar_type = nNodeVisitor.ARType.LOOP,
            parent_scope = self.current_scope
        )
        self.current_scope = loop_scope
        self.visit(node.statement_list)
        self.current_scope = self.current_scope.parent_scope

    def visit_ProcedureDecl(self, node):
        proc_name = node.proc_name
        proc_symbol = Symbol.ProcedureSymbol(proc_name,
                                             node.return_type_node,  
                                             node.block_node)
        self.current_scope.insert(proc_symbol)

        self.log('ENTER scope: %s' %  proc_name)
        # Scope for parameters and local variables
        procedure_scope = ScopedSymbolTable(
            scope_name = proc_name,
            scope_level= self.current_scope.scope_level + 1,
            ar_type = nNodeVisitor.ARType.PROCEDURE,
            parent_scope = self.current_scope
        )
        self.current_scope = procedure_scope

        # Insert parameters into the procedure scope
        for param in node.para_node:
            param_type = self.current_scope.lookup(param.type_node.value)
            var_symbol = Symbol.VarSymbol(param.var_node.value, param_type.type)
            var_symbol.is_initialized = True
            self.current_scope.insert(var_symbol)
            proc_symbol.params.append(var_symbol)

        self.visit(node.block_node)

        self.log(procedure_scope)
        self.log('LEAVE scope: %s' %  proc_name)
        self.current_scope = self.current_scope.parent_scope
    
    def visit_ProcedureCall(self, node):
        proc_symbol = self.current_scope.lookup(node.proc_name)
        
        if len(proc_symbol.params) != len(node.actual_params):
            self.error(error_code=ErrorCode.PRAR_COUNT_NOT_MATCHED, token=node.token)
        
        node.ref_procedure = proc_symbol
        node.value_type = proc_symbol.return_type_node.value

        for param_node in node.actual_params:
            self.visit(param_node)

    def visit_Block(self, node):
        self.visit(node.declarations)
        self.visit(node.compound_statement)

    def visit_Program(self, node):
        self.log('ENTER scope: global')
        global_scope = ScopedSymbolTable(
            scope_name='global',
            scope_level=1,
            ar_type = nNodeVisitor.ARType.PROGRAM
        )
        self.current_scope = global_scope

        # visit subtree
        self.visit(node.block)

        self.log(global_scope)
        self.log('LEAVE scope: global')
    
    def compare_type(self, node, token_type:TokenType):
        return node.value_type == token_type.name
    
    def detect_type_binop(self, binop:str, node_left, node_right):
        closest_tup = None
        max_score = 0

        for tup in BUILTIN_TYPE_BINOP_TABLE:
            if tup[0].name == binop:
                continue
            score = 1
            t1 = node_left.value_type
            t2 = node_right.value_type
            #if nToken.Compare(node_left.token, nToken.TokenType.IDENTIFIER):
            #    t1 = node_left.type
            #if nToken.Compare(node_right.token, nToken.TokenType.IDENTIFIER):                
            #    t2 = node_right.type

            if t1 == tup[1].name:
                score += 1
            if t2 == tup[2].name:
                score += 1
            if score > max_score:
                max_score = score
                closest_tup = tup

        return closest_tup[3].name