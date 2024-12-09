from NodeVisitor import NodeVisitor
import Symbol
import AST
from Token import TokenType
from Token import Token
from Error import ErrorCode
from Error import Error
from collections import OrderedDict
from LogOption import _SHOULD_LOG_SCOPE

class ScopedSymbolTable(object):
    def __init__(self, scope_name, scope_level, parent_scope = None):
        self._symbols = OrderedDict()
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.parent_scope = parent_scope
        self._init_builtins()

    def _init_builtins(self):
        self.insert(Symbol.BuiltinTypeSymbol('INTEGER'))
        self.insert(Symbol.BuiltinTypeSymbol('REAL'))

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
        print('Insert: %s' % symbol.name)
        self._symbols[symbol.name] = symbol

    def lookup(self, name,  current_scope_only=False):
        # 'symbol' is either an instance of the Symbol class or None
        print('Lookup: %s' % name)
        
        symbol = self._symbols.get(name)

        if symbol != None:
            return symbol
        
        if current_scope_only == True or self.parent_scope == None:
            return None
        
        return self.parent_scope.lookup(name)


class SemanticAnalyzer(NodeVisitor):
    def __init__(self):
        self.current_scope = None

    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)
            
    def error(self, error_code, token):                
        raise Error.SemanticError(
            error_code=error_code,
            token=token,
            message=f'{error_code.value} -> {token}',
        )

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

    def visit_Num(self, node):
        pass

    def visit_UnaryOp(self, node):
        if node.op.type == TokenType.PLUS.name:
            self.visit(node.expr)
        elif node.op.type == TokenType.MINUS.name:
            self.visit(node.expr)

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node):
        if node.op.type != TokenType.ASSIGN.name:
            self.error

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

    def visit_Var(self, node):
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)

    def visit_L_Var(self, node):
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)
        var_symbol.is_initialized = True

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
    def visit_NoOp(self, node):
        pass

    def visit_Declarations(self, node):
        for decl in node.decls:
            self.visit(decl)

    def visit_Type(self, node):
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            raise Exception(
                "Error: Type symbol(identifier) not found '%s'" % var_name
            )
        pass

    def visit_VARs_decl(self, node):
        type_symbol = self.current_scope.lookup(node.type_node.value)
        
        if type_symbol == None: #undefined type !
            self.error(
                error_code=ErrorCode.UNDEFINED_TYPE,
                token=node.type_node
            )

        for var in node.var_list:
            var_name = var.value
            if self.current_scope.lookup(var_name, current_scope_only = True) != None:
                self.error(
                    error_code=ErrorCode.DUPLICATE_ID,
                    token=node.var_node.token,
                )
            
            var_symbol = Symbol.VarSymbol(var_name, type_symbol)

            self.current_scope.insert(var_symbol)

    def visit_ProcedureDecl(self, node):
        proc_name = node.proc_name
        proc_symbol = Symbol.ProcedureSymbol(proc_name, node.block_node)
        self.current_scope.insert(proc_symbol)

        print('ENTER scope: %s' %  proc_name)
        # Scope for parameters and local variables
        procedure_scope = ScopedSymbolTable(
            scope_name = proc_name,
            scope_level= self.current_scope.scope_level + 1,
            parent_scope = self.current_scope
        )
        self.current_scope = procedure_scope

        # Insert parameters into the procedure scope
        for param in node.para_node:
            param_type = self.current_scope.lookup(param.type_node.value)
            var_symbol = Symbol.VarSymbol(param.var_node.value, param_type)
            var_symbol.is_initialized = True
            self.current_scope.insert(var_symbol)
            proc_symbol.params.append(var_symbol)

        self.visit(node.block_node)

        print(procedure_scope)
        print('LEAVE scope: %s' %  proc_name)
        self.current_scope = self.current_scope.parent_scope
    
    def visit_ProcedureCall(self, node):
        proc_symbol = self.current_scope.lookup(node.proc_name)
        
        if len(proc_symbol.params) != len(node.actual_params):
            self.error(error_code=ErrorCode.PRAR_COUNT_NOT_MATCHED, token=node.token)
        
        node.ref_procedure = proc_symbol

        for param_node in node.actual_params:
            self.visit(param_node)

    def visit_Block(self, node):
        self.visit(node.declarations)
        self.visit(node.compound_statement)

    def visit_Program(self, node):
        print('ENTER scope: global')
        global_scope = ScopedSymbolTable(
            scope_name='global',
            scope_level=1,
        )
        self.current_scope = global_scope

        # visit subtree
        self.visit(node.block)

        print(global_scope)
        print('LEAVE scope: global')