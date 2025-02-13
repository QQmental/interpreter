import NodeVisitor as nNodeVisitor
import Symbol
import AST
import Token as nToken
import Error as nError
from collections import OrderedDict
from LogOption import _SHOULD_LOG_SCOPE
import DataObject as nDO
import TypeDescriptor as nTDS
import copy

BUILTIN_TYPE_BINOP_TABLE = {(nToken.TokenType.PLUS, nTDS.TypeDescriptor.TypeClass.INTEGER,     nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER),
                            (nToken.TokenType.PLUS, nTDS.TypeDescriptor.TypeClass.REAL,        nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL),
                            
                            (nToken.TokenType.MINUS, nTDS.TypeDescriptor.TypeClass.INTEGER,    nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER),
                            (nToken.TokenType.MINUS, nTDS.TypeDescriptor.TypeClass.REAL,       nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL),
                            
                            (nToken.TokenType.MULTIPLY, nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER),
                            (nToken.TokenType.MULTIPLY, nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL),


                            (nToken.TokenType.FLOAT_DIV, nTDS.TypeDescriptor.TypeClass.REAL, nTDS.TypeDescriptor.TypeClass.REAL, nTDS.TypeDescriptor.TypeClass.REAL),

                            (nToken.TokenType.INTEGER_DIV, nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER),
                            (nToken.TokenType.LOGIC_OR,    nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL),
                            (nToken.TokenType.LOGIC_AND,   nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL),
                            (nToken.TokenType.NOT,         nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL),
                            (nToken.TokenType.EQUAL,       nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER),
                            (nToken.TokenType.EQUAL,       nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL),
                            (nToken.TokenType.EQUAL,       nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL),
                            (nToken.TokenType.INEQUAL,     nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER),
                            (nToken.TokenType.INEQUAL,     nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL),
                            (nToken.TokenType.INEQUAL,     nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL),
                            (nToken.TokenType.LT,          nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER),
                            (nToken.TokenType.LT,          nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL),
                            (nToken.TokenType.LT,          nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL) ,
                            (nToken.TokenType.LTE,         nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER),
                            (nToken.TokenType.LTE,         nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL),
                            (nToken.TokenType.LTE,         nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL),
                            (nToken.TokenType.GT,          nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER),
                            (nToken.TokenType.GT,          nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL),
                            (nToken.TokenType.GT,          nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL),
                            (nToken.TokenType.GTE,         nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER, nTDS.TypeDescriptor.TypeClass.INTEGER),
                            (nToken.TokenType.GTE,         nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL,    nTDS.TypeDescriptor.TypeClass.REAL),
                            (nToken.TokenType.GTE,         nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL,    nTDS.TypeDescriptor.TypeClass.BOOL),
                        }

def define_type_aassignd_method(type_descriptor:nTDS.TypeDescriptor, is_decl:bool):
    if type_descriptor.type_class == nTDS.TypeDescriptor.TypeClass.BOOL: #cast the val into a BOOl value
        return  lambda data_obj : nDO.NaiveInitValueObject(data_obj.getter() != 0)
    elif type_descriptor.type_class == nTDS.TypeDescriptor.TypeClass.STRING:
        return  lambda data_obj : nDO.NaiveInitValueObject(data_obj.getter())
    elif type_descriptor.is_reference():
        if is_decl == True:
            return lambda data_obj : nDO.ReferenceObject(data_obj.ref())
        else:
            return lambda data_obj : data_obj
    else:
        return lambda data_obj : nDO.NaiveInitValueObject(data_obj.getter())



class EvalExprInfo(object):
    def __init__(self, type_descriptor:nTDS.TypeDescriptor, is_rvalue:bool, constexpr_value = None):
        self.type_descriptor = type_descriptor
        self.m_is_rvalue = is_rvalue
        self.constexpr_value_ = constexpr_value

        """if self.is_rvalue() == False and self.type_class() != nTDS.TypeDescriptor.TypeClass.REFERENCE:
            nested_type_descriptor = self.type_descriptor
            self.type_descriptor = nTDS.TypeDescriptor(self.type_descriptor.name+"_ref", nTDS.TypeDescriptor.TypeClass.REFERENCE)
            self.type_descriptor.nested_type_descriptor = nested_type_descriptor
        pass"""

    def get(self):
        return self.constexpr_value_
    
    def set(self, val):
        self.constexpr_value_ = val
    
    def type_class(self)->nTDS.TypeDescriptor.TypeClass:
        return self.type_descriptor.type_class
    
    def is_rvalue(self)->bool:
        return self.m_is_rvalue
    
    def is_integral(self)->bool:
        return self.type_class() == nTDS.TypeDescriptor.TypeClass.INTEGER or \
               self.type_class() == nTDS.TypeDescriptor.TypeClass.BOOL or \
               self.type_class() == nTDS.TypeDescriptor.TypeClass.ENUM
    
    def is_constexpr(self)->bool:
        return self.constexpr_value_ != None
    

def Eval_expr(lhs, rhs, bin_op, type_class = None):
    if type_class == None:
        if lhs.type_class() == nTDS.TypeDescriptor.TypeClass.REAL or rhs.type_class() == nTDS.TypeDescriptor.TypeClass.REAL:
            type_class = nTDS.TypeDescriptor.TypeClass.REAL
        else:
            type_class = nTDS.TypeDescriptor.TypeClass.INTEGER
    tds = nTDS.TypeDescriptor("", type_class)
    if lhs.get() != None and rhs.get() != None:
        return EvalExprInfo(tds, True, bin_op(lhs.get(), rhs.get()))
    else:
        return EvalExprInfo(tds, True)        


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
        builtin_types = [
                        nTDS.TypeDescriptor(nToken.TokenType.INTEGER.name, nTDS.TypeDescriptor.TypeClass.INTEGER),
                        nTDS.TypeDescriptor(nToken.TokenType.REAL.name, nTDS.TypeDescriptor.TypeClass.REAL),
                        nTDS.TypeDescriptor(nToken.TokenType.BOOL.name, nTDS.TypeDescriptor.TypeClass.BOOL),
                        nTDS.TypeDescriptor(nToken.TokenType.CHAR.name, nTDS.TypeDescriptor.TypeClass.CHAR),                        
                        nTDS.TypeDescriptor(nToken.TokenType.STRING.name, nTDS.TypeDescriptor.TypeClass.STRING),
                        nTDS.TypeDescriptor(nToken.TokenType.VOID.name, nTDS.TypeDescriptor.TypeClass.VOID)
                       ]
    
        for builtin_type in builtin_types:
            self.insert(Symbol.TypeSymbol(builtin_type))
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
        if (node.level == 1):
            if nToken.Compare(node.left.token, nToken.TokenType.IDENTIFIER) == False:
                self.error(error_code=nError.ErrorCode.UNEXPECTED_TOKEN, token=node.left.token)
            
        
        self.visit(node.inside)
        eval_expr = copy.deepcopy(self.visit(node.left))

        if (node.level == 1):
            node.left.assign_method = define_type_aassignd_method(node.left.type_descriptor, False)       
       
        tmp_type_descriptor = eval_expr.type_descriptor
        if tmp_type_descriptor.is_reference():
            tmp_type_descriptor = eval_expr.type_descriptor.nested_type_descriptor

        if tmp_type_descriptor.array_len:
            tmp_type_descriptor.array_len //= tmp_type_descriptor.dimension_size_list[0]
            tmp_type_descriptor.dimension -= 1
            del(tmp_type_descriptor.dimension_size_list[0])
            
            if tmp_type_descriptor.dimension == 0:
                tmp_type_descriptor.array_len = 0
                tmp_type_descriptor.dimension_size_list = None

        node.type_descriptor = eval_expr.type_descriptor
        return EvalExprInfo(node.type_descriptor, False)


    def visit_BinOp(self, node):
        if nToken.Compare(node.op, nToken.TokenType.PLUS):
            value = Eval_expr(self.visit(node.left), self.visit(node.right), lambda x,y : x + y)
        elif nToken.Compare(node.op, nToken.TokenType.MINUS):
            value = Eval_expr(self.visit(node.left), self.visit(node.right), lambda x,y : x - y)
        elif nToken.Compare(node.op, nToken.TokenType.MULTIPLY):
            value = Eval_expr(self.visit(node.left), self.visit(node.right), lambda x,y : x * y)
        elif nToken.Compare(node.op, nToken.TokenType.INTEGER_DIV):
            value = Eval_expr(self.visit(node.left), self.visit(node.right), lambda x,y : x // y)
        elif nToken.Compare(node.op, nToken.TokenType.FLOAT_DIV):
            value = Eval_expr(self.visit(node.left), self.visit(node.right), lambda x,y : float(float(x) / float(y)))
        elif nToken.Compare(node.op, nToken.TokenType.LOGIC_AND):
            value = Eval_expr(self.visit(node.left), self.visit(node.right), lambda x,y : x and y)
        elif nToken.Compare(node.op, nToken.TokenType.LOGIC_OR):
            value = Eval_expr(self.visit(node.left), self.visit(node.right), lambda x,y : x or y)
        elif nToken.Compare(node.op, nToken.TokenType.BIT_XOR):
            #not implemented
            self.visit(node.left)
            self.visit(node.right)
            value = 0
        elif nToken.Compare(node.op, nToken.TokenType.EQUAL):
            value = Eval_expr(self.visit(node.left), self.visit(node.right), lambda x,y : x == y, nTDS.TypeDescriptor.TypeClass.BOOL)
        elif nToken.Compare(node.op, nToken.TokenType.INEQUAL):
            value = Eval_expr(self.visit(node.left), self.visit(node.right), lambda x,y : x != y, nTDS.TypeDescriptor.TypeClass.BOOL)
        elif nToken.Compare(node.op, nToken.TokenType.LTE):
            value = Eval_expr(self.visit(node.left), self.visit(node.right), lambda x,y : x <= y, nTDS.TypeDescriptor.TypeClass.BOOL)
        elif nToken.Compare(node.op, nToken.TokenType.LT):
            value = Eval_expr(self.visit(node.left), self.visit(node.right), lambda x,y : x < y, nTDS.TypeDescriptor.TypeClass.BOOL)
        elif nToken.Compare(node.op, nToken.TokenType.GTE):
            value = Eval_expr(self.visit(node.left), self.visit(node.right), lambda x,y : x >= y, nTDS.TypeDescriptor.TypeClass.BOOL)
        elif nToken.Compare(node.op, nToken.TokenType.GT):
            value = Eval_expr(self.visit(node.left), self.visit(node.right), lambda x,y : x > y, nTDS.TypeDescriptor.TypeClass.BOOL)
        
        if node.right.type_descriptor.type_class == nTDS.TypeDescriptor.TypeClass.VOID:
            self.error(error_code=nError.ErrorCode.ASSIGNED_WITH_VOID, token=node.right.token)   

        node.type_descriptor = self.detect_type_binop(node.op.type, node.left, node.right)

        return value

    def visit_Num(self, node):
        return EvalExprInfo(node.type_descriptor, True, node.value)

    def visit_BoolVal(self, node):
        return EvalExprInfo(node.type_descriptor, True, node.value)
    
    def visit_StringVal(self, node):
        return EvalExprInfo(node.type_descriptor, True, node.value)

    def visit_CharVal(self, node):
        return EvalExprInfo(node.type_descriptor, True, node.value)


    def visit_UnaryOp(self, node):
        val = self.visit(node.expr)
        if node.op.type == nToken.TokenType.PLUS.name:     
            if val.get() != None:
                val.set(val.get() * 1)
        elif node.op.type == nToken.TokenType.MINUS.name:
            if val.get() != None:
                val.set(val.get() * -1)
        elif node.op.type == nToken.TokenType.NOT.name:
            if val.get() != None:
                val.set(not val.get())
                val.type_class = nTDS.TypeDescriptor.TypeClass.BOOL

        node.type_descriptor = val.type_descriptor
        return val
    
    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node):
        if node.op.type != nToken.TokenType.ASSIGN.name:
            self.error(error_code=nError.ErrorCode.UNEXPECTED_TOKEN, token=node.token)

        if node.op.value == ':=' or node.op.value == '=':
            self.visit(node.right)
            expr_info = self.visit(node.left)
        elif node.op.value == '+=':
            self.visit(node.right)
            expr_info = self.visit(node.left)
        elif node.op.value == '-=':
            self.visit(node.right)
            expr_info = self.visit(node.left)
        elif node.op.value == '*=':
            self.visit(node.right)
            expr_info = self.visit(node.left)
        elif node.op.value == '/=':
            self.visit(node.right)
            expr_info = self.visit(node.left)
        else:
            self.error()
        if self.compare_type(node.right, nTDS.TypeDescriptor.TypeClass.VOID):
            self.error(error_code=nError.ErrorCode.ASSIGNED_WITH_VOID, token=node.right.token)
        
        node.left.assign_method = define_type_aassignd_method(expr_info.type_descriptor, False)

    def visit_Var(self, node):
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=nError.ErrorCode.ID_NOT_FOUND, token=node.token)
        """
        if var_symbol.is_initialized == False \
           and self.current_scope.lookup(var_name, current_scope_only = True) != None:
            raise Exception(
                "Error: access uninitialized variable'%s'" % var_name
            )
        """
        #node.assign_method = define_type_aassignd_method(var_symbol.type_descriptor)
        node.type_descriptor = var_symbol.type_descriptor
        return EvalExprInfo(node.type_descriptor, False)
        
    def visit_MemberAccess(self, node):
        var_name = node.left.token.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=nError.ErrorCode.ID_NOT_FOUND, token=node.left.token)
        
        if var_symbol.type_class() == nTDS.TypeDescriptor.TypeClass.ENUM:
            print(str(var_symbol.name) + " is enum!")
            if node.right.right != None:
                self.error(error_code=nError.ErrorCode.UNEXPECTED_TOKEN, token=node.right.right.left.token)
            if var_symbol.member_set.get(node.right.left.token.value) == None:
                self.error(error_code=nError.ErrorCode.UNKNOWN_ENUM_MEMBER, token=node.right.left.token)
            
            val = var_symbol.member_set[node.right.left.token.value]
            def func():
                return nDO.NaiveInitValueObject(val)
            node.get_val_obj = func
            node.type_descriptor = var_symbol.type_descriptor

            tds = nTDS.TypeDescriptor(var_symbol.name, nTDS.TypeDescriptor.TypeClass.ENUM)
            return EvalExprInfo(tds, True, val)
        
        return EvalExprInfo()

    def visit_NoOp(self, node):
        pass

    def visit_EnumDecl(self, node):
        print(node.token, len(node.member_pair_list))

        member_set = {}
        val = 0
        for pair in node.member_pair_list:
            if pair[1] != None:
                val = self.visit(pair[1]).get()
            member_set[pair[0].value] = val
            val += 1

        esym = Symbol.EnumSymbol(node, member_set)
        self.current_scope.insert(esym)
        return

    def visit_Declarations(self, node):
        for decl in node.decls:
            self.visit(decl)

    def init_type_descp(self, type_descriptor:nTDS.TypeDescriptor):
        if type_descriptor.nested_type_descriptor != None:
            self.init_type_descp(type_descriptor.nested_type_descriptor)

    def visit_Type(self, node):
        cur_type_descriptor = node.type_descriptor
        while cur_type_descriptor.nested_type_descriptor != None:
            cur_type_descriptor = cur_type_descriptor.nested_type_descriptor

        var_name = cur_type_descriptor.name
        type_symbol = self.current_scope.lookup(var_name)
        if type_symbol is None:
            self.error(error_code = nError.ErrorCode.ID_NOT_FOUND, token=node.token)
        
        cur_type_descriptor.type_class = type_symbol.type_descriptor.type_class
        cur_type_descriptor.nested_type_descriptor = copy.deepcopy(type_symbol.type_descriptor.nested_type_descriptor)

        if cur_type_descriptor != node.type_descriptor:
            cur_type_descriptor.dimension = type_symbol.type_descriptor.dimension
            cur_type_descriptor.dimension_size_list = copy.deepcopy(type_symbol.type_descriptor.dimension_size_list)
            cur_type_descriptor.array_len = type_symbol.type_descriptor.array_len
        

        # array of node.type_descriptor.type_class
        if len(node.dimension_size_expr_list) > 0:
            product = 1
            
            cur_type_descriptor.dimension = len(node.dimension_size_expr_list)
            xlist = []
            for dim_size_expr in node.dimension_size_expr_list:
                eval_expr_info = self.visit(dim_size_expr)
                if eval_expr_info.is_integral() == False:
                    self.error(error_code = nError.ErrorCode.INVALID_ARRAY_SIZE_DEF, token=node.token)
                if eval_expr_info.is_constexpr() == False:
                    self.error(error_code = nError.ErrorCode.INVALID_ARRAY_SIZE_DEF, token=node.token)
                if eval_expr_info.get() < 0:
                    self.error(error_code = nError.ErrorCode.INVALID_ARRAY_SIZE_DEF, 
                            token=str(node.token)+ " array size=" + str(eval_expr_info.get()) +", array size should be greater or equal to 0")
                
                xlist.append(eval_expr_info.get())
                product *= eval_expr_info.get()
            
            cur_type_descriptor.dimension_size_list = xlist
            cur_type_descriptor.array_len = product
            
        elif cur_type_descriptor.type_class == nTDS.TypeDescriptor.TypeClass.STRING:
            cur_type_descriptor.dimension_size_list = [1]
        

    def visit_Control_flow_statement(self, node):
        scope = self.current_scope
        if  node.type == AST.Control_flow_statement.control_type.BREAK or \
            node.type == AST.Control_flow_statement.control_type.CONTINUE:
            while scope != None and scope.ar_tpye != nNodeVisitor.ARType.LOOP:
                if scope.ar_tpye == nNodeVisitor.ARType.PROCEDURE or scope.ar_tpye == nNodeVisitor.ARType.PROGRAM:
                    break
                scope = scope.parent_scope
            if scope == None or scope.ar_tpye != nNodeVisitor.ARType.LOOP:
                self.error(error_code = nError.ErrorCode.BREAK_ERROR, token=node.token)
        elif node.type == AST.Control_flow_statement.control_type.RETURN:
            return_val_expr_info = self.visit(node.return_val)
            while scope != None and scope.ar_tpye != nNodeVisitor.ARType.PROCEDURE and scope.ar_tpye != nNodeVisitor.ARType.PROGRAM:
                scope = scope.parent_scope
            proc_symobl = scope.lookup(scope.scope_name)

            if proc_symobl.return_type_node.type_descriptor.is_type_implicit_castable(return_val_expr_info.type_descriptor) == False:
                self.error(error_code = nError.ErrorCode.UNMATCHED_RETURN_VALUE, token=node.token)

            node.return_val.assign_method = define_type_aassignd_method(proc_symobl.return_type_node.type_descriptor, True)
            

    def visit_VARsDecl(self, node):
        self.visit(node.type_node)
        type_symbol = self.current_scope.lookup(node.type_node.value)

        if type_symbol.type_class() == nTDS.TypeDescriptor.TypeClass.VOID:
            self.error(
                error_code=nError.ErrorCode.INVALID_TYPE_OF_OBJ_DECLARATION,
                token=node.type_node.token         
            )
        for var in node.var_list:
            var_name = var.value
            if self.current_scope.lookup(var_name, current_scope_only = True) != None:
                self.error(
                    error_code=nError.ErrorCode.DUPLICATE_ID,
                    token=node.var_node.token,
                )
            
            var_symbol = Symbol.VarSymbol(var_name, node.type_node)

            self.current_scope.insert(var_symbol)

            self.visit(var)
            var.assign_method = define_type_aassignd_method(node.type_node.type_descriptor, True)
        
        if node.initilized_value != None:
            expr_info = self.visit(node.initilized_value)
        
        if var_symbol.type_descriptor.type_class == nTDS.TypeDescriptor.TypeClass.REFERENCE:
            if node.assignment_symbol != nToken.TokenType.LEFT_ARROW.value:
                self.error(error_code=nError.ErrorCode.INVALID_VARIABLE_INITILIZATION, 
                           token=str(node.type_node.token) + "reference variable should be assign with <-, rather than node.assignment_symbol")
            if expr_info.is_rvalue() == True:
                self.error(error_code=nError.ErrorCode.INVALID_VARIABLE_INITILIZATION, 
                           token=str(node.initilized_value.token) + "reference variable should be assign with a lvalue")

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

    def visit_ForBlock(self, node):
        loop_scope = ScopedSymbolTable(
            scope_name = str(node.token),
            scope_level= self.current_scope.scope_level + 1,
            ar_type = nNodeVisitor.ARType.LOOP,
            parent_scope = self.current_scope
        )
        self.current_scope = loop_scope
    
        for decl in node.var_decls:
            self.visit(decl)
        
        if node.condition == None: #synthetic node
            token = nToken.Token(nToken.TokenType.BOOL.name, True, -1, -1)
            AST.BoolVal(token)
        
        self.visit(node.condition)
        self.visit(node.statement_list)
        for post_statement in node.post_statements:
            self.visit(post_statement)

        self.current_scope = self.current_scope.parent_scope
        pass

    def visit_ProcedureDecl(self, node):
        self.visit(node.return_type_node)
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
            self.visit(param.type_node)

            var_symbol = Symbol.VarSymbol(param.var_node.value, param.type_node)
            var_symbol.is_initialized = True
            self.current_scope.insert(var_symbol)
            self.visit(param.var_node)
            
            proc_symbol.params.append((var_symbol, define_type_aassignd_method(var_symbol.type_descriptor, True)))

        self.visit(node.block_node)

        self.log(procedure_scope)
        self.log('LEAVE scope: %s' %  proc_name)
        self.current_scope = self.current_scope.parent_scope
    
    def visit_ProcedureCall(self, node):
        proc_symbol = self.current_scope.lookup(node.proc_name)
        
        if proc_symbol == None:
            self.error(error_code=nError.ErrorCode.ID_NOT_FOUND, token=node.token)
        if len(proc_symbol.params) != len(node.actual_params):
            self.error(error_code=nError.ErrorCode.PRAR_COUNT_NOT_MATCHED, token=node.token)
        
        node.ref_procedure = proc_symbol
        node.type_descriptor = proc_symbol.return_type_node.type_descriptor

        for idx, param_node in enumerate(node.actual_params):
            actual_para_expr_info = self.visit(param_node)
            para_var_symbol = proc_symbol.params[idx][0]
            if actual_para_expr_info.is_rvalue() == False:
                type_descriptor = nTDS.TypeDescriptor(actual_para_expr_info.type_descriptor.name + "_ref", nTDS.TypeDescriptor.TypeClass.REFERENCE)
                type_descriptor.nested_type_descriptor = actual_para_expr_info.type_descriptor
                param_node.type_descriptor = type_descriptor
                actual_para_expr_info = EvalExprInfo(param_node.type_descriptor, False, actual_para_expr_info.get())
            if para_var_symbol.type_descriptor.is_type_implicit_castable(actual_para_expr_info.type_descriptor) == False:
                self.error(error_code=nError.ErrorCode.PARAMETER_TYPE_MISMATCHED, 
                           token=node.token)

        return EvalExprInfo(proc_symbol.return_type_node.type_descriptor, not proc_symbol.return_type_node.type_descriptor.is_reference())

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
    
    def compare_type(self, node, type_class:nTDS.TypeDescriptor.TypeClass):
        return node.type_descriptor.type_class == type_class
    
    def detect_type_binop(self, binop:str, node_left, node_right):
        closest_tup = None
        max_score = 0

        for tup in BUILTIN_TYPE_BINOP_TABLE:
            if tup[0].name != binop:
                continue
            score = 1
            t1 = node_left.type_descriptor
            t2 = node_right.type_descriptor

            if t1.type_class == tup[1]:
                score += 1
            if t2.type_class == tup[2]:
                score += 1
            if score > max_score:
                max_score = score
                closest_tup = tup

        return nTDS.TypeDescriptor("", closest_tup[3])
