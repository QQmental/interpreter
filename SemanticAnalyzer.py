import NodeVisitor as nNodeVisitor
import Symbol
import AST
import Token as nToken
import Error as nError
from LogOption import _SHOULD_LOG_SCOPE
import DataObject as nDO
import EvalExprInfo as nEVEXPR
import TypeDescriptor as nTDS
import TypeSystem as nTST
import InitializedDataInfo as nINIT_DI
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
        return  lambda data_space, data_obj : nDO.NaiveInitValueObject(data_obj.getter() != 0)
    elif type_descriptor.type_class == nTDS.TypeDescriptor.TypeClass.STRING:
        return  lambda data_space, data_obj : nDO.NaiveInitValueObject(data_obj.getter())
    elif type_descriptor.is_reference():
        if is_decl == True:
            return lambda data_space, data_obj : nDO.ReferenceObject(data_space, data_obj)
        else:
            return lambda data_space, data_obj : data_obj
    elif type_descriptor.type_class == nTDS.TypeDescriptor.TypeClass.VOID:
        return lambda data_space, data_obj : nDO.NaiveInitValueObject(None)
    else:
        return lambda data_space, data_obj : nDO.NaiveInitValueObject(data_obj.getter())

    

def Eval_expr(lhs, rhs, bin_op, type_class = None):
    if type_class == None:
        if lhs.type_class() == nTDS.TypeDescriptor.TypeClass.REAL or rhs.type_class() == nTDS.TypeDescriptor.TypeClass.REAL:
            type_class = nTDS.TypeDescriptor.TypeClass.REAL
        else:
            type_class = nTDS.TypeDescriptor.TypeClass.INTEGER
    tds = nTDS.TypeDescriptor("", type_class)
    if lhs.is_constexpr() == True and rhs.is_constexpr() == True:
        return nEVEXPR.EvalExprInfo(tds, True, bin_op(lhs.get(), rhs.get()))
    else:
        return nEVEXPR.EvalExprInfo(tds, True)        


def create_interger_TDS()->nTDS.TypeDescriptor:
    return nTDS.TypeDescriptor(nToken.TokenType.INTEGER.name, nTDS.TypeDescriptor.TypeClass.INTEGER)

def create_real_TDS()->nTDS.TypeDescriptor:
    return nTDS.TypeDescriptor(nToken.TokenType.REAL.name, nTDS.TypeDescriptor.TypeClass.REAL)

def create_bool_TDS()->nTDS.TypeDescriptor:
    return nTDS.TypeDescriptor(nToken.TokenType.BOOL.name, nTDS.TypeDescriptor.TypeClass.BOOL)

def create_char_TDS()->nTDS.TypeDescriptor:
    return nTDS.TypeDescriptor(nToken.TokenType.CHAR.name, nTDS.TypeDescriptor.TypeClass.CHAR)

def create_string_TDS()->nTDS.TypeDescriptor:
    str_descrp = nTDS.TypeDescriptor(nToken.TokenType.STRING.name, nTDS.TypeDescriptor.TypeClass.STRING)
    str_descrp.dimension_size_list = [1]
    return str_descrp

def create_void_TDS()->nTDS.TypeDescriptor:
    return nTDS.TypeDescriptor(nToken.TokenType.VOID.name, nTDS.TypeDescriptor.TypeClass.VOID)

gBUILTIN_TYPE_LIST = [
                       create_interger_TDS(),
                       create_real_TDS(),
                       create_bool_TDS(),
                       create_char_TDS(),                        
                       create_string_TDS(),
                       create_void_TDS()
                     ]


class ScopedSymbolTable(object):
    global_scope_level = 1
    def __init__(self, scope_name, scope_level, ar_type:nNodeVisitor.ARType, parent_scope):
        self._symbols = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.ar_tpye = ar_type
        self.parent_scope = parent_scope
        self.max_var_count = 0
        self.var_count = 0
        if self.parent_scope != None and self.ar_tpye != nNodeVisitor.ARType.PROCEDURE:
            self.var_count = self.parent_scope.var_count
            self.max_var_count = self.var_count
            
        self._init_builtins()

    # return the symbol with initialized var_offset
    def add_alloc_sym(self, sym:Symbol.ReferableSymbol, length:int = 1):
        sym.var_offset = self.var_count
        self.var_count += length
        self.max_var_count = max(self.var_count, self.max_var_count)
        return sym

    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)

    def _init_builtins(self):
        for builtin_type in gBUILTIN_TYPE_LIST:
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
        return None
    def is_global_scope(self):
        return self.scope_level == ScopedSymbolTable.global_scope_level


class SemanticAnalyzer(nNodeVisitor.NodeVisitor):
    def __init__(self):
        self.current_scope = None
        self.allocated_sym_list = nINIT_DI.InitializedDataInfoTable()

    def log(self, msg):
        if _SHOULD_LOG_SCOPE:
            print(msg)
            
    def error(self, error_code, token):                
        raise nError.SemanticError(
            error_code=error_code,
            token=token,
            message=f'{error_code.value} -> {token}',
        )

    def get_global_scope(self)->ScopedSymbolTable:
        scp = self.current_scope
        while scp.parent_scope != None:
            scp = scp.parent_scope
        return scp

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
        return nEVEXPR.EvalExprInfo(node.type_descriptor, False)


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
        return nEVEXPR.EvalExprInfo(node.type_descriptor, True, node.value)

    def visit_BoolVal(self, node):
        return nEVEXPR.EvalExprInfo(create_bool_TDS(), True, node.value)
    
    def visit_StringVal(self, node):
        return nEVEXPR.EvalExprInfo(create_string_TDS(), True, node.value)

    def visit_CharVal(self, node):
        return nEVEXPR.EvalExprInfo(create_char_TDS(), True, node.value)


    def visit_UnaryOp(self, node):
        val = self.visit(node.expr)
        if node.op.type == nToken.TokenType.PLUS.name:     
            if val.is_constexpr() == True:
                val.set(val.get() * 1)
        elif node.op.type == nToken.TokenType.MINUS.name:
            if val.is_constexpr() == True:
                val.set(val.get() * -1)
        elif node.op.type == nToken.TokenType.NOT.name:
            if val.is_constexpr() == True:
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
            src_expr_info = self.visit(node.right)
            dst_expr_info = self.visit(node.left)
        elif node.op.value == '+=':
            src_expr_info = self.visit(node.right)
            dst_expr_info = self.visit(node.left)
        elif node.op.value == '-=':
            src_expr_info = self.visit(node.right)
            dst_expr_info = self.visit(node.left)
        elif node.op.value == '*=':
            src_expr_info = self.visit(node.right)
            dst_expr_info = self.visit(node.left)
        elif node.op.value == '/=':
            src_expr_info = self.visit(node.right)
            dst_expr_info = self.visit(node.left)
        else:
            self.error()
        
        if self.compare_type(node.right, nTDS.TypeDescriptor.TypeClass.VOID):
            self.error(error_code=nError.ErrorCode.ASSIGNED_WITH_VOID, token=node.right.token)

        if nTST.is_assigned_value_type_compatible(dst_expr_info, src_expr_info) == False:
            self.error(error_code=nError.ErrorCode.ASSIGNMENT_INCOMPATIBLE, 
                        token=str(node.left.token) + " " + str(node.right.token))

        node.left.assign_method = define_type_aassignd_method(src_expr_info.type_descriptor, False)

    
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
        node.var_offset = var_symbol.var_offset
        if var_symbol.is_on_stack_symbol:
            node.access_method = lambda call_stack : call_stack.top()[node.var_offset]
        else:
            node.access_method = lambda call_stack : call_stack.bot()[node.var_offset]

        node.type_descriptor = var_symbol.type_descriptor()

        if var_symbol.type_descriptor().is_reference() == True:
            expr_info = nEVEXPR.EvalExprInfo(node.type_descriptor, False)
        else:
            tds = nTDS.TypeDescriptor("", nTDS.TypeDescriptor.TypeClass.REFERENCE)
            tds.nested_type_descriptor = node.type_descriptor
            expr_info = nEVEXPR.EvalExprInfo(tds, False)
        
        if var_symbol.is_on_stack_symbol == False:
            expr_info.mark_link_needed()
        
        return expr_info


    def visit_MemberAccess(self, node):
        var_name = node.left.token.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=nError.ErrorCode.ID_NOT_FOUND, token=node.left.token)
        
        if var_symbol.type_class() == nTDS.TypeDescriptor.TypeClass.ENUM:
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
            node.type_descriptor = tds
            return nEVEXPR.EvalExprInfo(tds, True, val)
        
        return nEVEXPR.EvalExprInfo()

    def visit_NoOp(self, node):
        tds = nTDS.TypeDescriptor(nToken.TokenType.VOID.name, nTDS.TypeDescriptor.TypeClass.VOID)
        return nEVEXPR.EvalExprInfo(tds, True)

    def visit_EnumDecl(self, node):
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

    def init_type_descp(self, node):
        if nToken.Compare(node.token, nToken.TokenType.INTEGER):
            node.type_descriptor = create_interger_TDS()
        elif nToken.Compare(node.token, nToken.TokenType.REAL):
            node.type_descriptor = create_real_TDS()
        elif nToken.Compare(node.token, nToken.TokenType.BOOL):
            node.type_descriptor = create_bool_TDS()
        elif nToken.Compare(node.token, nToken.TokenType.VOID):
            node.type_descriptor = create_void_TDS()
        elif nToken.Compare(node.token, nToken.TokenType.CHAR):
            node.type_descriptor = create_char_TDS()
        elif nToken.Compare(node.token, nToken.TokenType.STRING):
            node.type_descriptor = create_string_TDS()    
        elif nToken.Compare(node.token, nToken.TokenType.REFERNECE):
            node.type_descriptor = nTDS.TypeDescriptor("", nTDS.TypeDescriptor.TypeClass.REFERENCE)
        elif node.is_callable() == True:
            node.type_descriptor = nTDS.TypeDescriptor("", nTDS.TypeDescriptor.TypeClass.CallAble)
            self.visit(node.return_type_node())
            node.type_descriptor.return_type_descriptor = node.return_type_node().type_descriptor
            lst = []
            for para in node.para_node():
                self.visit(para.type_node)
                lst.append(para.type_node.type_descriptor)
            node.type_descriptor.param_type_list = lst
        else:
            node.type_descriptor = nTDS.TypeDescriptor(node.value)
        
        if node.nested_type != None:
            self.init_type_descp(node.nested_type)
            node.type_descriptor.nested_type_descriptor = node.nested_type.type_descriptor 

        if node.type_descriptor.is_reference() == True:
            ref_type_descriptor = nTDS.TypeDescriptor(node.type_string(), nTDS.TypeDescriptor.TypeClass.REFERENCE)
            ref_type_descriptor.nested_type_descriptor = node.nested_type.type_descriptor
            self.type_descriptor = ref_type_descriptor
        
        return node


    def visit_Type(self, node):
        node = self.init_type_descp(node)
        cur_type_descriptor = node.type_descriptor
        while cur_type_descriptor.nested_type_descriptor != None:
            cur_type_descriptor = cur_type_descriptor.nested_type_descriptor

        if cur_type_descriptor.has_symbol():
            type_name = cur_type_descriptor.name
            type_symbol = self.current_scope.lookup(type_name)
            if type_symbol is None:
                self.error(error_code = nError.ErrorCode.ID_NOT_FOUND, token=node.token)
            
            cur_type_descriptor.type_class = type_symbol.type_descriptor().type_class

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
            

    def visit_CallableProtocal(self, node):
        for param in node.para_node():
            self.visit(param.type_node)
        self.visit(node.return_type_node())
        pass

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

            if nTST.is_type_implicit_castable(proc_symobl.return_type_descriptor() , return_val_expr_info.type_descriptor) == False:
                self.error(error_code = nError.ErrorCode.UNMATCHED_RETURN_VALUE, token=node.token)

            node.return_val.assign_method = define_type_aassignd_method(proc_symobl.return_type_descriptor(), True)
            

    def visit_VARsDecl(self, node):
        self.visit(node.type_node)
        if node.type_node.type_descriptor.has_symbol():
            type_symbol = self.current_scope.lookup(node.type_node.symbol_string())
            if type_symbol.type_class() == nTDS.TypeDescriptor.TypeClass.VOID:
                self.error(
                    error_code=nError.ErrorCode.INVALID_TYPE_OF_OBJ_DECLARATION,
                    token=node.type_node.token         
                )

        expr_info = nEVEXPR.EvalExprInfo(node.type_node.type_descriptor, False)

        if node.initilized_value != None:
            expr_info = self.visit(node.initilized_value)
        
        for var in node.var_list:
            var_name = var.value
            if self.current_scope.lookup(var_name, current_scope_only = True) != None:
                self.error(
                    error_code=nError.ErrorCode.DUPLICATE_ID,
                    token=node.var_node.token,
                )
                
            if node.type_node.type_descriptor.is_reference() and \
                node.type_node.nested_type.type_descriptor.type_class == nTDS.TypeDescriptor.TypeClass.CallAble:
                decl_symbol = alloc_callable_symbol(self.current_scope , var_name, self.current_scope.scope_level != 1, node.type_node.type_descriptor)
            else:        
                decl_symbol = alloc_var_symbol(self.current_scope , var_name, self.current_scope.scope_level != 1, node.type_node)
            
            
            self.current_scope.insert(decl_symbol)
            self.visit(var)
            var.assign_method = define_type_aassignd_method(node.type_node.type_descriptor, True)
        
            if decl_symbol.is_on_stack_symbol == False and node.initilized_value != None:
                if expr_info.is_constexpr() == False and expr_info.is_link_needed() == False:
                    self.error(error_code=nError.ErrorCode.INVALID_VARIABLE_INITILIZATION, 
                               token = str(node.initilized_value.token) + "\nstatic allocated variable should not be initialized by a run time result")

            if self.current_scope.scope_level == ScopedSymbolTable.global_scope_level:
                info = nINIT_DI.InitializedDataInfo(var_name, decl_symbol.var_offset, expr_info, node.initilized_value, var.assign_method)
                self.allocated_sym_list.append(info)

        if decl_symbol.type_descriptor().type_class == nTDS.TypeDescriptor.TypeClass.REFERENCE:
            if node.assignment_symbol == "":
                self.error(error_code=nError.ErrorCode.INVALID_VARIABLE_INITILIZATION, 
                           token=str(node.type_node.token) + "reference variable should be assigned when it's declared")
            if node.assignment_symbol != nToken.TokenType.LEFT_ARROW.value:
                self.error(error_code=nError.ErrorCode.INVALID_VARIABLE_INITILIZATION, 
                           token=str(node.type_node.token) + "reference variable should be assigned with <-, rather than node.assignment_symbol")
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

        self.enter_new_socpe(if_scope, lambda : self.visit(node.statement))
        self.current_scope.max_var_count = max(self.current_scope.max_var_count, if_scope.max_var_count)
        

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

        self.enter_new_socpe(loop_scope, lambda : self.visit(node.statement))      
        self.current_scope.max_var_count = max(self.current_scope.max_var_count, loop_scope.max_var_count)

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
        
        def visit_method():
            self.visit(node.condition)
            self.visit(node.statement)
            for post_statement in node.post_statements:
                self.visit(post_statement)
        
        self.enter_new_socpe(loop_scope, visit_method)
        self.current_scope.max_var_count = max(self.current_scope.max_var_count, loop_scope.max_var_count)

    def visit_ProcedureDecl(self, node):
        proc_name = node.proc_name

        targ_proc_symbol = self.current_scope.lookup(proc_name)

        self.visit(node.protocal)

        # Scope for parameters and local variables
        procedure_scope = ScopedSymbolTable(
            scope_name = proc_name,
            scope_level= self.current_scope.scope_level + 1,
            ar_type = nNodeVisitor.ARType.PROCEDURE,
            parent_scope = self.current_scope
        )

        if targ_proc_symbol != None and targ_proc_symbol.is_defined() == True and node.block_node != None:
            err_msg = "{token}, first defined at {prev_token}".format(token = node.token, prev_token = targ_proc_symbol.definition_token)
            self.error(  nError.ErrorCode.REDEFINE_FUNC, err_msg)  

        self.log('ENTER scope: %s' %  proc_name)


        tds = nTDS.TypeDescriptor("", nTDS.TypeDescriptor.TypeClass.CallAble)
        tds.return_type_descriptor = node.return_type_node().type_descriptor
        para_tlst = []
        for para in node.para_node():
            para_tlst.append(para.type_node.type_descriptor)
        tds.param_type_list = para_tlst

        proc_symbol = Symbol.ProcSymbol(proc_name,
                                        self.current_scope.scope_level != ScopedSymbolTable.global_scope_level,
                                        tds)

        proc_symbol.set_definition(node.block_node, node.token)


        def walk_proc_decl():
            param_type_list = []
            params = []
            assign_methods = []
            # Insert parameters into the procedure scope
            for param in node.para_node():
                self.visit(param.type_node)
                
                if nToken.Compare(param.var_node.token, nToken.TokenType.COLON):
                    if proc_symbol.is_defined() == True:
                        self.error(error_code=nError.ErrorCode.ANONYMOUS_PARAMETER, token=param.var_node.token)
                    else:
                        continue
                var_symbol = alloc_var_symbol(self.current_scope, param.var_node.value, True, param.type_node)
                var_symbol.is_initialized = True
                self.current_scope.insert(var_symbol)
                self.visit(param.var_node)
                
                param_type_list.append(var_symbol.type_descriptor)
                params.append(var_symbol)
                assign_methods.append(define_type_aassignd_method(var_symbol.type_descriptor(), True))
   
            targ_proc_symbol.params = params
            targ_proc_symbol.assign_methods = assign_methods

            
            if proc_symbol.is_defined():
                targ_proc_symbol.set_definition(node.block_node, node.token)
                self.visit(targ_proc_symbol.block_node)
                targ_proc_symbol.max_var_count = self.current_scope.max_var_count

        
        if targ_proc_symbol == None:          
            scp = self.get_global_scope()
            targ_proc_symbol = alloc_proc_symbol(scp, proc_name, False, tds)
            
            scp.insert(targ_proc_symbol)
            expr_info = nEVEXPR.EvalExprInfo(nTDS.TypeDescriptor(nToken.TokenType.PROCEDURE.name, nTDS.TypeDescriptor.TypeClass.CallAble), False, targ_proc_symbol)
            info = nINIT_DI.InitializedDataInfo(proc_name, targ_proc_symbol.var_offset, expr_info, None)
            self.allocated_sym_list.append(info)
            self.enter_new_socpe(procedure_scope, lambda : walk_proc_decl())
        else:
            error_callback = lambda error_code : self.error(error_code=error_code, token=node.token)

            targ_proc_symbol.set_definition(proc_symbol.block_node, proc_symbol.definition_token)

            self.enter_new_socpe(procedure_scope, lambda : walk_proc_decl())

            callable_type_compare(proc_symbol, targ_proc_symbol, error_callback)


    def visit_CallInvoked(self, node):
        access_symbol = self.current_scope.lookup(node.access_name)
        
        if access_symbol == None:
            self.error(error_code=nError.ErrorCode.ID_NOT_FOUND, token=node.token)
        #if len(access_symbol.params) != len(node.actual_params):
        #    self.error(error_code=nError.ErrorCode.PRAR_COUNT_NOT_MATCHED, token=node.token)
        
        self.visit(node.callee_src)

        node.var_offset = access_symbol.var_offset
        node.type_descriptor = access_symbol.return_type_descriptor()

        for idx, param_node in enumerate(node.actual_params):
            input_para_expr_info = self.visit(param_node)
            para_var_symbol= access_symbol.params[idx]
            if input_para_expr_info.is_rvalue() == False and input_para_expr_info.type_descriptor.is_reference() == False:
                type_descriptor = nTDS.TypeDescriptor(input_para_expr_info.type_descriptor.name + "_ref", nTDS.TypeDescriptor.TypeClass.REFERENCE)
                type_descriptor.nested_type_descriptor = input_para_expr_info.type_descriptor
                param_node.type_descriptor = type_descriptor
                input_para_expr_info = nEVEXPR.EvalExprInfo(param_node.type_descriptor, False, input_para_expr_info.get())

                
            if nTST.is_type_implicit_castable(para_var_symbol.type_descriptor(), input_para_expr_info.type_descriptor) == False:
                self.error(error_code=nError.ErrorCode.PARAMETER_TYPE_MISMATCHED, 
                           token=node.token)

        return nEVEXPR.EvalExprInfo(node.type_descriptor, not node.type_descriptor.is_reference())

    def visit_Block(self, node):
        self.visit(node.declarations)
        self.visit(node.compound_statement)

    def visit_Program(self, node):
        self.log('ENTER scope: global')
        global_scope = ScopedSymbolTable(
            scope_name='global',
            scope_level=ScopedSymbolTable.global_scope_level,
            ar_type = nNodeVisitor.ARType.PROGRAM,
            parent_scope = None
        )
        
        token = nToken.Create_reserved_keyword_token(nToken.TokenType.INTEGER.value)
        token.column = -1
        token.lineno = -1
        always_return_int = AST.Type(token)

        prog_name = node.name

        prog_symbol =  alloc_proc_symbol(global_scope, prog_name, False, always_return_int.type_descriptor)
        
        prog_symbol.set_definition(node.block_node, node.token)

        node.var_offset = prog_symbol.var_offset

        expr_info = nEVEXPR.EvalExprInfo(nTDS.TypeDescriptor(nToken.TokenType.PROGRAM.name, nTDS.TypeDescriptor.TypeClass.CallAble), False, prog_symbol)
        info = nINIT_DI.InitializedDataInfo(prog_name, prog_symbol.var_offset, expr_info, prog_symbol, None)
        self.allocated_sym_list.append(info)

        def visit_method():
            self.visit(always_return_int)
            self.current_scope.insert(prog_symbol)
            self.visit(node.block_node.declarations)
            self.visit(node.declarations)
            self.visit(node.block_node.compound_statement)
        
        max_var_count = self.enter_new_socpe(global_scope, lambda : visit_method())
        self.allocated_sym_list.max_var_count = max_var_count
        
    
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
    
    def enter_new_socpe(self, new_scope, visit_method):
        self.current_scope = new_scope  
        visit_method()
        self.log(new_scope)
        self.log('LEAVE scope: %s' %  new_scope.scope_name)
        max_var_count = self.current_scope.max_var_count
        self.current_scope = self.current_scope.parent_scope
        return max_var_count


def alloc_var_symbol(scope:ScopedSymbolTable, name:str, is_on_stack_symbol: bool, type_node: AST.Type):
    sym = Symbol.VarSymbol(name, is_on_stack_symbol, type_node)
    if type_node.type_descriptor.array_len > 0:
        sym = scope.add_alloc_sym(sym, type_node.type_descriptor.array_len)
    else:
        sym = scope.add_alloc_sym(sym)

    return sym

def alloc_proc_symbol(scope:ScopedSymbolTable, name:str, is_on_stack_symbol: bool, type_descriptor:nTDS.TypeDescriptor):
    sym = Symbol.ProcSymbol(name, is_on_stack_symbol, type_descriptor)
    sym = scope.add_alloc_sym(sym)
    return sym

def alloc_callable_symbol(scope:ScopedSymbolTable, name:str, is_on_stack_symbol: bool, type_descriptor:nTDS.TypeDescriptor):
    sym = Symbol.CallableSymbol(name, is_on_stack_symbol, type_descriptor)
    sym = scope.add_alloc_sym(sym)
    return sym

def callable_type_compare(lhs:Symbol.ProcSymbol, rhs:Symbol.ProcSymbol, error_callback):
    for idx, param in enumerate(lhs.type_descriptor().param_type_list):
        if param.is_type_equal(rhs.type_descriptor().param_type_list[idx]) == False:
            error_callback(nError.ErrorCode.REDECL_PROC_PARAMETER_TYPE_MISMATCHED)
            return False
    
    if lhs.type_descriptor().return_type_descriptor.is_type_equal(rhs.type_descriptor().return_type_descriptor) == False:
        error_callback(nError.ErrorCode.REDECL_PROC_RETURB_TYPE_MISMATCHED)
        return False
    
    return True