import NodeVisitor as nNodeVisitor
import Symbol
import AST
import Token as nToken
import Error as nError
from collections import OrderedDict
from LogOption import _SHOULD_LOG_SCOPE
import ValueObject as nVO


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

def define_type_aassignd_method(name:str):
    if name == nToken.TokenType.BOOL.name: #cast the val into a BOOl value
        return  lambda val : val != 0
    else:
        return lambda val : val



class EvalExprInfo(object):
    def __init__(self, is_integral:bool = False, constexpr_value = None):
        self.is_integral_ = is_integral
        self.constexpr_value_ = constexpr_value
        pass

    def get(self):
        return self.constexpr_value_
    
    def is_integral(self) -> bool:
        return self.is_integral_
    
    def is_constexpr(self)->bool:
        return self.constexpr_value_ != None
    
    def __add__(self, other):
        if self.get() == None or other.get() == None:
            return EvalExprInfo(is_both_integral(self, other))
        else:
            return EvalExprInfo(is_both_integral(self, other), self.get() + other.get())
    def __sub__(self, other):
        if self.get() == None or other.get() == None:
            return EvalExprInfo(is_both_integral(self, other))
        else:
            return EvalExprInfo(is_both_integral(self, other), self.get() - other.get())
    def __mul__(self, other):
        if self.get() == None or other.get() == None:
            return EvalExprInfo(is_both_integral(self, other))
        else:
            return EvalExprInfo(is_both_integral(self, other), self.get() * other.get())
    def __truediv__(self, other):
        if self.get() == None or other.get() == None:
            return EvalExprInfo(is_both_integral(self, other))
        else:
            return EvalExprInfo(is_both_integral(self, other), self.get() / other.get())

    def __floordiv__(self, other):
        if self.get() == None or other.get() == None:
            return EvalExprInfo(is_both_integral(self, other))
        else:
            return EvalExprInfo(is_both_integral(self, other), self.get() // other.get())        
   
    def __mod__(self, other):
        if self.get() == None or other.get() == None:
            return EvalExprInfo(is_both_integral(self, other))
        else:
            return EvalExprInfo(is_both_integral(self, other), self.get() % other.get())     

    def opr(self, other, bin_op):
        if self.get() == None or other.get() == None:
            return EvalExprInfo(is_both_integral(self, other))
        else:
            return EvalExprInfo(is_both_integral(self, other), bin_op(self.get(), other.get())) 

def is_both_integral(lhs:EvalExprInfo, rhs:EvalExprInfo):
    return lhs.is_integral() and rhs.is_integral()


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
        if (node.level == 1):
            if nToken.Compare(node.left.token, nToken.TokenType.IDENTIFIER) == False:
                self.error(error_code=nError.ErrorCode.UNEXPECTED_TOKEN, token=node.left.token) 
        
        self.visit(node.left)


    def visit_BinOp(self, node):
        if nToken.Compare(node.op, nToken.TokenType.PLUS):
            value = self.visit(node.left) + self.visit(node.right)
        elif nToken.Compare(node.op, nToken.TokenType.MINUS):
            value = self.visit(node.left) - self.visit(node.right)
        elif nToken.Compare(node.op, nToken.TokenType.MULTIPLY):
            value = self.visit(node.left) * self.visit(node.right)
        elif nToken.Compare(node.op, nToken.TokenType.INTEGER_DIV):
            value = self.visit(node.left) // self.visit(node.right)
        elif nToken.Compare(node.op, nToken.TokenType.FLOAT_DIV):
            value = float(self.visit(node.left)) / float(self.visit(node.right))
        elif nToken.Compare(node.op, nToken.TokenType.LOGIC_AND):
            value = EvalExprInfo(self.visit(node.left), self.visit(node.right), lambda x,y : x and y)
        elif nToken.Compare(node.op, nToken.TokenType.LOGIC_OR):
            value = EvalExprInfo(self.visit(node.left), self.visit(node.right), lambda x,y : x or y)
        elif nToken.Compare(node.op, nToken.TokenType.BIT_XOR):
            #not implemented
            self.visit(node.left)
            self.visit(node.right)
            value = 0
        elif nToken.Compare(node.op, nToken.TokenType.EQUAL):
            value = EvalExprInfo.opr(self.visit(node.left), self.visit(node.right), lambda x,y : x == y)
        elif nToken.Compare(node.op, nToken.TokenType.INEQUAL):
            value = EvalExprInfo.opr(self.visit(node.left), self.visit(node.right), lambda x,y : x != y)
        elif nToken.Compare(node.op, nToken.TokenType.LTE):
            value = EvalExprInfo.opr(self.visit(node.left), self.visit(node.right), lambda x,y : x <= y)
        elif nToken.Compare(node.op, nToken.TokenType.LT):
            value = EvalExprInfo.opr(self.visit(node.left), self.visit(node.right), lambda x,y : x < y)
        elif nToken.Compare(node.op, nToken.TokenType.GTE):
            value = EvalExprInfo.opr(self.visit(node.left), self.visit(node.right), lambda x,y : x >= y)
        elif nToken.Compare(node.op, nToken.TokenType.GT):
            value = EvalExprInfo.opr(self.visit(node.left), self.visit(node.right), lambda x,y : x > y)
        
        if node.right.value_type == nToken.TokenType.VOID.name:
            self.error(error_code=nError.ErrorCode.ASSIGNED_WITH_VOID, token=node.right.token)   

        node.type = self.detect_type_binop(node.op.type, node.left, node.right)

        return value

    def visit_Num(self, node):
        return EvalExprInfo(node.value_type == nToken.TokenType.INTEGER.name,
                            node.value)

    def visit_BoolVal(self, node):
        return EvalExprInfo(True, node.value)
    
    def visit_UnaryOp(self, node):
        if node.op.type == nToken.TokenType.PLUS.name:
            val = EvalExprInfo.opr(self.visit(node.expr), EvalExprInfo(True, 1), lambda x,y : x)
            node.value_type = nToken.TokenType.INTEGER.name
        elif node.op.type == nToken.TokenType.MINUS.name:
            val = EvalExprInfo.opr(self.visit(node.expr), EvalExprInfo(True, -1), lambda x,y : -x)
            node.value_type = nToken.TokenType.INTEGER.name
        elif node.op.type == nToken.TokenType.NOT.name:
            val = val = EvalExprInfo.opr(self.visit(node.expr), EvalExprInfo(True, 1), lambda x,y : not x)
            node.value_type = nToken.TokenType.INTEGER.name
        if self.compare_type(node.expr, nToken.TokenType.VOID):
            self.error(error_code=nError.ErrorCode.ASSIGNED_WITH_VOID, token=node.expr.token)
        return val
    
    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node):
        if node.op.type != nToken.TokenType.ASSIGN.name:
            self.error(error_code=nError.ErrorCode.UNEXPECTED_TOKEN, token=node.token)

        if node.op.value == ':=':
            self.visit(node.right)
            self.visit(node.left)
        elif node.op.value == '+=':
            self.visit(node.right)
            self.visit(node.left)
        elif node.op.value == '-=':
            self.visit(node.right)
            self.visit(node.left)
        elif node.op.value == '*=':
            self.visit(node.right)
            self.visit(node.left)
        elif node.op.value == '/=':
            self.visit(node.right)
            self.visit(node.left)
        else:
            self.error()
        if self.compare_type(node.right, nToken.TokenType.VOID):
            self.error(error_code=nError.ErrorCode.ASSIGNED_WITH_VOID, token=node.right.token)

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
        node.assign_method = define_type_aassignd_method(var_symbol.type)
        node.value_type = var_symbol.type
        return EvalExprInfo(node.value_type == nToken.TokenType.INTEGER.name)
        
    def visit_MemberAccess(self, node):
        var_name = node.left.token.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=nError.ErrorCode.ID_NOT_FOUND, token=node.left.token)
        
        if var_symbol.type.value_type == nToken.TokenType.ENUM.name:
            print(str(var_symbol.name) + " is enum!")
            if node.right.right != None:
                self.error(error_code=nError.ErrorCode.UNEXPECTED_TOKEN, token=node.right.right.left.token)
            if var_symbol.member_set.get(node.right.left.token.value) == None:
                self.error(error_code=nError.ErrorCode.UNKNOWN_ENUM_MEMBER, token=node.right.left.token)
            
            val = var_symbol.member_set[node.right.left.token.value]
            def func():
                return nVO.ValueObject(nVO.ValueObject.just_set, nVO.ValueObject.just_get, val)
            node.get_val_obj = func
            node.value_type = nToken.TokenType.INTEGER.name

            return EvalExprInfo(True, val)
        
        return EvalExprInfo()

    def visit_NoOp(self, node):
        pass

    def visit_Enum_def(self, node):
        print(node.token, len(node.member_pair_list))

        member_set = {}
        val = 0
        for pair in node.member_pair_list:
            if pair[1] != None:
                val = self.visit(pair[1]).get()
            member_set[pair[0].value] = val
            val += 1

        esym = Symbol.EnumSymbol(node.token.value, node, member_set)
        self.current_scope.insert(esym)
        return

    def visit_Declarations(self, node):
        for decl in node.decls:
            self.visit(decl)

    def visit_Type(self, node):
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code = nError.ErrorCode.ID_NOT_FOUND, token=node.token)
        
        product = 1
        
        for dim_size_expr in node.dimension_size_list:
            eval_expr_info = self.visit(dim_size_expr)
            if eval_expr_info.is_integral() == False:
                self.error(error_code = nError.ErrorCode.INVALID_ARRAY_SIZE_DEF, token=node.token)
            if eval_expr_info.is_constexpr() == False:
                self.error(error_code = nError.ErrorCode.INVALID_ARRAY_SIZE_DEF, token=node.token)
            if eval_expr_info.get() < 0:
                self.error(error_code = nError.ErrorCode.INVALID_ARRAY_SIZE_DEF, 
                           token="array size=" + str(eval_expr_info.get()) +", array size should be greater or equal to 0")         
            product *= eval_expr_info.get()
        
        if node.dimension > 0:
            node.array_len = product

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
            self.visit(node.return_val)
            while scope != None and scope.ar_tpye != nNodeVisitor.ARType.PROCEDURE and scope.ar_tpye != nNodeVisitor.ARType.PROGRAM:
                scope = scope.parent_scope
            ret_type_name = scope.lookup(scope.scope_name).return_type_node.token.value
            if ret_type_name == nToken.TokenType.VOID.name and node.return_val.type != nToken.TokenType.VOID.name:
                self.error(error_code = nError.ErrorCode.UNMATCHED_RETURN_VALUE, token=node.token)
            elif ret_type_name != nToken.TokenType.VOID.name and node.return_val.value_type == nToken.TokenType.VOID.name:
                self.error(error_code = nError.ErrorCode.UNMATCHED_RETURN_VALUE, token=node.token)
            

    def visit_VARs_decl(self, node):
        self.visit(node.type_node)
        type_symbol = self.current_scope.lookup(node.type_node.value)

        if type_symbol.type == nToken.TokenType.VOID.name:
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
            self.error(error_code=nError.ErrorCode.PRAR_COUNT_NOT_MATCHED, token=node.token)
        
        node.ref_procedure = proc_symbol
        node.value_type = proc_symbol.return_type_node.value

        for param_node in node.actual_params:
            self.visit(param_node)
        return EvalExprInfo()

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
    
    def compare_type(self, node, token_type:nToken.TokenType):
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

            if t1 == tup[1].name:
                score += 1
            if t2 == tup[2].name:
                score += 1
            if score > max_score:
                max_score = score
                closest_tup = tup

        return closest_tup[3].name
