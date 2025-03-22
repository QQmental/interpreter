import Token as nToken
from enum import Enum
import TypeDescriptor as nTDS

class AST(object):
    pass

class ValueNode(AST):
    type_descriptor:nTDS.TypeDescriptor
    def __init__(self, type_descriptor = None):
        self.type_descriptor = type_descriptor
        self.var_offset = 0

class BinOp(ValueNode):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class UnaryOp(ValueNode):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr

class Subscript(ValueNode):
    get_value_func = None
    def __init__(self, left, expr, level):
        self.left = left        
        self.inside = expr
        self.level = level


class Num(ValueNode):
    def __init__(self, token, type_class:nTDS.TypeDescriptor.TypeClass):
        super().__init__(nTDS.TypeDescriptor(token.type, type_class))
        self.token = token
        self.value = token.value

class BoolVal(ValueNode):
    def __init__(self, token):
        type_class = nTDS.TypeDescriptor.TypeClass.BOOL
        type_name = nToken.TokenType.BOOL.name

        super().__init__(nTDS.TypeDescriptor(type_name, type_class))
        self.token = token
        self.value = token.value == 'TRUE'

class StringVal(ValueNode):
    def __init__(self, token):
        type_class = nTDS.TypeDescriptor.TypeClass.STRING
        type_name = nToken.TokenType.STRING.name

        super().__init__(nTDS.TypeDescriptor(type_name, type_class))
        self.token = token
        self.value = token.value

class CharVal(ValueNode):
    def __init__(self, token):
        type_class = nTDS.TypeDescriptor.TypeClass.CHAR
        type_name = nToken.TokenType.CHAR.name

        super().__init__(nTDS.TypeDescriptor(type_name, type_class))
        self.token = token
        self.value = token.value

class Compound(AST):
    """Represents a 'BEGIN ... END' block"""
    def __init__(self):
        self.children = []


class NoOp(AST):
    type_descriptor:nTDS.TypeDescriptor
    def __init__(self, token):
        self.token = token
        self.type_descriptor = nTDS.TypeDescriptor("", nTDS.TypeDescriptor.TypeClass.VOID)
    pass


class MemberAccess(ValueNode):
    def __init__(self, dot_token, left, right):
        self.token = dot_token
        self.left = left
        self.right = right
        self.get_val_obj = None

class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(ValueNode):
    """The Var node is constructed out of ID token."""
    def __init__(self, token, assign_method = None):
        self.token = token
        # value is the name of this varriable
        self.value = token.value
        #assign to this or from this var
        self.assign_method = assign_method
        self.access_method = None

class RefVar(ValueNode):
    def __init__(self, token):
        self.token = token
        # value is the name of this varriable
        self.value = token.value


class Type(AST):
    type_descriptor = None
    def __init__(self, token:nToken.Token):
        self.token = token
        self.value = token.value
        self.dimension_size_expr_list = []
        self.nested_type = None
    
    def is_callable(self)->bool:
        return False

    def type_string(self)->str:
        arr_prefix = ""
        if self.dimension_size_expr_list != []:
            arr_prefix = "array_"

        if self.nested_type == None:
            return arr_prefix + self.value
        else:
            if self.type_descriptor.is_reference():
                return arr_prefix + "ref_" + self.nested_type.type_string()
            return arr_prefix

    def symbol_string(self)->str:
        t = self
        while t.nested_type != None:
            t = t.nested_type
        return t.value


class Param(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


class CallableProtocal(Type):
    def __init__(self, token, para_node, return_type_node:Type):
        super().__init__(token)
        self.para_node_ = para_node  # list[Param]
        self.return_type_node_ = return_type_node

    def is_callable(self)->bool:
        return True

    def para_node(self):
        return self.para_node_

    def return_type_node(self):
        return self.return_type_node_

    def symbol_string(self)->str:
        return ""

class EnumDecl(ValueNode):
    def __init__(self, token:nToken.Token, member_pair_list):
        super().__init__(nTDS.TypeDescriptor(token.value, nTDS.TypeDescriptor.TypeClass.ENUM))
        self.token = token
        self.member_pair_list = member_pair_list


#id_list:list[Var], type_node:Type
class VARsDecl(AST):
    def __init__(self, var_list, type_node:Type, initilized_value, assignment_symbol:str):
        self.var_list = var_list
        self.type_node = type_node
        self.initilized_value = initilized_value
        self.assignment_symbol = assignment_symbol

class Declarations(AST):
    def __init__(self, declarations):
        self.decls = declarations # a list of AST.VARsDecl, AST.EnumDecl, AST.ProcedureDecl

    def combine(self, src):
        self.decls += src.decls

class Block(AST):
    def __init__(self, declarations:Declarations, compound_statement:Compound):
        self.declarations = declarations
        self.compound_statement = compound_statement


class IfBlock(AST):
    def __init__(self, token, statement, condition = None):
        self.token = token
        self.condition = condition
        self.statement = statement


class Cond_statements(AST):
    def __init__(self, if_blocks):
        self.if_blocks = if_blocks


class WhileBlock(AST):
    def __init__(self, token, statement, condition = None):
        self.token = token
        self.condition = condition
        self.statement = statement    

class ForBlock(AST):
    def __init__(self, token, statement, var_decls = None, condition = None, post_statements = None):
        self.token = token
        self.statement = statement
        self.var_decls = var_decls
        self.condition = condition
        if post_statements != None:
            self.post_statements = post_statements
        else:
            self.post_statements = []

class Control_flow_statement(AST):
    class control_type(Enum):
        BREAK = 'BREAK'
        CONTINUE = 'CONTINUE'
        RETURN = 'RETURN'
    
    def __init__(self, token:nToken.Token, type:control_type, return_val:AST = None):
        self.token = token
        self.type = type
        self.return_val = return_val



class ProcedureDecl(AST):
    def __init__(self, proc_name:Var, protocal:CallableProtocal, block_node):
        self.token = proc_name.token
        self.proc_name = proc_name.value
        self.protocal = protocal
        self.block_node = block_node

    def para_node(self):
        return self.protocal.para_node()
    
    def return_type_node(self):
        return self.protocal.return_type_node()

    def is_defined(self)->bool:
        return self.block_node != None


class CallInvoked(ValueNode):
    def __init__(self, access_name, callee_src, actual_params, token):
        super().__init__()
        self.access_name = access_name
        self.callee_src = callee_src
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        

class Program(AST):
    def __init__(self, node:Var, block_node, declarations:Declarations):
        self.token = node.token
        self.name = node.token.value
        self.block_node = block_node
        self.declarations = declarations