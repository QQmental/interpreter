import Token as nToken
from enum import Enum
import TypeDescriptor as nTDS

class AST(object):
    pass

class ValueNode(AST):
    type_descriptor:nTDS.TypeDescriptor
    def __init__(self, type_descriptor = None):
        self.type_descriptor = type_descriptor

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


class Var(AST):
    """The Var node is constructed out of ID token."""
    def __init__(self, token, assign_method = None):
        self.token = token
        # value is the name of this varriable
        self.value = token.value
        #assign to this or from this var
        self.assign_method = assign_method
        self.access_method = None
        self.var_offset = 0

class Type(AST):
    type_descriptor = None
    def __init__(self, token:nToken.Token, dimension_size_expr_list, is_ref_type:bool):
        self.token = token
        self.value = token.value
        self.dimension_size_expr_list = dimension_size_expr_list
        if self.value == nToken.TokenType.INTEGER.name:
            self.type_descriptor = nTDS.TypeDescriptor(self.value, nTDS.TypeDescriptor.TypeClass.INTEGER)
        elif self.value == nToken.TokenType.REAL.name:
            self.type_descriptor = nTDS.TypeDescriptor(self.value, nTDS.TypeDescriptor.TypeClass.REAL)
        elif self.value == nToken.TokenType.BOOL.name:
            self.type_descriptor = nTDS.TypeDescriptor(self.value, nTDS.TypeDescriptor.TypeClass.BOOL)
        elif self.value == nToken.TokenType.VOID.name:
            self.type_descriptor = nTDS.TypeDescriptor(self.value, nTDS.TypeDescriptor.TypeClass.VOID)            
        elif self.value == nToken.TokenType.CHAR.name:
            self.type_descriptor = nTDS.TypeDescriptor(self.value, nTDS.TypeDescriptor.TypeClass.CHAR)
        elif self.value == nToken.TokenType.STRING.name:
            self.type_descriptor = nTDS.TypeDescriptor(self.value, nTDS.TypeDescriptor.TypeClass.STRING)               
        else:
            self.type_descriptor = nTDS.TypeDescriptor(self.value)
        
        if is_ref_type == True:
            ref_type_descriptor = nTDS.TypeDescriptor(self.value + "_ref", nTDS.TypeDescriptor.TypeClass.REFERENCE)
            ref_type_descriptor.nested_type_descriptor = self.type_descriptor
            self.type_descriptor = ref_type_descriptor


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

class Param(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

class ProcedureDecl(AST):
    def __init__(self, proc_name:Var, para_node, return_type_node, block_node):
        self.token = proc_name.token
        self.proc_name = proc_name.value
        self.para_node = para_node  # list[Param]
        self.return_type_node = return_type_node
        self.block_node = block_node


class ProcedureCall(ValueNode):
    def __init__(self, proc_name, actual_params, token):
        super().__init__()
        self.proc_name = proc_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        self.ref_procedure = None
        


class Program(AST):
    def __init__(self, node:Var, block_node):
        self.token = node.token
        self.name = node.token.value
        self.block_node = block_node
        self.ref_procedure = None        