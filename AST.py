import Token as nToken
from enum import Enum


class AST(object):
    pass

class ValueNode(AST):
    value_type:str
    def __init__(self, type):
        self.value_type = type

class BinOp(ValueNode):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right
        self.value_type = ""

class UnaryOp(ValueNode):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr
        self.value_type = ""

class Subscript(ValueNode):
    get_value_func = None
    def __init__(self, left, expr, level):
        self.left = left        
        self.inside = expr
        self.value_type = ""
        self.level = level
        self.value = "z"



class Num(ValueNode):
    def __init__(self, token, value_type:str):
        super().__init__(value_type)
        self.token = token
        self.value = token.value

class BoolVal(ValueNode):
    def __init__(self, token):
        super().__init__(nToken.TokenType.BOOL.name)
        self.token = token
        self.value = token.value == 'TRUE'

class Compound(AST):
    """Represents a 'BEGIN ... END' block"""
    def __init__(self):
        self.children = []


class NoOp(AST):
    type:str
    def __init__(self):
        self.type = nToken.TokenType.VOID.name
    pass


class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(AST):
    """The Var node is constructed out of ID token."""
    def __init__(self, token):
        self.token = token
        # value is the name of this varriable
        self.value = token.value 

class L_Var(AST):
    """The Var node is constructed out of ID token."""
    def __init__(self, token, assign_method = None):
        self.token = token
        # value is the name of this varriable
        self.value = token.value 

        self.assign_method = assign_method

class R_Var(ValueNode):
    """The Var node is constructed out of ID token."""
    def __init__(self, token, assign_method = None):
        self.token = token
        # value is the name of this varriable
        self.value = token.value 

        self.assign_method = assign_method

class Type(AST):
    def __init__(self, token:nToken.Token, dimension:int, dimension_size_list):
        self.token = token
        self.value = token.value
        self.dimension = dimension
        self.dimension_size_list = dimension_size_list
        self.array_len = 0

class Enum_def(AST):
    def __init__(self, token:nToken.Token, member_pair_list):
        self.token = token
        self.member_pair_list = member_pair_list


#id_list:list[Var], type_node:Type
class VARs_decl(AST):
    def __init__(self, var_list, type_node:Type):
        self.var_list = var_list
        self.type_node = type_node


class Declarations(AST):
    def __init__(self, declarations):
        self.decls = declarations # a list of AST.VARs_decl


class Block(AST):
    def __init__(self, declarations:Declarations, compound_statement:Compound):
        self.declarations = declarations
        self.compound_statement = compound_statement


class IfBlock(AST):
    def __init__(self, token, statement_list, condition = None):
        self.token = token
        self.condition = condition
        self.statement_list = statement_list


class Cond_statements(AST):
    def __init__(self, if_blocks):
        self.if_blocks = if_blocks


class WhileBlock(AST):
    def __init__(self, token, statement_list, condition = None):
        self.token = token
        self.condition = condition
        self.statement_list = statement_list    

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
        self.para_node = para_node # list[Param]
        self.return_type_node = return_type_node
        self.block_node = block_node


class ProcedureCall(ValueNode):
    def __init__(self, proc_name, actual_params, token, procedure:ProcedureDecl = None):
        super().__init__(nToken.TokenType.VOID.name)
        self.proc_name = proc_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        self.ref_procedure = procedure
        


class Program(AST):
    def __init__(self, name, block):
        self.name = name
        self.block = block