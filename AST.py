from Token import Token

class AST(object):
    pass


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class UnaryOp(AST):
     def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr


class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class Compound(AST):
    """Represents a 'BEGIN ... END' block"""
    def __init__(self):
        self.children = []


class NoOp(AST):
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
    def __init__(self, token):
        self.token = token
        # value is the name of this varriable
        self.value = token.value 

class R_Var(AST):
    """The Var node is constructed out of ID token."""
    def __init__(self, token):
        self.token = token
        # value is the name of this varriable
        self.value = token.value 


class Type(AST):
    def __init__(self, token:Token):
        self.token = token
        self.value = token.value


#id_list:list[Var], type_node:Type
class VARs_decl(AST):
    def __init__(self, var_list, type_node:Type):
        self.var_list = var_list
        self.type_node = type_node


class Declarations(AST):
    def __init__(self, declarations):
        self.decls = declarations


class Block(AST):
    def __init__(self, declarations:Declarations, compound_statement:Compound):
        self.declarations = declarations
        self.compound_statement = compound_statement


class Param(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


class ProcedureDecl(AST):
    def __init__(self, proc_name:Var, para_node, block_node):
        self.token = proc_name.token
        self.proc_name = proc_name.value
        self.para_node = para_node # list[Param]
        self.block_node = block_node


class ProcedureCall(AST):
    def __init__(self, proc_name, actual_params, token, procedure:ProcedureDecl = None):
        self.proc_name = proc_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        self.ref_procedure = procedure


class Program(AST):
    def __init__(self, name, block):
        self.name = name
        self.block = block