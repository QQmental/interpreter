import AST
import TypeDescriptor as nTDS
import Token as nToken

class Symbol(object):
    is_initialized = False
    type_descriptor_ = None
    def __init__(self, name:str, type_descriptor=None):
        self.name = name
        self.type_descriptor_ = type_descriptor
        self.var_offset = -1


    def type_class(self):
        return self.type_descriptor().type_class
    
    def type_descriptor(self):
        return self.type_descriptor_

    def is_defined(self)->bool:
        return True
    
    def __str__(self):
        return self.name

    __repr__ = __str__

class TypeSymbol(Symbol):
    def __init__(self, type_descriptor:nTDS.TypeDescriptor):
        super().__init__(type_descriptor.name, type_descriptor)


class ReferableSymbol(Symbol):
    def __init__(self, name:str, is_on_stack_symbol:bool, type_descriptor=None):
        super().__init__(name, type_descriptor)
        self.is_on_stack_symbol = is_on_stack_symbol

class VarSymbol(ReferableSymbol):
    def __init__(self, name, is_on_stack_symbol:bool, type_node:AST.Type):
        super().__init__(name, is_on_stack_symbol, type_node.type_descriptor)
        self.type_node = type_node
        
    def __str__(self):
        return '<{name}:{type}>'.format(name=self.name, type=self.type_descriptor().name)

    __repr__ = __str__


class CallableSymbol(ReferableSymbol):
    def __init__(self, name, is_on_stack_symbol:bool, type_descriptor:nTDS.TypeDescriptor):
        super().__init__(name, is_on_stack_symbol, type_descriptor)
        # params is a list of symbol of the parameters
        self.params = []
        self.assign_methods = []

    def return_type_descriptor(self):
        return self.type_descriptor().return_type_descriptor

    def __str__(self):
        return '<(callable: {name})>'.format(
            name=self.name
        )

    __repr__ = __str__


class ProcSymbol(CallableSymbol):
    def __init__ (self, name, is_on_stack_symbol:bool, type_descriptor:nTDS.TypeDescriptor, params = None, assign_methods = None):
        super().__init__(name, is_on_stack_symbol, type_descriptor)
        self.params = params if params is not None else []
        self.assign_methods = assign_methods if assign_methods is not None else []
        self.block_node = None
        self.definition_token = None
        self.max_var_count = 0
    
    def set_definition(self, block_node:AST.Block, definition_token:nToken.Token):
        self.block_node = block_node
        self.definition_token = definition_token

    def is_defined(self):
        return self.block_node != None


class EnumSymbol(TypeSymbol):
    def __init__(self, type_node:AST.EnumDecl, member_set):
        type_class = nTDS.TypeDescriptor.TypeClass.ENUM
        type_descriptor = nTDS.TypeDescriptor(type_node.type_descriptor.name, type_class)
        super().__init__(type_descriptor)
        self.member_set = member_set
        self.type_node = type_node
    def __str__(self):
        return '<{name}:{type}>'.format(name=self.name, type=self.type_node.token)

    __repr__ = __str__