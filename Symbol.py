import AST
import TypeDescriptor as nTDS

class Symbol(object):
    is_initialized = False
    type_descriptor = None
    def __init__(self, name:str, is_on_stack_symbol:bool, type_descriptor=None):
        self.name = name
        self.is_on_stack_symbol = is_on_stack_symbol
        self.type_descriptor = type_descriptor
    def type_class(self):
        return self.type_descriptor.type_class

class TypeSymbol(Symbol):
    def __init__(self, type_descriptor:nTDS.TypeDescriptor):
        super().__init__(type_descriptor.name, False, type_descriptor)
        self.type_descriptor = type_descriptor
    def __str__(self):
        return self.name

    __repr__ = __str__


class VarSymbol(Symbol):
    def __init__(self, name, is_on_stack_symbol:bool, type_node:AST.Type, var_offset:int):
        super().__init__(name, is_on_stack_symbol, type_node.type_descriptor)
        self.type_node = type_node
        self.var_offset = var_offset

    def __str__(self):
        return '<{name}:{type}>'.format(name=self.name, type=self.type_descriptor.name)

    __repr__ = __str__


class CallableSymbol(Symbol):
    def __init__(self, name, return_type_node:AST.Type, block_node:AST.Block, params=None):
        super().__init__(name, False)
        # params is a list of pair:(symbol of the parameter, assign_method)
        self.params = params if params is not None else []
        self.return_type_node = return_type_node        
        self.block_node = block_node
        self.max_var_count = 0


    def __str__(self):
        return '<{class_name}(name={name}, parameters={params})>'.format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.params,
        )

    __repr__ = __str__

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