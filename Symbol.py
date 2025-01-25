import AST

class Symbol(object):
    is_initialized = False
    def __init__(self, name:str, type=None):
        self.name = name
        self.type = type


class BuiltinTypeSymbol(Symbol):
    def __init__(self, name, type:str):
        super().__init__(name, type)

    def __str__(self):
        return self.name

    __repr__ = __str__


class VarSymbol(Symbol):
    def __init__(self, name, type:AST.Type):
        super().__init__(name, type)

    def __str__(self):
        return '<{name}:{type}>'.format(name=self.name, type=self.type.value)

    __repr__ = __str__


class ProcedureSymbol(Symbol):
    def __init__(self, name, return_type_node:AST.Type, block_node:AST.Block, params=None):
        super().__init__(name,)
        # a list of formal parameters
        self.params = params if params is not None else []
        self.return_type_node = return_type_node        
        self.block_node = block_node


    def __str__(self):
        return '<{class_name}(name={name}, parameters={params})>'.format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.params,
        )

    __repr__ = __str__

class EnumSymbol(Symbol):
    def __init__(self, name, type:AST.Enum_def, member_set):
        super().__init__(name, type)
        self.member_set = member_set

    def __str__(self):
        return '<{name}:{type}>'.format(name=self.name, type=self.type.token)

    __repr__ = __str__