from enum import Enum
class TypeDescriptor(object):
    class TypeClass(Enum):
        INTEGER = 0
        REAL = 1
        BOOL = 2
        CHAR = 3
        m_INTEGER = 4
        STRING = 5
        VOID = 6
        m_PRIMITIVE = 7
        REFERENCE = 8
        STRUCT = 9
        ENUM = 10
        CallAble = 11
        ARRAY = 12
    
    dimension = 0
    dimension_size_list = None
    array_len = 0
    nested_type_descriptor = None
    param_type_list = []
    return_type_descriptor = None

    def __init__(self, name:str, type_class:TypeClass = None):
        self.name = name
        self.type_class = type_class

    def has_symbol(self)->bool:
        return self.name != ""

    def is_primitive(self):
        return self.type_class.value < self.TypeClass.m_PRIMITIVE.value
    
    def is_reference(self):
        return self.type_class == self.TypeClass.REFERENCE
    
    def is_integral(self):
        if self.is_reference():
            return self.nested_type_descriptor.is_integral()
        return self.type_class.value  < self.TypeClass.m_INTEGER.value or \
                self.type_class.value == self.TypeClass.ENUM.value 
    
    def is_type_equal(self, src):
        if self.dimension != src.dimension:
            return False
        
        if self.dimension_size_list != src.dimension_size_list:
            return False
        
        if self.is_primitive() and src.is_primitive():
            return self.type_class == src.type_class
    
        return self.type_class == src.type_class and self.name == src.name

        