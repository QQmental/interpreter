from enum import Enum
class TypeDescriptor(object):
    class TypeClass(Enum):
        INTEGER = 0
        REAL = 1
        BOOL = 2
        CHAR = 3
        STRING = 4
        VOID = 5
        PRIMITIVE = 6
        STRUCT = 7
        ENUM = 8
    
    dimension = 0
    dimension_size_list = None
    array_len = 0
    def __init__(self, name:str, type_class:TypeClass = None):
        self.name = name
        self.type_class = type_class
    def is_primitive(self):
        return self.type_class.value < self.TypeClass.PRIMITIVE.value
    def is_type_equal(self, other):
        if self.dimension != other.dimension:
            return False
        if self.dimension_size_list != other.dimension_size_list:
            return False
        if self.type_class != other.type_class:
            return False                
        if self.is_primitive() and other.is_primitive():
            return True
        if self.name != other.name:
            return False
        return True
        
    