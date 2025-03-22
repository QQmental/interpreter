
import TypeDescriptor as nTDS

class EvalExprInfo(object):
    def __init__(self, type_descriptor:nTDS.TypeDescriptor, is_rvalue:bool, constexpr_value = None):
        self.type_descriptor = type_descriptor
        self.m_is_rvalue = is_rvalue
        self.m_is_link_needed = False
        self.constexpr_value_ = constexpr_value

    def get(self):
        return self.constexpr_value_
    
    def set(self, val):
        self.constexpr_value_ = val
    
    def type_class(self)->nTDS.TypeDescriptor.TypeClass:
        return self.type_descriptor.type_class
    
    def is_rvalue(self)->bool:
        return self.m_is_rvalue
    
    def is_integral(self)->bool:
        return self.type_descriptor.is_integral()
    
    def is_constexpr(self)->bool:
        return self.constexpr_value_ != None
    
    def is_link_needed(self)->bool:
        return self.m_is_link_needed

    def mark_link_needed(self):
        self.m_is_link_needed = True