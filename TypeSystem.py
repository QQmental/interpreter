import TypeDescriptor as nTDS

# implicitly cast src to dst
def is_type_implicit_castable(dst, src):
    if dst.dimension != src.dimension:
        return False
    
    if dst.dimension_size_list != src.dimension_size_list:
        return False
    
    if dst.type_class != nTDS.TypeDescriptor.TypeClass.REFERENCE and src.type_class == nTDS.TypeDescriptor.TypeClass.REFERENCE:
        return is_type_implicit_castable(dst, src.nested_type_descriptor)
    
    if dst.type_class == nTDS.TypeDescriptor.TypeClass.REFERENCE and src.type_class == nTDS.TypeDescriptor.TypeClass.REFERENCE:
        return dst.nested_type_descriptor.is_type_equal(src.nested_type_descriptor)
    
    if dst.type_class == nTDS.TypeDescriptor.TypeClass.REFERENCE and src.type_class != nTDS.TypeDescriptor.TypeClass.REFERENCE:
        return False

    if dst.type_class == nTDS.TypeDescriptor.TypeClass.VOID and src.type_class != nTDS.TypeDescriptor.TypeClass.VOID:
        return False
    
    if dst.type_class != nTDS.TypeDescriptor.TypeClass.VOID and src.type_class == nTDS.TypeDescriptor.TypeClass.VOID:
        return False        

    if dst.type_class != src.type_class:    
        if dst.is_integral() and src.is_integral():
            if dst.type_class == nTDS.TypeDescriptor.TypeClass.ENUM:
                if src.type_class != nTDS.TypeDescriptor.TypeClass.ENUM: 
                    return False
            return True
        return False
    
    if dst.is_primitive() and src.is_primitive():
        return True
    
    if dst.name != src.name:
        return False
    
    return True


def is_assigned_value_type_compatible(dst:nTDS.TypeDescriptor, src:nTDS.TypeDescriptor)->bool:
    tds = dst.type_descriptor
    if tds.is_reference():
        tds = tds.nested_type_descriptor
        
    if is_type_implicit_castable(tds, src.type_descriptor) == False:
        return False
    
    return True