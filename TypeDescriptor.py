import AST
class TypeDescriptor(object):
    def __init__(self, type_node:AST.Type = None):
        if type_node != None:
            self.dimension = type_node.dimension
            self.dimension_size_list = type_node.dimension_size_list
        else :
            self.dimension = 0
            self.dimension_size_list = []
    