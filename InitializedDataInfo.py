import TypeSystem as nTST
import EvalExprInfo as nEVEXPR
import AST
import Symbol
from typing import Union

class InitializedDataInfo(object):
    #if initialize_value is None, no initialize value
    #if assign_method is None, the data is immuutable
    def __init__(self, var_name, var_offset:int, expr_info:nEVEXPR.EvalExprInfo, initialize_value:AST.AST, assign_method = None):
        self.var_name = var_name
        self.var_offset = var_offset
        self.expr_info = expr_info
        self.initialize_value = initialize_value
        self.assign_method = assign_method

class InitializedDataInfoTable(object):
    def __init__(self):
        self.max_var_count = 0
        self.data_init_info_list_ = []

    def data_init_info_list(self):
        return self.data_init_info_list_
    
    def append(self, info:InitializedDataInfo):
        self.data_init_info_list_.append(info)