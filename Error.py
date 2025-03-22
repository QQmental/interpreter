from enum import Enum

class ErrorCode(Enum):
    UNEXPECTED_TOKEN = 'Unexpected token'
    ID_NOT_FOUND     = 'Identifier not found'
    DUPLICATE_ID     = 'Duplicate id found'
    REDEFINE_FUNC    = 'function redefinition'
    UNDEFINED_TYPE   = 'Undefined type' 
    PRAR_COUNT_NOT_MATCHED = 'The number of parameters is not matched'
    BREAK_ERROR = 'break should be in a loop'
    UNMATCHED_RETURN_VALUE = 'return value type is not compatible'
    INVALID_TYPE_OF_OBJ_DECLARATION = 'void object is not allowed'
    ASSIGNED_WITH_VOID = str.format("return value has type void, can't be used as a rvalue")
    INVALID_ARRAY_SIZE_DEF = 'invalid array size defined'
    INVALID_ENUM_BODY_DEF = 'member of enum should be greater than 0'
    INVALID_VARIABLE_INITILIZATION = 'invalid initialization of the variable'
    UNKNOWN_ENUM_MEMBER = 'unknown access of a enum member'
    PARAMETER_TYPE_MISMATCHED = 'parameter mismatched'
    REDECL_PROC_PARAMETER_TYPE_MISMATCHED = 'redeclared procedure parameter mismatched'
    REDECL_PROC_RETURB_TYPE_MISMATCHED = 'redeclared procedure return type mismatched'
    ANONYMOUS_PARAMETER = 'parameter should have name'
    ASSIGNMENT_INCOMPATIBLE = 'assigned with incompatible value'

class Error(Exception):
    def __init__(self, error_code=None, token=None, message=None):
        self.error_code = error_code
        self.token = token
        # add exception class name before the message
        self.message = f'{self.__class__.__name__}: {message}'

class LexerError(Error):
    pass

class ParserError(Error):
    pass

class SemanticError(Error):
    pass