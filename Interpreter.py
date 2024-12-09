from enum import Enum
from LogOption import _SHOULD_LOG_STACK
from NodeVisitor import NodeVisitor
from Token import TokenType

class ARType(Enum):
    PROGRAM   = 'PROGRAM'
    PROCEDURE = 'PROCEDURE'

class CallStack:
    def __init__(self):
        self._records = []

    def push(self, ar):
        self._records.append(ar)

    def pop(self):
        return self._records.pop()

    def top(self):
        return self._records[-1]

    def __str__(self):
        s = '\n'.join(repr(ar) for ar in reversed(self._records))
        s = f'CALL STACK\n{s}\n'
        return s

    def __repr__(self):
        return self.__str__()


class ActivationRecord:
    def __init__(self, name, type, nesting_level):
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.members = {}

    def __setitem__(self, key, value):
        self.members[key] = value

    def __getitem__(self, key):
        return self.members[key]

    def get(self, key):
        return self.members.get(key)

    def __str__(self):
        lines = [
            '{level}: {type} {name}'.format(
                level=self.nesting_level,
                type=self.type.value,
                name=self.name,
            )
        ]
        for name, val in self.members.items():
            lines.append(f'   {name:<20}: {val}')

        s = '\n'.join(lines)
        return s

    def __repr__(self):
        return self.__str__()
    

class Interpreter(NodeVisitor):
    def __init__(self, tree):
        self.tree = tree
        self.call_stack = CallStack()

    def log(self, msg):
        if _SHOULD_LOG_STACK:
            print(msg)

    def visit_BinOp(self, node):
        if node.op.type == TokenType.PLUS.name:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == TokenType.MINUS.name:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == TokenType.MULTIPLY.name:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == TokenType.INTEGER_DIV.name:
            return self.visit(node.left) // self.visit(node.right)
        elif node.op.type == TokenType.FLOAT_DIV.name:
            return float(self.visit(node.left)) / float(self.visit(node.right))

    def visit_Num(self, node):
        return node.value

    def visit_UnaryOp(self, node):
        if node.op.type == TokenType.PLUS.name:
            return self.visit(node.expr)
        elif node.op.type == TokenType.MINUS.name:
            return -1 * self.visit(node.expr)

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node):
        var_name = node.left.value
        ar = self.call_stack.top()
        if node.op.value == ':=':
            ar[var_name] = self.visit(node.right)
        elif node.op.value == '+=':
            ar[var_name] = ar[var_name] + self.visit(node.right)
        elif node.op.value == '-=':
            ar[var_name] = ar[var_name] - self.visit(node.right)
        elif node.op.value == '*=':
            ar[var_name] = ar[var_name] * self.visit(node.right)
        elif node.op.value == '/=':
            ar[var_name] = ar[var_name] / self.visit(node.right)
        else:
            self.error()

    def visit_Var(self, node):
        var_name = node.value
        ar = self.call_stack.top()
        val = ar[var_name]
        return val

    def visit_L_Var(self, node):
        var_name = node.value
        ar = self.call_stack.top()
        val = ar[var_name]
        return val
    
    def visit_R_Var(self, node):
        var_name = node.value
        ar = self.call_stack.top()
        val = ar[var_name]
        return val

    def visit_NoOp(self, node):
        pass

    def visit_Declarations(self, node):
        # Do nothing
        pass

    def visit_Type(self, node):
        # Do nothing
        pass

    def visit_VARs_decl(self, node):
        pass

    def visit_ProcedureDecl(self, node):
        pass

    def visit_Block(self, node):
        self.visit(node.declarations)
        self.visit(node.compound_statement)
    
    def visit_ProcedureCall(self, node):
        self.log(f'ENTER: PROCEDURE {node.proc_name}')

        ar = ActivationRecord(
            name = node.proc_name,
            type = ARType.PROCEDURE,
            nesting_level = self.call_stack.top().nesting_level + 1,
        )

        procedure = node.ref_procedure

        for idx, para_node in enumerate(procedure.params):
            ar[para_node.name] = self.visit(node.actual_params[idx])

        self.call_stack.push(ar)
        self.visit(procedure.block_node)

        self.log(f'LEAVE: PROCEDURE {node.proc_name}')
        self.log(str(self.call_stack))
        self.call_stack.pop()
    
        pass

    def visit_Program(self, node):
        self.program_name = node.name
        self.log(f'ENTER: PROGRAM {self.program_name}')

        ar = ActivationRecord(
            name = self.program_name,
            type = ARType.PROGRAM,
            nesting_level = 1,
        )
        self.call_stack.push(ar)

        self.log(str(self.call_stack))

        self.visit(node.block)

        self.log(f'LEAVE: PROGRAM {self.program_name}')
        self.log(str(self.call_stack))

        self.call_stack.pop()

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ''
        return self.visit(tree)