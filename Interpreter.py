from LogOption import _SHOULD_LOG_STACK
import NodeVisitor as nNodeVisitor
import Token as nToken
import AST
import ValueObject as nVO
    

class CallStack:
    def __init__(self):
        self._records = []

    def push(self, ar):
        self._records.append(ar)

    def pop(self):
        return self._records.pop()

    def top(self):
        return self._records[-1]
    
    def lookup(self, name:str):
        for ar in reversed(self._records):
            if ar.get(name) != None:
                return ar
        return None
    
    def update_value(self, name:str, new_val):
        for ar in reversed(self._records):
            if ar.get(name) != None:
                ar[name] = new_val
                break

    def __str__(self):
        s = '\n'.join(repr(ar) for ar in reversed(self._records))
        s = f'CALL STACK\n{s}\n'
        return s

    def __repr__(self):
        return self.__str__()


class ActivationRecord:
    return_value = None
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
    

class Interpreter(nNodeVisitor.NodeVisitor):
    def __init__(self, tree):
        self.tree = tree
        self.call_stack = CallStack()
        self.break_flag = False
        self.continue_flag = False
        self.return_flag = False

    def log(self, msg):
        if _SHOULD_LOG_STACK:
            print(msg)

    def visit_Subscript(self, node):
        idx = 1
        product = 1
        sum = 0
        head = node
        while head.level != 1:
            head = head.left

        size_list = head.left.type_descriptor.dimension_size_list

        cur = node

        while cur != head.left:
            sum += product * self.visit(cur.inside).getter()
            product *= size_list[head.left.type_descriptor.dimension - idx]
            idx += 1
            cur = cur.left
        var_name = head.left.value
        ar = self.call_stack.lookup(var_name)

        def setter(val_obj, val):
            ar[var_name][sum] = head.left.assign_method(val)

        def getter(val_obj):
            return ar[var_name][sum]

        return nVO.ValueObject(setter, getter)       


    def visit_BinOp(self, node):
        if  nToken.Compare(node.op, nToken.TokenType.PLUS):
            value = self.visit(node.left).getter() + self.visit(node.right).getter()
        elif nToken.Compare(node.op, nToken.TokenType.MINUS):
            value = self.visit(node.left).getter() - self.visit(node.right).getter()
        elif nToken.Compare(node.op, nToken.TokenType.MULTIPLY):
            value = self.visit(node.left).getter() * self.visit(node.right).getter()
        elif nToken.Compare(node.op, nToken.TokenType.INTEGER_DIV):
            value = self.visit(node.left).getter() // self.visit(node.right).getter()
        elif nToken.Compare(node.op, nToken.TokenType.FLOAT_DIV):
            value = float(self.visit(node.left).getter()) / float(self.visit(node.right).getter())
        elif nToken.Compare(node.op, nToken.TokenType.LOGIC_AND):
            value = self.visit(node.left).getter() and self.visit(node.right).getter()
        elif nToken.Compare(node.op, nToken.TokenType.LOGIC_OR):
            value = self.visit(node.left).getter() or self.visit(node.right).getter()
        elif nToken.Compare(node.op, nToken.TokenType.BIT_XOR):
            #not implemented
            self.visit(node.left).getter()
            self.visit(node.right).getter()
            value = 0
        elif nToken.Compare(node.op, nToken.TokenType.EQUAL):
            value = self.visit(node.left).getter() == self.visit(node.right).getter()
        elif nToken.Compare(node.op, nToken.TokenType.INEQUAL):
            value = self.visit(node.left).getter() != self.visit(node.right).getter()  
        elif nToken.Compare(node.op, nToken.TokenType.LTE):
            value = self.visit(node.left).getter() <= self.visit(node.right).getter()
        elif nToken.Compare(node.op, nToken.TokenType.LT):
            value = self.visit(node.left).getter() < self.visit(node.right).getter()
        elif nToken.Compare(node.op, nToken.TokenType.GTE):
            value = self.visit(node.left).getter() >= self.visit(node.right).getter()
        elif nToken.Compare(node.op, nToken.TokenType.GT):
            value = self.visit(node.left).getter() > self.visit(node.right).getter()
        return nVO.ValueObject(nVO.ValueObject.just_set, nVO.ValueObject.just_get, value)

    def visit_Num(self, node):
        def setter(val_obj, val):
            val_obj.value = val
        def getter(val_obj):
            return val_obj.value

        return nVO.ValueObject(setter, getter, node.value)

    def visit_BoolVal(self, node):   
        def setter(val_obj, val):
            val_obj.value = val == True
        def getter(val_obj):
            return val_obj.value
        
        return nVO.ValueObject(setter, getter, node.value)

    def visit_StringVal(self, node):
        def setter(val_obj, val):
            pass
        def getter(val_obj):
            return val_obj.value
        
        return nVO.ValueObject(setter, getter, node.value)

    def visit_CharVal(self, node):
        def setter(val_obj, val):
            pass
        def getter(val_obj):
            return val_obj.value
        
        return nVO.ValueObject(setter, getter, node.value)

    def visit_UnaryOp(self, node):
        val_obj = self.visit(node.expr)

        if nToken.Compare(node.op, nToken.TokenType.PLUS):
            val_obj.setter(val_obj.getter() * 1)
        elif nToken.Compare(node.op, nToken.TokenType.MINUS):
            val_obj.setter(val_obj.getter() * -1)
        elif nToken.Compare(node.op, nToken.TokenType.NOT):
            val_obj.setter(not val_obj.getter())
        return val_obj

    def visit_Compound(self, node):
        for child in node.children:
            if self.break_flag == True or self.continue_flag == True or self.return_flag == True:
                break
            self.visit(child)

    def visit_Assign(self, node):
        val_obj = self.visit(node.left)
        right_val = self.visit(node.right).getter()
        if node.op.value == ':=' or node.op.value == '=':
            val_obj.setter(right_val)                 
        else:
            if node.op.value == '+=':
                val_obj.setter(val_obj.getter() + right_val)
            elif node.op.value == '-=':
                val_obj.setter(val_obj.getter() - right_val)
            elif node.op.value == '*=':
                val_obj.setter(val_obj.getter() * right_val)
            elif node.op.value == '/=':
                val_obj.setter(val_obj.getter() / right_val)                                
    
    def visit_Var(self, node):
        var_name = node.value

        def setter(val_obj, val):
            ar = self.call_stack.lookup(var_name)
            ar[var_name] = node.assign_method(val)

        def getter(val_obj):
            ar = self.call_stack.lookup(var_name)
            return ar[var_name]
        
        return nVO.ValueObject(setter, getter)

    def visit_MemberAccess(self, node):
        return node.get_val_obj()


    def visit_NoOp(self, node):
        pass

    def visit_EnumDecl(self, node):
        pass

    def visit_Declarations(self, node):
        # Do nothing
        for decl in node.decls:
            self.visit(decl)
        pass

    def visit_Type(self, node):
        # Do nothing
        pass

    def visit_Control_flow_statement(self, node):
        if  node.type == AST.Control_flow_statement.control_type.BREAK:
            self.break_flag = True
        elif  node.type == AST.Control_flow_statement.control_type.CONTINUE:
            self.continue_flag = True
        elif node.type == AST.Control_flow_statement.control_type.RETURN:
            value = self.visit(node.return_val).getter()
            self.return_flag = True
            for ar in reversed(self.call_stack._records):
                if ar.type == nNodeVisitor.ARType.PROCEDURE:
                    ar.return_value = nVO.ValueObject(nVO.ValueObject.just_set,
                                                      nVO.ValueObject.just_get,
                                                      value)
                    break

    def visit_VARsDecl(self, node):
        for var in node.var_list:
            var_name = var.value
            if node.type_node.type_descriptor.array_len == 0:
                self.call_stack.top()[var_name] = 0
                if node.initilized_value != None:
                    val_obj = self.visit(var)
                    right_val = self.visit(node.initilized_value).getter()
                    val_obj.setter(right_val)
            else:
                self.call_stack.top()[var_name] = [0] * node.type_node.type_descriptor.array_len

    def visit_ProcedureDecl(self, node):
        pass

    def visit_IfBlock(self, node:AST.IfBlock):
        self.log(f'ENTER: IF cond{node.token}')

        if node.condition != None and self.visit(node.condition).getter() == False:
            return False

        self.log(f'ENTER: IF {node.token}')
        ar = ActivationRecord(
            name = node.token,
            type = nNodeVisitor.ARType.IF,
            nesting_level = self.call_stack.top().nesting_level + 1,
        )
        self.call_stack.push(ar)
        
        self.visit(node.statement_list)
        self.log(f'LEAVE: IF {node.token}')
        self.log(str(self.call_stack))
        self.call_stack.pop()

        return True

    def visit_Cond_statements(self, node):
        for cond_block in node.if_blocks:
            if self.visit(cond_block) == True:
                break
        
    def visit_WhileBlock(self, node):
        while self.visit(node.condition).getter() != False:
            self.log(f'ENTER: WHILE {node.token}')
            ar = ActivationRecord(
                name = node.token,
                type = nNodeVisitor.ARType.LOOP,
                nesting_level = self.call_stack.top().nesting_level + 1,
            )
            self.call_stack.push(ar)

            self.visit(node.statement_list)
            self.log(f'LEAVE: WHILE {node.token}')
            self.log(str(self.call_stack))
            self.call_stack.pop()

            if self.continue_flag == True:
                self.continue_flag = False
            elif self.break_flag == True:
                self.break_flag = False
                break
    
    def visit_Block(self, node):
        self.visit(node.declarations)
        self.visit(node.compound_statement)
    
    def visit_ProcedureCall(self, node):
        self.log(f'ENTER: PROCEDURE {node.proc_name}')

        ar = ActivationRecord(
            name = node.proc_name,
            type = nNodeVisitor.ARType.PROCEDURE,
            nesting_level = self.call_stack.top().nesting_level + 1,
        )

        procedure = node.ref_procedure

        for idx, para_node in enumerate(procedure.params):
            ar[para_node.name] = self.visit(node.actual_params[idx]).getter()

        self.call_stack.push(ar)
        self.visit(procedure.block_node)
        if self.return_flag == True:
            self.return_flag = False

        self.log(f'LEAVE: PROCEDURE {node.proc_name}')
        self.log(str(self.call_stack))
        return_value = self.call_stack.top().return_value
        self.call_stack.pop()    

        return return_value

    def visit_Program(self, node):
        self.program_name = node.name
        self.log(f'ENTER: PROGRAM {self.program_name}')

        ar = ActivationRecord(
            name = self.program_name,
            type = nNodeVisitor.ARType.PROGRAM,
            nesting_level = 1,
        )
        self.call_stack.push(ar)

        self.log(str(self.call_stack))
        self.visit(node.block)

        self.log(f'LEAVE: PROGRAM {self.program_name}')
        self.log(str(self.call_stack))
        print(str(self.call_stack))

        self.call_stack.pop()

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ''
        return self.visit(tree)