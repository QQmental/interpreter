from LogOption import _SHOULD_LOG_STACK
import NodeVisitor as nNodeVisitor
import Token as nToken
import AST
import DataObject as nDO
import DataSpace as nDataSpace
import TypeDescriptor as nTDS

class Interpreter(nNodeVisitor.NodeVisitor):
    def __init__(self, tree, allocated_sym_list):
        self.tree = tree
        self.allocated_sym_list = allocated_sym_list
        self.data_space = nDataSpace.DataSpace()
        self.break_flag = False
        self.continue_flag = False
        self.return_flag = False
        self.return_value = None

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

        var = head.left
        start_data_obj = var.access_method(self.data_space)

        val_obj = self.data_space.load(start_data_obj.address + sum)
        return val_obj

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
        return nDO.NaiveInitValueObject(value)

    def visit_Num(self, node):
        def setter(val_obj, src_data_obj):
            val_obj.value = src_data_obj.getter()
        def getter(val_obj):
            return val_obj.value

        return nDO.ValueObject(setter, getter, node.value)

    def visit_BoolVal(self, node):   
        def setter(val_obj, src_data_obj):
            val_obj.value = src_data_obj.getter() == True
        def getter(val_obj):
            return val_obj.value
        
        return nDO.ValueObject(setter, getter, node.value)

    def visit_StringVal(self, node):
        def setter(val_obj, src_data_obj):
            pass
        def getter(val_obj):
            return val_obj.value
        
        return nDO.ValueObject(setter, getter, node.value)

    def visit_CharVal(self, node):
        def setter(val_obj, src_data_obj):
            pass
        def getter(val_obj):
            return val_obj.value
        
        return nDO.ValueObject(setter, getter, node.value)

    def visit_UnaryOp(self, node):
        data_obj = self.visit(node.expr)

        if nToken.Compare(node.op, nToken.TokenType.PLUS):
            data_obj.setter(nDO.NaiveInitValueObject(data_obj.getter() * 1))
        elif nToken.Compare(node.op, nToken.TokenType.MINUS):
            data_obj.setter(nDO.NaiveInitValueObject(data_obj.getter() * -1))
        elif nToken.Compare(node.op, nToken.TokenType.NOT):
            data_obj.setter(nDO.NaiveInitValueObject(not data_obj.getter()))
        return data_obj

    def visit_Compound(self, node):
        for child in node.children:
            if self.break_flag == True or self.continue_flag == True or self.return_flag == True:
                break
            self.visit(child)

    def visit_Assign(self, node):
        data_obj = self.visit(node.left)
        src_data_obj = self.visit(node.right)
        right_val = src_data_obj.getter()
        if node.op.value == ':=' or node.op.value == '=':
            data_obj.setter(src_data_obj)                 
        else:
            if node.op.value == '+=':
                data_obj.setter(nDO.NaiveInitValueObject(data_obj.getter() + right_val))
            elif node.op.value == '-=':
                data_obj.setter(nDO.NaiveInitValueObject(data_obj.getter() - right_val))
            elif node.op.value == '*=':
                data_obj.setter(nDO.NaiveInitValueObject(data_obj.getter() * right_val))
            elif node.op.value == '/=':
                data_obj.setter(nDO.NaiveInitValueObject(data_obj.getter() / right_val))      
    
    def visit_Var(self, node):
        return node.access_method(self.data_space)

    def visit_MemberAccess(self, node):
        return node.get_val_obj()


    def visit_NoOp(self, node):
        pass

    def visit_EnumDecl(self, node):
        pass

    def visit_Declarations(self, node):
        for decl in node.decls:
            self.visit(decl)
        pass

    def visit_Type(self, node):
        pass

    def visit_CallableProtocal(self, node):
        pass

    def visit_Control_flow_statement(self, node):
        if  node.type == AST.Control_flow_statement.control_type.BREAK:
            self.break_flag = True
        elif  node.type == AST.Control_flow_statement.control_type.CONTINUE:
            self.continue_flag = True
        elif node.type == AST.Control_flow_statement.control_type.RETURN:
            value = node.return_val.assign_method(self.data_space.space(), self.visit(node.return_val))
            self.return_flag = True
            self.return_value = value
            
            
    def visit_VARsDecl(self, node):
        
        def create_setter(var):
            def setter(data_obj, src_data_obj):
                data_obj.value = var.assign_method(self.data_space.space(), src_data_obj).getter()
            return setter
        
        for var in node.var_list:
            var_name = var.value
            
            def setter(data_obj, src_data_obj):
                data_obj.value = var.assign_method(self.data_space.space(), src_data_obj).getter()

            def getter(data_obj):
                return data_obj.value 
        
            if node.type_node.type_descriptor.array_len == 0:             
                if node.initilized_value != None:
                    right_data_obj = self.visit(node.initilized_value)
                    self.data_space.top().store(var.var_offset, var.assign_method(self.data_space.space(), right_data_obj), var_name)
                else:
                    self.data_space.top().store(var.var_offset, nDO.ValueObject(setter, getter, 0), var_name)
            else:
                #self.data_space.top().store(var.var_offset, nDO.ValueObject(setter, getter, [0] * node.type_node.type_descriptor.array_len), var_name)
                for idx in range(0, node.type_node.type_descriptor.array_len):
                    self.data_space.bot().store(var_name.var_offset + idx, nDO.ValueObject(setter, getter, 0), var_name + "[" + str(idx)+"]")

    def visit_ProcedureDecl(self, node):
        pass

    def visit_IfBlock(self, node:AST.IfBlock):
        self.log(f'ENTER: IF cond{node.token}')

        if node.condition != None and self.visit(node.condition).getter() == False:
            return False

        self.visit(node.statement)
        self.log(f'LEAVE: IF {node.token}')
        self.log(str(self.data_space))

        return True

    def visit_Cond_statements(self, node):
        for cond_block in node.if_blocks:
            if self.visit(cond_block) == True:
                break
        
    def visit_WhileBlock(self, node):
        self.log(f'ENTER: WHILE {node.token}')
        while self.visit(node.condition).getter() != False:
            self.visit(node.statement)

            if self.continue_flag == True:
                self.continue_flag = False
            elif self.break_flag == True:
                self.break_flag = False
                break
            elif self.return_flag == True:
                break
            
        self.log(f'LEAVE: WHILE {node.token}')
        self.log(str(self.data_space))
    
    def visit_ForBlock(self, node):
        self.log(f'ENTER: FOR {node.token}')
           
        for decl in node.var_decls:
            self.visit(decl)

        while self.visit(node.condition).getter() != False:
            self.visit(node.statement)
            if self.break_flag == True:
                self.break_flag = False
                break
            if self.continue_flag == True:
                self.continue_flag = False
            for post_statement in node.post_statements:
                self.visit(post_statement)
            if self.return_flag == True:
                break
        self.log(f'LEAVE: FOR {node.token}')
        self.log(str(self.data_space))    
        pass

    def visit_Block(self, node):
        self.visit(node.declarations)
        self.visit(node.compound_statement)
    
    def visit_CallInvoked(self, node):
        self.log(f'ENTER: PROCEDURE {node.access_name}')
        
        self.visit(node.callee_src)
        ref_callable_symbol = self.visit(node.callee_src).getter()

        space_size = ref_callable_symbol.max_var_count

        ar = nDataSpace.ActivationRecord(
            name = node.access_name,
            type = nNodeVisitor.ARType.PROCEDURE,
            nesting_level = self.data_space.top().nesting_level + 1,
            space_size = space_size,
            stack_pointer= self.data_space.stack_top + space_size,
            data_space = self.data_space.space(),
            data_names = self.data_space.data_names()
        )

        procedure = ref_callable_symbol

        for idx in range(0, len(procedure.params)):
            para_node = procedure.params[idx]
            assign_method = procedure.assign_methods[idx]
            para_data_obj = self.visit(node.actual_params[idx])
            arg_data_obj = assign_method(self.data_space.space(), para_data_obj)
            ar.store(para_node.var_offset, arg_data_obj, para_node.name)

        self.data_space.push(ar)
        self.visit(procedure.block_node)
        if self.return_flag == True:
            self.return_flag = False

        self.log(f'LEAVE: PROCEDURE {node.access_name}')
        self.log(str(self.data_space))
        return_value = self.return_value
        self.data_space.pop()

        return return_value

    def visit_Program(self, node):
        self.program_name = node.name
        self.log(f'ENTER: PROGRAM {self.program_name}')
        space_size = self.allocated_sym_list.max_var_count
        
        ar = nDataSpace.ActivationRecord(
            name = self.program_name,
            type = nNodeVisitor.ARType.PROGRAM,
            nesting_level = 1,
            space_size = space_size,
            stack_pointer= self.data_space.stack_top + space_size,
            data_space = self.data_space.space(),
            data_names = self.data_space.data_names()            
        )
        self.data_space.push(ar)

        self.log(str(self.data_space))

        def create_setter(sym):
            def func(data_obj, src_data_obj):
                data_obj.value = sym.assign_method(self.data_space.space(), src_data_obj).getter()
            return func
 

        for sym in self.allocated_sym_list.data_init_info_list():
            name = sym.var_name
            
            def immu_setter(data_obj, src_data_obj):
                pass

            def getter(data_obj):
                return data_obj.value

            setr = create_setter(sym)

            if sym.expr_info.type_descriptor.array_len == 0:
                if sym.expr_info.get() == None and sym.initialize_value == None:
                    self.data_space.bot().store(sym.var_offset, nDO.ValueObject(setr, getter, 0), name)
                else:
                    if sym.expr_info.type_descriptor.type_class == nTDS.TypeDescriptor.TypeClass.CallAble:
                        right_data_obj = nDO.ValueObject(immu_setter, getter, sym.expr_info.get())
                        self.data_space.bot().store(sym.var_offset, right_data_obj, name)
                    elif sym.expr_info.type_descriptor.is_reference():
                        data_obj = self.data_space.bot().load(sym.initialize_value.var_offset)
                        ref_obj = nDO.ReferenceObject(self.data_space.space(), data_obj)
                        self.data_space.bot().store(sym.var_offset, ref_obj, name)
                    else:
                        right_data_obj = self.visit(sym.initialize_value)
                        self.data_space.bot().store(sym.var_offset, sym.assign_method(self.data_space.space(), right_data_obj), name)
            else:
                for idx in range(0, sym.expr_info.type_descriptor.array_len):
                    self.data_space.bot().store(sym.var_offset + idx, nDO.ValueObject(setr, getter, 0), name + "[" + str(idx)+"]")


        ref_callable_symbol = self.data_space.load(node.var_offset).getter()
        self.visit(ref_callable_symbol.block_node.compound_statement)

        self.log(f'LEAVE: PROGRAM {self.program_name}')
        self.log(str(self.data_space))
        print(str(self.data_space))

        self.data_space.pop()

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ''

        return self.visit(tree)