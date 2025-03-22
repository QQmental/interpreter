import DataObject as nDO

class ActivationRecord:
    return_value = None
    def __init__(self, name, type, nesting_level, space_size, stack_pointer, data_space, data_names):
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.space_size = space_size
        self.stack_pointer = stack_pointer
        self.data_space = data_space
        self.data_names = data_names


    def store(self, offset, data_obj, name = ""):
        data_obj.address = self.offset_cal(offset)
        self.data_space[self.offset_cal(offset)] = data_obj
        self.data_names[self.offset_cal(offset)] = name

    def load(self, offset):
        return self.data_space[self.offset_cal(offset)]      


    def __getitem__(self, offset):
        return self.load(offset)

    def __str__(self):
        lines = [
            '{level}: {type} {name}'.format(
                level=self.nesting_level,
                type=self.type.value,
                name=self.name,
            )
        ]
        
        for idx in range(0, self.space_size):
            lines.append(f'{self.data_names[self.offset_cal(idx)]:<20} {self.load(idx)}')
        s = '\n'.join(lines)
        return s

    def offset_cal(self, offset):
        return self.stack_pointer - self.space_size + offset
    
    def __repr__(self):
        return self.__str__()

class DataSpace:
    def __init__(self):
        self._records = [nDO.NaiveInitValueObject(None)]*256
        self._records_name = [""] * 256
        self.environment_stack = []
        self.stack_top = 0

    def space(self):
        return self._records
    
    def data_names(self):
        return self._records_name

    def load(self, address:int):
        return self.space()[address]
    
    def store(self, data_obj, address:int):
        self.space()[address] = data_obj
        
    def push(self, ar):
        self.environment_stack.append(ar)
        self.stack_top += ar.space_size

    def pop(self):
        self.stack_top -= self.top().space_size
        return self.environment_stack.pop()

    def top(self):
        return self.environment_stack[-1]
    
    def bot(self):
        return self.environment_stack[0]

    def __str__(self):
        s = '\n'.join(repr(ar) for ar in reversed(self.environment_stack))
        s = f'CALL STACK\n{s}\n'
        return s

    def __repr__(self):
        return self.__str__()