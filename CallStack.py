import DataObject as nDO

class ActivationRecord:
    return_value = None
    def __init__(self, name, type, nesting_level, space):
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.members = [nDO.NaiveInitValueObject(None)]*space
        self.members_name = [""] * space


    def store(self, offset, value, name = ""):
        self.members[offset] = value
        self.members_name[offset] = name

    def load(self, offset):
        return self.members[offset]

    def __getitem__(self, offset):
        return self.load(offset)

    def get(self, offset):
        return self.members.get(offset)

    def __str__(self):
        lines = [
            '{level}: {type} {name}'.format(
                level=self.nesting_level,
                type=self.type.value,
                name=self.name,
            )
        ]
        #for item in self.members:
        #    lines.append(f'   {item[1]:<20}: {item[0]}')
        for idx in range(0, len(self.members)):
            lines.append(f'{self.members_name[idx]:<20} {self.members[idx]}')
        s = '\n'.join(lines)
        return s

    def __repr__(self):
        return self.__str__()

class CallStack:
    def __init__(self):
        self._records = []

    def push(self, ar):
        self._records.append(ar)

    def pop(self):
        return self._records.pop()

    def top(self):
        return self._records[-1]
    
    def bot(self):
        return self._records[0]
      
    def lookup_data_obj(self, name:str):
        for ar in reversed(self._records):
            if ar.get(name) != None:
                return ar[name]
        return None 

    def __str__(self):
        s = '\n'.join(repr(ar) for ar in reversed(self._records))
        s = f'CALL STACK\n{s}\n'
        return s

    def __repr__(self):
        return self.__str__()