class DataObject(object):
    def __init__(self, setter, getter, value = None):
        self.setter_fn = setter
        self.getter_fn = getter
        self.value = value
        self.address = -1

    def setter(self, val):
        self.setter_fn(self, val)
    
    def getter(self):
        return self.getter_fn(self)

    def set_addr(self, addr):
        self.address = addr

    def value_ref(self):
        return self.address

    def __str__(self):
        return str(self.value)
    
class ReferenceObject(DataObject):
    # value of a ReferenceObject is the referenced DataObject
    # data_space is a list of DataObject
    def __init__(self, data_space, data_obj:DataObject):
        self.data_space = data_space
        self.value = data_obj.value_ref()

    def setter(self, val):
        data_obj = self.data_space[self.value]
        data_obj.setter(val)
    
    def getter(self):
        data_obj = self.data_space[self.value]
        return data_obj.getter()

    def value_ref(self):
        return self.value
    

    def __str__(self):
        return "ref addr: " + str(self.value)

class ValueObject(DataObject):
    def __init__(self, setter, getter, value = None):
        super().__init__(setter, getter, value)
    
    
def NaiveInitValueObject(value):
    def just_set(dst_obj, src_obj):
        dst_obj.value = src_obj.getter()

    def just_get(val_obj):
        return val_obj.value
    
    return ValueObject(just_set, just_get, value)

