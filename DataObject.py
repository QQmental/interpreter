class ValueObject(object):
    def __init__(self, setter, getter, value = None):
        self.setter_fn = setter
        self.getter_fn = getter
        self.value = value

    def setter(self, val):
        self.setter_fn(self, val)
    
    def getter(self):
        return self.getter_fn(self)
    
    def just_get(self):
        return self.value
    
    def just_set(self, val):
        self.value = val

class ReferenceObject(ValueObject):
    # value of a ReferenceObject is the referenced ValueObject
    def __init__(self, setter, getter, val_obj:ValueObject):
        super().__init__(setter, getter, val_obj)

    def setter(self, val):
        self.value.setter(val)
    
    def getter(self):
        return self.value.getter()