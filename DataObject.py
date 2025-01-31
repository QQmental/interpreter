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
    def __init__(self, setter, getter, val_obj:ValueObject):
        self.setter_fn = setter
        self.getter_fn = getter
        self.val_obj = val_obj

    def setter(self, val):
        self.val_obj.setter(val)
    
    def getter(self):
        return self.val_obj.getter()