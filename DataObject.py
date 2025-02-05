class ValueObject(object):
    def __init__(self, setter, getter, value = None):
        self.setter_fn = setter
        self.getter_fn = getter
        self.value = value

    def setter(self, val):
        self.setter_fn(self, val)
    
    def getter(self):
        return self.getter_fn(self)
    
    def __str__(self):
        return str(self.value)
    
def NaiveInitValueObject(value):
    
    def just_get(val_obj):
        return val_obj.value
    
    def just_set(val_obj, val):
        val_obj.value = val

    return ValueObject(just_set, just_get, value)

class ReferenceObject(object):
    # value of a ReferenceObject is the referenced ValueObject
    def __init__(self, val_obj:ValueObject):
        self.value = val_obj

    def setter(self, val):
        self.value.setter(val)
    
    def getter(self):
        return self.value.getter()
