class DataObject(object):
    def __init__(self, setter, getter, value = None):
        self.setter_fn = setter
        self.getter_fn = getter
        self.value = value

    def setter(self, val):
        self.setter_fn(self, val)
    
    def getter(self):
        return self.getter_fn(self)

    def ref(self):
        pass

    def __str__(self):
        return str(self.value)
    
class ReferenceObject(DataObject):
    # value of a ReferenceObject is the referenced DataObject
    def __init__(self, val_obj:DataObject):
        self.value = val_obj.ref()

    def setter(self, val):
        self.value.setter(val)
    
    def getter(self):
        return self.value.getter()

    def ref(self):
        return self.value


class ValueObject(DataObject):
    def __init__(self, setter, getter, value = None):
        self.setter_fn = setter
        self.getter_fn = getter
        self.value = value
    
    def ref(self):
        return self
    
def NaiveInitValueObject(value):
    def just_set(dst_obj, src_obj):
        dst_obj.value = src_obj.getter()

    def just_get(val_obj):
        return val_obj.value
    
    return ValueObject(just_set, just_get, value)

