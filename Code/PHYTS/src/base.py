from collections import namedtuple
# Small wrapper to distinguish objects that are intended to end up in some section of a .inp file, and define equality and hashability in sensible ways.

types = {"parameters",
         "source",
         "material",
         "surface",
         "cell",
         "transform",
         "temperature",
         "mat_time_change"
         "magnetic_field",
         "neutron_magnetic_field",
         "mapped_magnetic_field",
         "uniform_electromagnetic_field",
         "mapped_electromagnetic_field",
         "delta_ray",
         "track_structure",
         "super_mirror",
         "elastic_option",
         "importance",
         "weight_window",
         "ww_bias",
         "forced_collisions",
         "repeated_collisions",
         "volume",
         "multiplier",
         "mat_name_color",
         "reg_name",
         "counter",
         "timer",
         "t-track",
         "t-cross",
         "t-point",
         "t-adjoint",
         "t-deposit",
         "t-deposit2",
         "t-heat",
         "t-yield",
         "t-product",
         "t-dpa",
         "t-let",
         "t-sed",
         "t-time",
         "t-interact",
         "t-dchain",
         "t-wwg",
         "t-wwbg",
         "t-volume",
         "t-gshow",
         "t-rshow",
         "t-3dshow"}


class PhitsObject:
    def __init__(self, name, index=None, **kwargs):
        self.index = index

        if name in types:
            self.name = name
        else:
            raise ValueError(f"Unrecognized PHITS type {name} in ")

        if kwargs:
            self.parameters = Parameters(**kwargs)

    def __eq__(self, other):
        if type(self) != type(other):
            return False
        else:
            return self.__dict__ == other.__dict__
    def __hash__(self):
        return hash(tuple(v for k, v in sorted(self.__dict__.items()) \
                          if (self not in v.__dict__.values() if hasattr(v, "__dict__") else True)))


class Parameters(PhitsObject): # A simple dictionary of variable-value pairs; necessary so we catch it in the base class
    def __init__(self, **kwargs):
        self.name = "parameters"
        for k,v in kwargs.items():
            setattr(self, k, v)
    def __getitem__(self, key):
        return self.__dict__[key]
    def empty(self):
        return True if self.__dict__ == {"name": "parameters"} else False
    def definition(self):
        inp = ""
        for var, val in  self.__dict__.items():
            if var != "name":
                inp += f"{var} = {val}\n"

        return inp
