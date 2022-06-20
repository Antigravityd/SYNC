from collections import namedtuple
# Small wrapper to distinguish objects that are intended to end up in some section of a .inp file, and define equality and hashability in sensible ways.

# Discursion on the object model is warranted.
# In general, the user will create an object using an __init__ with some arguments; some are required by PHITS for that
# subsection, some are optional. The non-inclusion of a particular argument in the input file is indicated by its value
# being None. Some objects have different parameter/attribute names in Python than in the PHITS input, either for better
# mnemonic ability than the 6-char-limited Fortran-style names they often use or because of differing identifier lexing rules.
# Similarly, options that take enumerated values would be well-served in having descriptive string names.
# There are two types of lines that are written to the input file: grid-like lines, which are just a list of whitespace-delimited
# values, and parameter-like lines, which are assignments of the form <identifier> = <value>.
# Some lines are position-dependent.

# The PhitsObject class is a factory. I want to suck-start a shotgun for the mere fact of having ever considered writing one, but
# it seems justified here. It has a "name" attribute that serves to identify which subsection of the .inp the object's definition
# ought to be placed, a "required" argument that contains  all of the required attributes, an "optional" argument that
# contains all the attributes that can be set but aren't required, a "positional" argument that contains a tuple of the
# required attribute names that are allowed to be specified positionally, an "ordering" argument that contains tuples
# that indicate mandatory sequencing of the given attributes' definitions in the .inp, an "ident_map" argument that contains
# the remapping of Python attribute names to what ought to appear in the .inp, and "value_map" is the same but for the values.
# Any attribute that's a tuple or frozenset
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

# args and kwargs here are pulled directly from the arguments to the __init__ of the subclass
class PhitsObject:
    def __init__(self, name, required, optional, positional, ordering, ident_map, value_map, index=None, *args, **kwargs):

        if name in types:
            self.name = name
        else:
            raise ValueError(f"Unrecognized PHITS type {name} in ")

        if len(args) == len(positional):
            for idx, arg in enumerate(args):
                setattr(self, positional[idx], arg if not isinstance(arg, list) else tuple(arg))
        else:
            raise TypeError(f"Too many positional arguments specified in the definition of {self.name} object.")

        for arg in required:
            if arg not in positional:
                if arg in kwargs:
                    setattr(self, arg, kwargs[arg] if not isinstance(kwargs[arg], list) else tuple(kwargs[arg]))
                else:
                    raise TypeError(f"Missing required argument in the definition of {self.name} object.")

        for arg in optional:
            if arg in kwargs:
                setattr(self, arg, kwargs[arg] if not isinstance(kwargs[arg], list) else tuple(kwargs[arg]))
            else:
                setattr(self, arg, None)

        self.ordering = ordering
        self.ident_map = ident_map
        self.value_map = value_map

        self.index = index

        remaining = {k: v for k, v in kwargs.items() if k not in required and k not in optional}
        if remaining:
            self.parameters = Parameters(**remaining)



    def definition(self):
        inp = ""
        def append(attr, val, string):
            if val is not None:
                    if isinstance(val, tuple): # grid-like section
                        for entry in val:
                            string += f"{entry} "
                        string += "\n"
                    else:
                        if attr in self.ident_map and val not in self.value_map:
                            string += f"{ident_map[attr]} = {val}\n"
                        elif attr not in self.ident_map and val in self.value_map:
                            string += f"{attr} = {self.value_map[val]}\n"
                        elif attr in self.ident_map and val in self.value_map:
                            string += f"{self.ident_map[attr]} = {self.value_map[val]}\n"
                        else:
                            string += f"{attr} = {val}\n"

        for attr, val in {k: v for k, v in self.__dict__.items() if k not in {"name", "ordering", "ident_map", "value_map", \
                                                                              "index", "parameters"}}:
            if attr not in it.chain.from_iterable(self.ordering): # freely orderable parameter
                append(attr, val, inp)


        for tup in self.ordering:
            for attr in tup:
                if attr in self.__dict__:
                    append(attr, self.__dict__[attr], inp)
                else:
                    raise ValueError(f"A {self.name} object's ordering argument {attr} isn't in the required or optional arguments.")

        return inp




    def __eq__(self, other):
        if type(self) != type(other):
            return False
        elif hasattr(self, "__dict__") and hasattr(other, "__dict__"):
            d1 = {k: v for k, v in self.__dict__.items() if k != "index"}
            d2 = {k: v for k, v in other.__dict__.items() if k != "index"}
            return d1 == d2

    def __hash__(self):
        return hash(tuple(v for k, v in sorted(self.__dict__.items()) \
                          if (self not in v.__dict__.values() if hasattr(v, "__dict__") else True) and k != "index"))


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
