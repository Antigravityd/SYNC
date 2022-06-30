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
# required attribute names that are allowed to be specified positionally, a "shape" argument that contains a tuple
# that indicates the sequencing of the given attributes' definitions in the .inp (sub-tuples indicate the values of those
# parameters are to appear alone on a line, the bare values indicate a single assignment on the line, or a string literal
# on that line, if the given string is not in the attributes of the object), an "ident_map" argument that contains
# the remapping of Python attribute names to what ought to appear in the .inp, and "value_map" is the same but for the values.
# The "subobjects" parameter is a list of tuples of attributes that contain PhitsObjects that need a property set to the value
# of the current object if it's None in the subobject; the second element is the name of the property to set.
# The override parameter, if set, calls the object's override() method and appends the resulting string at the point
# in the shape array corresponding to the numeric value of the parameter.
# Any attribute that's a tuple or frozenset


# args and kwargs here are pulled directly from the arguments to the __init__ of the subclass
# Subclasses must have class parameters name, required, positional, optional, shape, ident_map, value_map, subobjects, and nones
class PhitsObject:
    required = []
    positional = []
    optional = []
    ident_map = dict()
    value_map = dict()
    subobjects = []
    nones = dict()
    index = None
    no_hash = {"index", "value_map", "ident_map", "nones", "shape", "subobjects", "required", "positional", "optional"}
    names = {"parameters", "source", "material", "surface", "cell", "transform", "temperature","mat_time_change","magnetic_field",
             "neutron_magnetic_field", "mapped_magnetic_field", "uniform_electromagnetic_field", "mapped_electromagnetic_field",
             "delta_ray", "track_structure", "super_mirror", "elastic_option", "importance", "weight_window", "ww_bias",
             "forced_collisions", "repeated_collisions", "volume", "multiplier", "mat_name_color", "reg_name", "counter", "timer",
             "t-track", "t-cross", "t-point", "t-adjoint", "t-deposit", "t-deposit2", "t-heat", "t-yield", "t-product", "t-dpa",
             "t-let", "t-sed", "t-time", "t-interact", "t-dchain", "t-wwg", "t-wwbg", "t-volume", "t-gshow", "t-rshow","t-3dshow"}

    def __init__(self, *args,  **kwargs):
        assert self.name in self.names, f"Unrecognized PHITS type {self.name} in PhitsObject initialization."

        if len(args) == len(self.positional):
            for idx, arg in enumerate(args):
                setattr(self, self.positional[idx], arg if not isinstance(arg, list) else tuple(arg))
        else:
            raise TypeError(f"Wrong number of positional arguments specified in the definition of {self.name} object.")

        for arg in self.required:
            if arg not in self.positional:
                if arg in kwargs:
                    setattr(self, arg, kwargs[arg] if not isinstance(kwargs[arg], list) else tuple(kwargs[arg]))
                else:
                    raise TypeError(f"Missing required argument in the definition of {self.name} object.")

        for arg in self.optional:
            if arg in kwargs:
                setattr(self, arg, kwargs[arg] if not isinstance(kwargs[arg], list) else tuple(kwargs[arg]))
            else:
                if arg in self.nones:
                    setattr(self, arg, self.nones[arg])
                else:
                    setattr(self, arg, None)


        for attr in self.subobjects:
            child = getattr(self, attr)
            if hasattr(child, self.name):
                val = getattr(child, self.name)
                if val is None:
                    setattr(child, self.name, self)



        remaining = {k: v for k, v in kwargs.items() if k not in self.required and k not in self.optional}
        if remaining:
            self.parameters = Parameters(**remaining)

        print({k: v for k, v in vars(self).items() if k not in self.no_hash})



    def definition(self):
        def idx(ob):
            if isinstance(ob, PhitsObject):
                return str(ob.index)
            else:
                return ob
        def attr_map(ob):
            if ob in self.ident_map:
                return self.ident_map[ob]
            else:
                return ob

        def val_map(ob):
            if ob in self.value_map:
                return self.value_map[ob]
            elif isinstance(ob, tuple):
                return " ".join(map(val_map, ob))
            elif isinstance(ob, Mesh):
                inp = f"{ob.axis[0]}-type = 1\n"
                inp += f"n{ob.axis[0]} = {len(ob.bins)-1}\n"
                for i in ob.bins:
                    inp += f"{i} "
                inp += "\n"
                return inp
            else:
                return ob



        def append(tup, app, assignments=True): # Recursion in the case of grid-like lines
            for attr in tup:
                assign = ""
                endstr = "\n" if assignments else " "
                spacing = " "
                if isinstance(attr, str) and attr[-1] == "\\":
                    endstr = " "
                    spacing = ""
                    attr = attr[:-1]

                if callable(attr):
                    app += attr(self)
                    app += endstr
                elif isinstance(attr, tuple):
                    append(attr, app, assignments=False)
                    if isinstance(attr[-1], str) and attr[-1][-1] == "\\":
                        app += " "
                    else:
                        app += "\n"
                elif hasattr(self, attr):
                    val = getattr(self, attr)
                    if val is not None:
                        if attr in self.ident_map and isinstance(attr_map(attr), tuple):
                            for i, (att2, val2) in enumerate(zip(attr_map(attr), val)):
                                assign = f"{att2}{spacing}={spacing}" if assignments else ""
                                app += f"{assign}{idx(val_map(val2))}{endstr}"
                        else:
                            assign = f"{attr_map(attr)}{spacing}={spacing}" if assignments else ""
                            app += f"{assign}{idx(val_map(val))}{endstr}"
                else:
                    if attr == "self":
                        app += idx(self)
                        app += endstr
                    else:
                        app += attr
                        app += endstr


        inp = []
        append(self.shape, inp)

        inp = "".join(inp)


        # Continue lines that are too long
        r = ""
        length = 0
        last_whitespace = 0
        for i, char in enumerate(inp):
            if char.isspace():
                last_whitespace = i
            if length < 195:
                if char == "\n":
                    length = 0
                r += char
                length += 1
            else:
                r = r[:last_whitespace] + " \\\n     " + r[last_whitespace:i] + char + r[i:]
                length = 0

        
        return r




    def __eq__(self, other):
        if type(self) != type(other):
            return False
        elif hasattr(self, "__dict__") and hasattr(other, "__dict__"):
            d1 = {k: v for k, v in self.__dict__.items() if k not in self.no_hash}
            d2 = {k: v for k, v in other.__dict__.items() if k not in self.no_hash}
            return d1 == d2

    def __hash__(self):
        return hash(tuple(v for k, v in sorted(self.__dict__.items()) \
                          if (self not in v.__dict__.values() if hasattr(v, "__dict__") else True) and k not in self.no_hash))


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

class Mesh():
    def __init__(self, axis, bins=None): # All bin generation is easily done in Python via list comprehensions
        assert axis in ["energy", "time", "x", "y", "z", "radius", "angle", "let"], f"Unrecognized axis {axis} in mesh definition."
        self.axis = axis
        self.bins = tuple(bins)
        print(self.bins)
        if axis != "angle":
            self.type = 2
        else:
            pass  # TODO: figure out angle mesh


    def __eq__(self, other):
        if type(self) != type(other):
            return False

        else:
            return {k: v for k, v in self.__dict__.items() if v is not other} \
                == {k: v for k, v in other.__dict__.items() if v is not self}

    def __hash__(self):
        return hash(tuple(v for k, v in sorted(self.__dict__.items()) \
                          if (self not in v.__dict__.values() if hasattr(v, "__dict__") else True)))
