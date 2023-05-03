from copy import deepcopy
from base import *
from transform import *
from misc import *
from material import *
from surface import TetrahedronBox, surface_spec

def tup_to_def(tup):
    r = ""
    for el in tup:
        if isinstance(el, PhitsObject):
            sense = "-" if el.inside is not None else ""
            r += f"{sense}{el.index} "
        elif isinstance(el, tuple):
            r += "(" + tup_to_def(el) + ") "
        elif el == "~":
            r += "#"
        elif el == "|":
            r += ": "
        else:
            breakpoint()
            raise ValueError(f"Unrecognized token {el} in cell region definition.")

    return r

def outer_void(tup):
    return "~ (" + " : ".join(map(lambda x: tup_to_def((x,)), tup)) + ")"


subobject_syntax = {"transform": ("TRCL", IsA(Transform, index=True), None),
                    "magnetic_field": (None, IsA(MagneticField), None),
                    "neutron_magnetic_field": (None, IsA(NeutronMagneticField), None),
                    "mapped_magnetic_field": (None, IsA(MappedMagneticField), None),
                    "uniform_electromagnetic_field": (None, IsA(UniformElectromagneticField), None),
                    "mapped_electromagnetic_field": (None, IsA(MappedElectromagneticField), None),
                    "delta_ray": (None, IsA(DeltaRay), None),
                    "track_structure": (None, IsA(TrackStructure), None),
                    "super_mirror": (None, IsA(SuperMirror), None),
                    # "elastic_option": (None, IsA(ElasticOption), None),
                    "importance": (None, IsA(Importance), None),
                    "weight_window": (None, IsA(WeightWindow), None),
                    "ww_bias": (None, IsA(WWBias), None),
                    "forced_collisions": (None, IsA(ForcedCollisions), None),
                    "repeated_collisions": (None, IsA(RepeatedCollisions), None),
                    "reg_name": (None, IsA(RegionName), None),
                    "counter": (None, IsA(Counter), None),
                    "timer": (None, IsA(Timer), None)}

common_syntax = subobject_syntax | {"volume": ("VOL", PosReal(), None),
                                    "temperature": ("TMP", PosReal(), None)}

class Tetrahedral(PhitsObject):
    name = "cell"
    syntax = common_syntax | {"within": (None, IsA(TetrahedronBox, index=True), 0),
                              "tet_format": (None, FinBij({"tetgen": "tetgen", "NASTRAN": "NASTRAN"}), 1),
                              "tet_file": (None, Path(), 2),
                              "scale_factor": ("TSFAC", PosReal(), None)}

    shape = lambda self: (("self", "material", "density\\"), tup_to_def((self.within,)),
                          "volume\\", "temperature\\", "transform\\", "LAT=3",
                          f"tfile={self.tet_file}" if self.tet_format == "tetgen" else f"nfile={self.tet_file}", "scale_factor\\")

    subobjects = set(subobject_syntax.keys())


class Void(PhitsObject):
    name = "cell"
    syntax = common_syntax | {"regions": (None, List(surface_spec), 0)}
    shape = lambda self: (("self", "0", tup_to_def(self.regions), "volume\\", "temperature\\", "transform\\"),)
    subobjects = set(subobject_syntax.keys())

    def __or__(self, other): # Union of cells; adopts leftmost's properties
        r = deepcopy(self)
        setattr(r, "regions", self.regions + ("|",) + other.regions)
        return r

    def __invert__(self): # Set complement of cell; new cell has old properties
        r = deepcopy(self)
        r.regions = ("~", self.regions)
        return r

    def __and__(self, other): # Intersection of cells; drops properties
        r = deepcopy(self)
        r.regions = self.regions + other.regions
        return r

    def __rshift__(self, other): # returns other's regions with self's properties
        r = deepcopy(self)
        r.regions = other.regions
        return r

    def __lshift__(self, other): # returns self's region with other's properties
        r = deepcopy(other)
        r.regions = self.regions
        return r

class OuterVoid(PhitsObject):
    name = "cell"
    syntax = common_syntax | {"regions": (None, List(surface_spec), 0)}
    shape = lambda self: (("self", "-1", tup_to_def(self.regions), "volume\\", "temperature\\", "transform\\"),)
    subobjects = set(subobject_syntax.keys())

    def __or__(self, other): # Union of cells; adopts leftmost's properties
        r = deepcopy(self)
        setattr(r, "regions", self.regions + ("|",) + other.regions)
        return r

    def __invert__(self): # Set complement of cell; new cell has old properties
        r = deepcopy(self)
        r.regions = ("~", self.regions)
        return r

    def __and__(self, other): # Intersection of cells; drops properties
        r = deepcopy(self)
        r.regions = self.regions + other.regions
        return r

    def __rshift__(self, other): # returns other's regions with self's properties
        r = deepcopy(self)
        r.regions = other.regions
        return r

    def __lshift__(self, other): # returns self's region with other's properties
        r = deepcopy(other)
        r.regions = self.regions
        return r

# TODO: operations
class Cell(PhitsObject):
    name = "cell"
    syntax = common_syntax | {"regions": (None, List(surface_spec), 0),
                              "material": (None, IsA(Material, index=True), 1),
                              "density": (None, PosReal(), 2)}
    shape = lambda self: (("self", "material", "density", tup_to_def(self.regions), "volume\\", "temperature\\", "transform\\"),)


    def __or__(self, other): # Union of cells; adopts leftmost's properties
        r = deepcopy(self)
        setattr(r, "regions", self.regions + ("|",) + other.regions)
        return r

    def __invert__(self): # Set complement of cell; new cell has old properties
        r = deepcopy(self)
        r.regions = ("~", self.regions)
        return r

    def __and__(self, other): # Intersection of cells; adopts leftmost's properties
        r = deepcopy(self)
        r.regions = self.regions + other.regions
        return r

    def __rshift__(self, other): # returns other's regions with self's properties
        r = deepcopy(self)
        r.regions = other.regions
        return r

    def __lshift__(self, other): # returns self's region with other's properties
        r = deepcopy(other)
        r.regions = self.regions
        return r

# idea: generate a UUID for the universe/fill, and then map UUIDs -> index at runtime
# other idea: make a Universe class, define an __init__, and make a call to super() for the normal __init__,
# but use the rest of __init__ to set the right attributes on the underlying cells, and marshall definitions
def fill_universe(mask: Cell, contents: list[Cell]):
    pass
