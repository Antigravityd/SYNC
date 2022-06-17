import copy
from base import *

class Cell(PhitsObject): # dictionary of properties and a frozenset of Surface() objects

            # Operations &, |, >>, <<, and ~ are the set operations on the regions. & and | clear properties,
            # inversion preserves them, and << and >> copy properties of one cell onto another.
            # The property dictionary has equivalent natural language and array syntaxes (hopefully!),
            # e.g. {material: "60% water, 40% lead 208"} ↔ {material: [(0.6, "H20"), (0.4, "Pb-208")]}
            # Every property that applies to a cell ought to be supported, like transform, tallies, etc.
    def __init__(self, regions, material, density, transform=None, **kwargs):
        self.regions = tuple(regions)
        self.material = material
        self.density = density
        self.transform = transform

        allowed_keys = {"transform", # Values one wants to associate with a cell, and that require setting the value's .cell attribute because it can't be set at initialization
                        "temperature",
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
                        "reg_name",
                        "counter",
                        "timer",
                        "tally"}


        for k in allowed_keys:
            if k in kwargs:
                setattr(self, k, tuple(kwargs[k]) if isinstance(kwargs[k], list) else kwargs[k])
                kwargs[k].cell = self
            else:
                setattr(self, k, None)

        super().__init__("cell", **{k: v for k, v in kwargs.items() if k not in allowed_keys})

    # TODO: fix these dunder methods given new required options. They probably ought to set the material to void and density to zero, but I'm not sure those are handled right as materials.
    def __or__(self, other): # Union of cells; drops properties
        return Cell(self.regions | other.regions)

    def __inv__(self): # Set complement of cell; new cell has old properties
        new = []
        for i in self.regions:
            assert len(i) == 2, f"Encountered incorrectly-sized entry {i} among regions."
            if i[1] == "<":
                other = ">"
            elif i[1] == ">":
                other = "<"
            else:
                raise ValueError(f"Encountered incorrect orientation {i[1]} among regions.")

            new.append((i[0], other))

        r = copy.deepcopy(self)
        setattr(r, "regions", new)  
        return r

    def __and__(self, other): # Intersection of cells; drops properties
        return Cell(self.regions & other.regions)

    def __rshift__(self, other): # returns other's regions with self's properties
        r = copy.deepcopy(self)
        setattr(r, "regions", other.regions)
        return r

    def __lshift__(self, other): # returns self's region with other's properties
        r = copy.deepcopy(other)
        setattr(r, "regions", self.regions)
        return r

    def definition(self):
        inp = f"{self.index} {self.material.index} {self.density} "
#        breakpoint()
        for sur, orient in self.regions:
            if orient == "<": # This may not be correct; the "sense" of surfaces is poorly documented.
                inp += f"{sur.index} "
            elif orient == ">":
                inp += f"-{sur.index} "
            else:
                raise ValueError(f"Invalid orientation {i[1]} among regions.")
        if self.volume is not None:
            inp += f"VOL={self.volume} "
        if self.temperature is not None:
            inp += f"TMP={self.temperature} "
        inp += "\n"
        return inp
