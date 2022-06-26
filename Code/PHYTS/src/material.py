from base import *


class MatTimeChange(PhitsObject):
    def __init__(self, *args, **kwargs):                              # old can get set by passing this to Material
        super().__init__("mat_time_change", required=["time", "new"], positional=["time, new"], optional=["old"],
                         shape=(("old", "time", "new")), *args, **kwargs)



class DataMax(PhitsObject): # requires special handling in make_input
    def __init__(self, *args, **kwargs):
        super().__init__("data_max", required=["particles", "nucleus", "threshold"],
                         positional=["particles", "nucleus", "threshold"],
                         optional=["material"], shape=(("material", "nucleus", "threshold")), *args, **kwargs)


class MatNameColor(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("mat_name_color", required=["name", "size", "color"], positional=["name", "size", "color"],
                         optional=["material"], shape=(("material", "name", "size", "color")), *args, **kwargs)


# TODO: implement the molecular structures as in 5.4.7
# TODO: how the libraries work isn't well-documented. Is there a single library set for the whole material, or
# does one set a library after each element of the compositon? Can the thermal neutron library be set anywhere?
class Material(PhitsObject): # Composition is a list of pairs of (<element name string>, <ratio>) e.g. ("8Li", 0.5)
    def __init__(self, *args, **kwargs):
        super().__init__("material",
                         required=["composition"],
                         positional=["composition"],
                         optional=["time_change", "data_max", "mat_name_color", "condensed", "conductive", "electron_step",
                                   "neutron_lib", "proton_lib", "electron_lib", "photon_lib", "thermal_lib"],
                         shape=(lambda: f"MAT[{self.index}]",
                                (lambda: "".join(map(lambda tup: f"{tup[0]} {tup[1]} ", self.composition))), "condensed",
                                "conductive", "electron_step", "neutron_lib", "proton_lib","electron_lib", "photon_lib",
                                lambda: f"MT{self.index} {self.thermal_lib}" if self.thermal_lib is not None else ""),
                         subobjects=["time_change", "data_max", "mat_name_color"],
                         ident_map={"condensed": "GAS", "conductive": "COND", "electron_step": "ESTEP", "neutron_lib": "NLIB",
                                    "proton_lib": "HLIB", "electron_lib": "HLIB", "photon_lib": "PLIB"}, *args, **kwargs)
