from base import *


class MatTimeChange(PhitsObject):
    def __init__(self, time, new, **kwargs):
        super().__init__("mat_time_change", **kwargs)
        self.old = None
        self.time = time
        self.new = new

    def definition(self):
        return f"{self.old.index} {self.time} {self.new.index}\n"



class DataMax(PhitsObject):
    def __init__(self, particles, threshold, **kwargs):
        super().__init__("data_max", **kwargs)
        self.particles = particles
        self.material = None
        self.threshold = threshold

class MatNameColor(PhitsObject):
    def __init__(self, name, size, color, **kwargs):
        super().__init__("mat_name_color", **kwargs)
        self.material = None
        self.name = name
        self.size = size
        self.color = color

    def definition(self):
        return f"{self.material.index} {self.name} {self.size} {self.color}\n"

# TODO: implement the molecular structures as in 5.4.7
class Material(PhitsObject): # Composition is a list of pairs of (<element name string>, <ratio>) e.g. ("8Li", 0.5)
    def __init__(self, composition, time_change=None, data_max=None, mat_name_color=None, condensed=True, conductive=False,
                 electron_step=None, neutron_lib=None, proton_lib=None, electron_lib=None, photon_lib=None, thermal_lib=None, **kwargs):
        super().__init__("material", **kwargs)
        self.composition = composition
        self.time_change = time_change
        self.data_max = data_max
        self.mat_name_color = mat_name_color
        self.condensed = condensed
        self.conductive = conductive
        self.electron_step = electron_step
        self.neutron_lib = neutron_lib
        self.proton_lib = proton_lib
        self.electron_lib = electron_lib
        self.photon_lib = photon_lib

        if self.time_change is not None:
            self.time_change.old = self

        if self.data_max is not None:
            self.data_max.material = self

        if self.mat_name_color is not None:
            self.mat_name_color.material = self


    def definition(self):
        inp = f"MAT[{self.index}]\n"
        for element, ratio in self.composition:
            inp += f"{element} {ratio}\n"

        if not self.condensed == "gas":
            inp += "GAS = 1\n"

        if self.conductive:
            inp += "COND = 1\n"
        else:
            inp += "COND = -1\n"

        if self.electron_step is not None:
            inp += f"ESTEP = {self.electron_step}\n"

        if self.neutron_lib is not None:
            inp += f"NLIB = {self.neutron_lib}\n"
        if self.proton_lib is not None:
            inp += f"HLIB = {self.proton_lib}\n"
        if self.electron_lib is not None:
            inp += f"ELIB = {self.electron_lib}\n"
        if self.photon_lib is not None:
            inp += f"PLIB = {self.photon_lib}\n"

        if self.thermal_lib is not None:
            inp += f"MT{self.index} {self.thermal_lib}\n"

        return inp
