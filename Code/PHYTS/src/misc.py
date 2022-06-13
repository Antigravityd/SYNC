from base import *

class MagneticField(PhitsObject): # Right now, the only way to set this is to do setattr(<cell>, "magnetic_field", <MagneticField>) since one cannot pass the cell to create the magnetic field while the cell is being initialized.
    def __init__(self, typ, gap, strength, transform=None, time=None, **kwargs):
        super().__init__("magnetic_field", **kwargs)
        self.cell = None
        self.typ = typ
        self.gap = gap
        self.strength = strength
        self.transform = transform
        self.time = time

    def definition(self):
        non = "non"
        return f"{self.cell.index} {self.typ} {self.gap} {self.strength} {self.transform if self.transform is not None else 0} {self.time if self.time is not None else non}\n"

class NeutronMagneticField(PhitsObject):
    def __init__(self, typ, gap, strength, transform=None, polar=None, time=None, **kwargs):
        super().__init__("neutron_magnetic_field", **kwargs)
        self.cell = None
        self.typ = typ
        self.gap = gap
        self.strength = strength
        self.transform = transform
        self.polar = polar
        self.time = time

    def definition(self):
        non = "non"
        return f"{self.cell.index} {self.typ} {self.gap} {self.strength} {self.transform if self.transform is not None else 0} {self.polar if self.polar is not None else non} {self.time if self.polar is not None else non}\n"

class MappedMagneticField(PhitsObject):
    def __init__(self, typ, gap, strength, m_file, transform=None, **kwargs):
        super().__init__("neutron_magnetic_field", **kwargs)
        self.cell = None
        self.typ = typ
        self.gap = gap
        self.transform = transform
        self.m_file = m_file

    def definition(self):
        return f"{self.cell.index} {self.typ} {self.gap} {self.strength} {self.transform if self.transform is not None else 0} {self.m_file}\n"


class UniformElectromagneticField(PhitsObject):
    def __init__(self, e_strength, m_strenth, e_transform=None, m_transform=None, **kwargs):
        super().__init__("uniform_electromagnetic_field", **kwargs)
        self.cell = None
        self.e_strength = e_strength
        self.m_strength = m_strength
        self.e_transform = e_transform
        self.m_transform = m_transform

    def definition(self):
        return f"{self.cell.index} {self.e_strength} {self.m_strength} {self.e_transform if self.e_transform is not None else 0} {self.m_transform if self.m_transform is not None else 0}\n"

class MappedElectromagneticField(PhitsObject):
    def __init__(self, typ_e, typ_m, gap, e_strength, m_strenth, e_file, m_file, e_transform=None, m_transform=None, **kwargs):
        super().__init__("mapped_electromagnetic_field", **kwargs)
        self.cell = None
        self.typ_e = typ_e
        self.typ_m = typ_m
        self.e_strength = e_strength
        self.m_strength = m_strength
        self.e_transform = e_transform
        self.m_transform = m_transform
        self.e_file = e_file
        self.m_file = m_file

    def definition(self):
        return f"{self.cell.index} {self.typ_e} {self.typ_m} {self.gap} {self.e_strength} {self.m_strength} {self.e_transform if self.e_transform is not None else 0} {self.m_transform if self.m_transform is not None else 0} {self.e_file} {self.m_file}\n"
    
class DeltaRay(PhitsObject):
    def __init__(self, threshold, **kwargs):
        super().__init__("delta_ray", **kwargs)
        self.cell = None
        self.threshold = threshold

    def definition(self):
        return f"{self.cell.index} {self.threshold}\n"

class TrackStructure(PhitsObject):
    def __init__(self, mID, **kwargs):
        super().__init__("track_structure", **kwargs)
        self.cell = None
        self.mID = mID

    def definition(self):
        return f"{self.cell.index} {self.mID}\n"

class Importance(PhitsObject):
    def __init__(self, particles, importance, **kwargs):
        super().__init__("importance", **kwargs)
        self.particles = particles
        self.importance = importance
        self.cell = None

class ForcedCollisions(PhitsObject):
    def __init__(self, particles, fcl, **kwargs):
        super().__init__("forced_collisions", **kwargs)
        self.particles = particles
        self.fcl = fcl
        self.cell = None

class RegionName(PhitsObject):
    def __init__(self, name, color, **kwargs):
        super().__init__("region_name", **kwargs)
        self.name = name
        self.color = color
        self.cell = None

    def definition(self):
        return f"{self.cell.index} {self.name} {self.size}\n"

class Timer(PhitsObject):
    def __init__(self, enter, out, coll, ref, **kwargs):
        super().__init__("timer", **kwargs)
        self.enter = enter
        self.out = out
        self.coll = coll
        self.ref = ref
        self.cell = None

    def definition(self):
        return f"{self.cell.index} {self.enter} {self.out} {self.coll} {self.ref}\n"
