from base import *

class MagneticField(PhitsObject): # Right now, the only way to set this is to do setattr(<cell>, "magnetic_field", <MagneticField>) since one cannot pass the cell to create the magnetic field while the cell is being initialized.
    name = "magnetic_field"
    required = ["typ", "gap", "strength"]
    positional = ["typ", "gap", "strength"]
    optional = ["transform", "time", "cell"]
    shape = (("cell", "typ", "gap", "strength", "transform", "time"))
    nones = {"transform": 0, "time": "non"}

class NeutronMagneticField(PhitsObject):
    name = "neutron_magnetic_field"
    required = ["typ", "gap", "strength"]
    positional = ["typ", "gap", "strength"]
    optional = ["transform", "polar", "time", "cell"]
    shape = (("cell", "typ", "gap", "strength", "transform", "polar", "time"))
    nones = {"transform": 0, "polar": "non", "time": "non"}

class MappedMagneticField(PhitsObject):
    name = "neutron_magnetic_field"
    required = ["typ", "gap", "strength", "m_file"]
    positional = ["typ", "gap", "strength", "m_file"]
    optional = ["transform", "cell"]
    shape = (("cell", "typ", "gap", "strength", "transform", "m_file"))
    nones = {"transform": 0}


class UniformElectromagneticField(PhitsObject):
    name = "uniform_electromagnetic_field"
    required = ["e_strength", "m_strength"]
    positional = ["e_strength", "m_strength"]
    optional = ["e_transform", "m_transform"]
    shape = (("cell", "e_strength", "m_strength", "e_transform", "m_transform"))
    nones = {"e_transform": 0, "m_transform": 0}


class MappedElectromagneticField(PhitsObject):
    name = "mapped_electromagnetic_field"
    required = ["typ_e", "typ_m", "gap", "e_strength", "m_strength", "e_file", "m_file"]
    positional = ["typ_e", "typ_m", "gap", "e_strength", "m_strength", "e_file", "m_file"]
    optional = ["e_transform", "m_transform"]
    shape = (("cell", "typ_e", "typ_m", "gap", "e_strength", "m_strength", "e_file", "m_file"))
    nones = {"e_transform": 0, "m_transform": 0}


class DeltaRay(PhitsObject):
    name = "delta_ray"
    required = ["threshold"]
    positional = ["threshold"]
    optional = ["cell"]
    shape = (("cell", "threshold"))


class TrackStructure(PhitsObject):
    name = "track_structure"
    required = ["mID"]
    positional = ["mID"]
    optional = ["cell"]
    shape = (("cell", "mID"))


class SuperMirror(PhitsObject):
    name = "super_mirror"
    required = ["r_in", "r_out", "mirror_material", "reflectivity", "cutoff", "falloff_rate", "cutoff_width"]
    positional = ["r_in", "r_out", "mirror_material", "reflectivity", "cutoff","falloff_rate", "cutoff_width"]
    optional = []
    shape = (("r_in", "r_out", "mirror_material", "reflectivity", "cutoff", "falloff_rate", "cutoff_width"))


class ElasticOption(PhitsObject):
    name = "elastic_option"
    required = ["c1", "c2", "c3", "c4"]
    positional = ["c1", "c2", "c3", "c4"]
    optional = ["cell"]
    shape = (("cell", "c1", "c2", "c3", "c4"))
    nones = {"c1": "non", "c2": "non", "c3": "non", "c4": "non"}


class FragData(PhitsObject):
    name = "frag_data"
    required = ["option", "projectile", "target", "file"]
    positional = ["option", "projectile", "target", "file"]
    optional = []
    shape = (("option", "projectile", "target", "file"))


class Importance(PhitsObject):
    name = "importance"
    required = ["particles", "importance"]
    positional = ["particles", "importance"]
    optional = ["cell"]
    shape = (("cell", "importance"))

# TODO: Weight Window
class WeightWindow(PhitsObject):
    name = "weight_window"
    required = ["mesh", "particles", "windows"]
    positional = ["mesh", "particles", "windows"]
    optional = ["grid"]
    shape = (("windows"))

# TODO: WW Bias
# class WWBias(PhitsObject):
#     def __init__(self, *args, **kwargs):
#         super().__init__("ww_bias", required=)


class ForcedCollisions(PhitsObject):
    name = "forced_collisions"
    required = ["particles", "probability"]
    positional = ["particles", "probability"]
    optional = ["cell"]
    shape = (("cell", "probability"))


# TODO: Repeated Collisions
class RepeatedCollisions(PhitsObject):
    name = "repeated_collisions"
    required = ["particles", "mother", "ebounds", "collision_reps", "evaporation_reps"]
    positional = ["particles", "mother", "ebounds", "collision_reps", "evaporation_reps"]
    optional = ["cell"]
    shape = (("cell", "collision_reps", "evaporation_reps"))


class RegionName(PhitsObject):
    name = "region_name"
    required = ["name", "size"]
    positional = ["name", "size"]
    optional = ["cell"]
    shape = (("cell", "name", "size"))


class Timer(PhitsObject):
    name = "timer"
    required = ["enter", "out", "coll", "ref"]
    positional = ["enter", "out", "coll", "ref"]
    optional = ["cell"]
    shape = (("cell", "enter", "out", "coll", "ref"))
