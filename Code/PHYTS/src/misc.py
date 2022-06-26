from base import *

class MagneticField(PhitsObject): # Right now, the only way to set this is to do setattr(<cell>, "magnetic_field", <MagneticField>) since one cannot pass the cell to create the magnetic field while the cell is being initialized.
    def __init__(self, *args, **kwargs):
        super().__init__("magnetic_field", required=["typ", "gap", "strength"], positional=["typ", "gap", "strength"],
                         optional=["transform", "time", "cell"], shape=(("cell", "typ", "gap", "strength", "transform", "time")),
                         nones={"transform": 0, "time": "non"}, *args, **kwargs)

class NeutronMagneticField(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("neutron_magnetic_field", required=["typ", "gap", "strength"],
                         positional=["typ", "gap", "strength"], optional=["transform", "polar", "time", "cell"],
                         shape=(("cell", "typ", "gap", "strength", "transform", "polar", "time")),
                         nones={"transform": 0, "polar": "non", "time": "non"}, *args, **kwargs)

class MappedMagneticField(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("neutron_magnetic_field", required=["typ", "gap", "strength", "m_file"],
                         positional=["typ", "gap", "strength", "m_file"], optional=["transform", "cell"],
                         shape=(("cell", "typ", "gap", "strength", "transform", "m_file")),
                         nones={"transform": 0}, *args, **kwargs)


class UniformElectromagneticField(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("uniform_electromagnetic_field", required=["e_strength", "m_strength"],
                         positional=["e_strength", "m_strength"], optional=["e_transform", "m_transform"],
                         shape=(("cell", "e_strength", "m_strength", "e_transform", "m_transform")),
                         nones={"e_transform": 0, "m_transform": 0}, *args, **kwargs)


class MappedElectromagneticField(PhitsObject):
    def __init__(self, typ_e, typ_m, gap, e_strength, m_strenth, e_file, m_file, e_transform=None, m_transform=None, **kwargs):
        super().__init__("mapped_electromagnetic_field", required=["typ_e", "typ_m", "gap", "e_strength", "m_strength", "e_file",
                                                                   "m_file"],
                         positional=["typ_e", "typ_m", "gap", "e_strength", "m_strength", "e_file", "m_file"],
                         optional=["e_transform", "m_transform"],
                         shape=(("cell", "typ_e", "typ_m", "gap", "e_strength", "m_strength", "e_file", "m_file")),
                         nones={"e_transform": 0, "m_transform": 0}, *args, **kwargs)

class DeltaRay(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("delta_ray", required=["threshold"], positional=["threshold"], optional=["cell"],
                         shape=(("cell", "threshold")), *args, **kwargs)

class TrackStructure(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("track_structure", required=["mID"], positional=["mID"], optional=["cell"],
                         shape=(("cell", "mID")), *args, **kwargs)


class SuperMirror(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("super_mirror", required=["r_in", "r_out", "mirror_material", "reflectivity", "cutoff",
                                                   "falloff_rate", "cutoff_width"],
                         positional=["r_in", "r_out", "mirror_material", "reflectivity", "cutoff","falloff_rate", "cutoff_width"],
                         optional=[],
                         shape=(("r_in", "r_out", "mirror_material", "reflectivity", "cutoff", "falloff_rate", "cutoff_width")),
                         *args, **kwargs)


class ElasticOption(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("elastic_option", required=["c1", "c2", "c3", "c4"], positional=["c1", "c2", "c3", "c4"],
                         optional=["cell"], shape=(("cell", "c1", "c2", "c3", "c4")),
                         nones={"c1": "non", "c2": "non", "c3": "non", "c4": "non"}, *args, **kwargs)


class FragData(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("frag_data", required=["option", "projectile", "target", "file"],
                         positional=["option", "projectile", "target", "file"],
                         optional=[], shape=(("option", "projectile", "target", "file")), *args, **kwargs)


class Importance(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("importance", required=["particles", "importance"], positional=["particles", "importance"],
                         optional=["cell"], shape=(("cell", "importance")), *args, **kwargs)

# TODO: Weight Window
# class WeightWindow(PhitsObject):
#     def __init__(self, *args, **kwargs):
#         super().__init__("weight_window", required=["mesh", "particles", ""])
# TODO: WW Bias
# class WWBias(PhitsObject):
#     def __init__(self, *args, **kwargs):
#         super().__init__("ww_bias", required=)


class ForcedCollisions(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("forced_collisions", required=["particles", "probability"], positional=["particles", "probability"],
                         optional=["cell"], shape=(("cell", "probability")), *args, **kwargs)


# TODO: Repeated Collisions
class RepeatedCollisions(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("repeated_collisions",
                         required=["particles", "mother", "ebounds", "collision_reps", "evaporation_reps"],
                         positional=["particles", "mother", "ebounds", "collision_reps", "evaporation_reps"],
                         optional=["cell"], shape=(("cell", "collision_reps", "evaporation_reps")))


class RegionName(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("region_name", required=["name", "size"], positional=["name", "size"], optional=["cell"],
                         shape=(("cell", "name", "size")), *args, **kwargs)


class Timer(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("timer", required=["enter", "out", "coll", "ref"], positional=["enter", "out", "coll", "ref"],
                         optional=["cell"], shape=(("cell", "enter", "out", "coll", "ref")), *args, **kwargs)
