# Sources will be specified by an iterable whose elements are a tuple  (Source(), weight) where the last two are optional.
# totfact and iscorr may be specified in the general parameters section.

# Currently, no a-type mesh support for the elevation angles.
from base import *

class Source(PhitsObject): # currently no support for cnt(i) or ibatch common parameters
    def __init__(self, s_type, projectile, *, spin=(0, 0, 0), mask=(None, 1000),
                 transform=None, weight=1.0, factor=1.0, charge_override=None, fissile=False, **kwargs):
        super().__init__("source", **kwargs)
        self.s_type = s_type
        self.proj = projectile
        self.sx = spin[0]
        self.sy = spin[1]
        self.sz = spin[2]
        self.reg = mask[0]
        self.ntmax = mask[1]
        self.trcl = transform
        self.wgt = weight
        self.factor = factor
        self.izst = charge_override
        self.ispfs = 0 if not fissile else (2 if fissile == "neutrons" else 1)

    def definition(self):
        inp = ""
        for var, val in [(k, v) for k, v in self.__dict__.items() if k not in {"index", "name", "parameters"}]:
            if val is not None:
                if isinstance(val, PhitsObject):
                    inp += f"{var} = {val.index}\n"
                else:
                    var2 = var.replace("_", "-")
                    inp += f"{var2} = {val}\n"

        return inp

        

class Cylindrical(PhitsObject):
    name = "source"
    required=["projectile", "energy"]
    positional=["projectile", "energy"]
    optional=["spin", "mask", "transform", "weight", "factor", "charge_override", "fissile",
              "center", "bounds", "r_out", "r_in", "elevation", "azimuth", "dispersion"]
    ident_map={"spin": ("sx", "sy", "sz"), "mask": ("reg", "ntmax"), "transform": "trcl",
               "weight": "wgt", "charge_override": "izst", "fissile": "ispfs", "center": ("x0", "y0"),
               "bounds": ("z0", "z1"), "r_out": "r0", "r_in": "r1", "elevation": "dir", "azimuth": "phi",
               "dispersion": "dom", "energy": "e0", "projectile": "proj"}
    value_map={"neutrons": 2, True: 1}
    shape=("s-type = 1", "projectile", "spin", "mask", "transform", "weight", "factor", "charge_override",
           "fissile", "center", "bounds", "r_out", "r_in", "elevation", "azimuth", "dispersion", "energy")



class Rectangular(PhitsObject):
    name = "source"
    required=["projectile", "energy"]
    positional=["projectile", "energy"]
    optional=["spin", "mask", "transform", "weight", "factor", "charge_override", "fissile",
              "xbounds", "ybounds", "zbounds", "elevation", "azimuth", "dispersion"]
    ident_map={"spin": ("sx", "sy", "sz"), "mask": ("reg", "ntmax"), "transform": "trcl",
               "weight": "wgt", "charge_override": "izst", "fissile": "ispfs", "xbounds": ("x0", "x1"),
               "ybounds": ("y0", "y1"), "zbounds": ("z0", "z1"), "elevation": "dir", "azimuth": "phi",
               "dispersion": "dom", "energy": "e0"}
    value_map={"neutrons": 2, True: 1}
    shape=("s_type = 2", "projectile", "spin", "mask", "transform", "weight", "factor", "charge_override",
           "fissile", "xbounds", "ybounds", "zbounds", "elevation", "azimuth", "dispersion", "energy")



class Gaussian(PhitsObject):
    def __init__(self, *args, **kwargs):
        if "zbounds" in kwargs:
            self.name = "source"
            self.required = ["projectile", "energy"]
            self.positional = ["projectile", "energy"]
            self.optional = ["spin", "mask", "transform", "weight", "factor", "charge_override", "fissile",
                      "center", "fwhms", "zbounds", "elevation", "azimuth", "dispersion"]
            self.ident_map = {"spin": ("sx", "sy", "sz"), "mask": ("reg", "ntmax"), "transform": "trcl",
                       "weight": "wgt", "charge_override": "izst", "fissile": "ispfs",
                       "center": ("x0", "y0"), "fwhms": "r1", "zbounds": ("z0", "z1"),
                       "elevation": "dir", "azimuth": "phi", "dispersion": "dom", "energy": "e0"}
            self.value_map = {"neutrons": 2, True: 1}
            self.shape = ("s_type = 13", "projectile", "spin", "mask", "transform", "weight", "factor",
                   "charge_override", "fissile", "center", "fwhms", "zbounds", "elevation", "azimuth",
                   "dispersion", "energy")
            super().__init__(*args, **kwargs)
        else:
            self.name = "source"
            self.required = ["projectile", "energy"]
            self.positional = ["projectile", "energy"]
            self.optional = ["spin", "mask", "transform", "weight", "factor", "charge_override", "fissile",
                      "center", "fwhms", "zbounds", "elevation", "azimuth", "dispersion"]
            self.ident_map = {"spin": ("sx", "sy", "sz"), "mask": ("reg", "ntmax"), "transform": "trcl",
                              "weight": "wgt", "charge_override": "izst", "fissile": "ispfs",
                              "center": ("x0", "y0", "z0"), "fwhms": ("x1", "y1", "z1"),
                              "elevation": "dir", "azimuth": "phi", "dispersion": "dom", "energy": "e0"}
            self.value_map = {"neutrons": 2, True: 1}
            self.shape = ("s_type = 3", "projectile", "spin", "mask", "transform", "weight", "factor",
                          "charge_override", "fissile", "center", "fwhms", "elevation", "azimuth",
                          "dispersion", "energy")
            super().__init__(*args, **kwargs)




class Parabolic(PhitsObject):
    # Thanks to lazy typing, xyz or x-y is determined by dimension of center
    def __init__(self, *args, **kwargs):
        if width in kwargs and len(kwargs["width"]) == 2:
            self.name = "source"
            self.required = ["projectile", "energy"]
            self.positional = ["projectile", "energy"]
            self.optional = ["spin", "mask", "transform", "weight", "factor", "charge_override", "fissile",
                             "center", "width", "zbounds", "order", "elevation", "azimuth", "dispersion"]
            self.ident_map = {"spin": ("sx", "sy", "sz"), "mask": ("reg", "ntmax"), "transform": "trcl",
                              "weight": "wgt", "charge_override": "izst", "fissile": "ispfs",
                              "center": ("x0", "y0"), "width": "r1", "zbounds": ("z0", "z1"), "order": "rn",
                              "elevation": "dir", "azimuth": "phi", "dispersion": "dom", "energy": "e0"}
            self.value_map = {"neutrons": 2, True: 1}
            self.shape = ("s_type = 15", "projectile", "spin", "mask", "transform", "weight", "factor",
                          "charge_override", "fissile", "center", "width", "zbounds", "order", "elevation", "azimuth",
                          "dispersion", "energy")
            super().__init__(*args, **kwargs)
        else:
            self.name = "source"
            self.required = ["projectile", "energy"]
            self.positional = ["projectile", "energy"]
            self.optional = ["spin", "mask", "transform", "weight", "factor", "charge_override", "fissile",
                      "center", "width", "zbounds", "order", "elevation", "azimuth", "dispersion"]
            self.ident_map = {"spin": ("sx", "sy", "sz"), "mask": ("reg", "ntmax"), "transform": "trcl",
                       "weight": "wgt", "charge_override": "izst", "fissile": "ispfs",
                       "center": ("x0", "y0", "z0"), "width": ("x1", "y1", "z1"), "order": "rn",
                       "elevation": "dir", "azimuth": "phi", "dispersion": "dom", "energy": "e0"}
            self.value_map = {"neutrons": 2, True: 1}
            self.shape = ("s_type = 7", "projectile", "spin", "mask", "transform", "weight", "factor",
                   "charge_override", "fissile", "center", "width", "zbounds", "order", "elevation", "azimuth",
                   "dispersion", "energy")
            super().__init__(*args, **kwargs)


        
class Spherical(PhitsObject):
    name = "source"
    required=["projectile", "energy"]
    positional=["projectile", "energy"]
    optional=["spin", "mask", "transform", "weight", "factor", "charge_override", "fissile",
              "center", "r_in", "r_out", "direction", "elevation_bounds", "azimuth_bounds",
              "cutoff_behavior"]
    ident_map={"spin": ("sx", "sy", "sz"), "mask": ("reg", "ntmax"), "transform": "trcl",
               "weight": "wgt", "charge_override": "izst", "fissile": "ispfs",
               "center": ("x0", "y0", "z0"), "r_in": "r1", "r_out": "r2", "direction": "dir",
               "elevation_bounds": ("ag1", "ag2"), "azimuth_bounds": ("pg1", "pg2"),
               "cutoff_behavior": "isbias", "energy": "e0"}
    value_map={"neutrons": 2, True: 1, "ignore": 0, "resample": 1, "outward": 1.0, "inward": -1.0,
               "isotropic": "all", "iso": "all", "cosine": "-all"}
    shape=("s_type = 9", "projectile", "spin", "mask", "transform", "weight", "factor",
           "charge_override", "fissile", "center", "r_in", "r_out", "direction", "elevation_bounds",
           "azimuth_bound", "cutoff_behavior", "energy")


class Beam(PhitsObject): # I don't understand what this is trying to do
    def __init__(self, center=(0.0, 0.0), xbound=(0.0, 0.0), ybound=(0.0, 0.0), zbound=(0.0,0.0),
                 gradx=0.0, grady=0.0):
        super().__init__("source", required=["projectile", "energy"], positional=["projectile", "energy"],
                         optional=["spin", "mask", "transform", "weight", "factor", "charge_override", "fissile",
                                   "center", "zbounds", "gradients", "sampling" "width", "azimuth", "dispersion"],
                         ident_map={"spin": ("sx", "sy", "sz"), "mask": ("reg", "ntmax"), "transform": "trcl",
                                    "weight": "wgt", "charge_override": "izst", "fissile": "ispfs",
                                    "center": ("x0", "y0", "z0"), "width": ("x1", "y1", "z1"), "order": "rn",
                                    "elevation": "dir", "azimuth": "phi", "dispersion": "dom", "energy": "e0"},
                         value_map={"neutrons": 2, True: 1},
                         shape=("s_type = 7", "projectile", "spin", "mask", "transform", "weight", "factor",
                                "charge_override", "fissile", "center", "width", "zbounds", "order", "elevation", "azimuth",
                                "dispersion", "energy"), *args, **kwargs)
        pass
    
class Conical(PhitsObject):
    name = "source"
    required = ["projectile", "energy"]
    positional = ["projectile", "energy"]
    optional = ["spin", "mask", "transform", "weight", "factor", "charge_override", "fissile",
                "top", "altitude", "trim", "elevation", "azimuth", "dispersion"]
    ident_map = {"spin": ("sx", "sy", "sz"), "mask": ("reg", "ntmax"), "transform": "trcl",
                 "weight": "wgt", "charge_override": "izst", "fissile": "ispfs",
                 "top": ("x0", "y0", "z0"), "altitude": ("x1", "y1", "z1"), "trim": ("r0", "r1"),
                 "elevation": "dir", "azimuth": "phi", "dispersion": "dom", "energy": "e0"}
    value_map = {"neutrons": 2, True: 1}
    shape=("s_type = 18", "projectile", "spin", "mask", "transform", "weight", "factor",
           "charge_override", "fissile", "top", "altitude", "trim", "elevation", "azimuth",
           "dispersion", "energy")

            
                
class Prism(PhitsObject):
    name = "source"
    required = ["projectile", "energy"]
    positional = ["projectile", "energy"]
    optional = ["spin", "mask", "transform", "weight", "factor", "charge_override", "fissile",
                "origin", "side1", "side2", "extrusion", "attenuation", "elevation", "azimuth", "dispersion"]
    ident_map = {"spin": ("sx", "sy", "sz"), "mask": ("reg", "ntmax"), "transform": "trcl",
                 "weight": "wgt", "charge_override": "izst", "fissile": "ispfs",
                 "origin": ("x0", "y0", "z0"), "side1": ("x1", "y1", "z1"), "side2": ("x2", "y2", "z2"),
                 "extrusion": ("x3", "y3", "z3"), "attenuation": "exa",
                 "elevation": "dir", "azimuth": "phi", "dispersion": "dom", "energy": "e0"}
    value_map = {"neutrons": 2, True: 1}
    shape = ("s_type = 20", "projectile", "spin", "mask", "transform", "weight", "factor",
             "charge_override", "fissile", "origin", "side1", "side2", "extrusion", "attenuation", "azimuth",
             "dispersion", "energy")



class Grid(PhitsObject): # I'm not sure how the intensity is supposed to work
    def __init__(self, mesh, projectile, energy, spin=(0, 0, 0), mask=(None, 1000),
                 transform=None, weight=1.0, factor=1.0, charge_override=None, fissile=False):
        pass

class Tetrahedral(PhitsObject): # TODO: tetrahedral geometry
    def __init__(self, projectile, energy, region,
                 elevation=1.0, azimuth=None, dispersion=0.0, **kwargs):
        super().__init__(24, projectile, **kwargs)
        self.tetreg = region
        self.dir = elevation
        self.phi = azimuth
        self.dom = dispersion
        self.e0 = energy


# class SurfaceSource(PhitsObject): # TODO: think about how this can interact with surface numbers, and how
#                      # cuts will work
#     def __init__(self, projectile, spin=(0, 0, 0), mask=(None, 1000), transform=idTransform,
#                  different_charge=None, surface, cuts, elevation, azimuth, dispersion,
#                  projectile_energy, **kwargs):
#         super().__init__(20, projectile, spin=spin, mask=mask, transform=transform, charge_override=charge_override, fissile=fissile, **kwargs)
         
    

# class Energy: This is actually about the e-type subsection. Need a class perhaps?

# class Angular: part of a-type subsection defined for each source. Should overload
# elevation parameter to support a list of a-type options. 

# class TimeDependent: part of t-type subsections defined for each 

# class Fissile: part of e-type
