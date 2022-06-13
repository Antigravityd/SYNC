# Sources will be specified by an iterable whose elements are a tuple  (Source(), weight) where the last two are optional.
# totfact and iscorr may be specified in the general parameters section.

# Currently, no a-type mesh support for the elevation angles.
from base import *

class Source(PhitsObject): # currently no support for cnt(i) or ibatch common parameters
    def __init__(self, s_type, projectile, spin=(0, 0, 0), mask=(None, 1000),
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
        self.izst = different_charge
        self.ispfs = 0 if not fissile else (2 if fissile == "neutrons" else 1)

    def definition(self):
        inp = ""
        params = {k: v for k, v in self.__dict__.items() if k not in super().__dict__}
        for var, val in params:
            if val is not None:
                if isinstance(val, PhitsObject):
                    inp += f"{var} = {val.index}\n"
                else:
                    var2 = var.replace("_", "-")
                    inp += f"{var2} = {val}\n"

        return inp

        

class Cylindrical(Source):
    def __init__(self, projectile, energy, spin=(0, 0, 0), mask=(None, 1000),
                 transform=None, weight=1.0, factor=1.0, charge_override=None, fissile=False,
                 center=(0.0, 0.0), bounds=(0.0, 0.0), r_out=0.0, r_in=0.0,
                 elevation=0.0, azimuth=None, dispersion=0.0, **kwargs):
        # elevation is elevation angle in degrees if numeric, set to "all" for isotropic,
        # and an requires an a-type subsection if a function.
        # Similarly, projectile_energy corresponds to e0 if numeric or e-type if a function or it'ble.
        super().__init__(1, projectile, spin=spin, mask=mask, transform=transform, charge_override=charge_override, fissile=fissile, **kwargs)
        self.x0 = center[0]
        self.y0 = center[1]
        self.z0 = bounds[0]
        self.z1 = bounds[1]
        self.r0 = r_out
        self.r1 = r_in
        self.dir = elevation
        self.phi = azimuth
        self.dom = dispersion
        self.e0 = energy

    def definition(self):
        inp = super().__init__().definition()
        params = {k: v for k, v in self.__dict__.items() if k not in super().__dict__}
        for var, val in params:
            if val is not None:
                if isinstance(val, PhitsObject):
                    inp += f"{var} = {val.index}\n"
                else:
                    var2 = var.replace("_", "-")
                    inp += f"{var2} = {val}\n"
        return inp


class Rectangular(Source):
    def __init__(self, projectile, energy, spin=(0, 0, 0), mask=(None, 1000),
                 transform=None, weight=1.0, factor=1.0, charge_override=None, fissile=False,
                 xbound=(0.0, 0.0), ybound=(0.0, 0.0), zbound=(0.0, 0.0),
                 elevation=0.0, azimuth=None, dispersion=0.0, **kwargs):
        super().__init__(2, projectile, spin=spin, mask=mask, transform=transform, charge_override=charge_override, fissile=fissile, **kwargs)
        self.x0 = xbound[0]
        self.x1 = xbound[1]
        self.y0 = ybound[0]
        self.y1 = ybound[1]
        self.z0 = zbound[0]
        self.z1 = zbound[1]
        self.dir = elevation
        self.phi = azimuth
        self.dom = dispersion
        if isinstance(projectile_energy, float):
            self.e0 = projectile_energy
        else:
            self.e0 = projectile_energy

    def definition(self):
        inp = super().definition()
        params = {k: v for k, v in self.__dict__.items() if k not in super().__dict__}
        for var, val in params:
            if val is not None:
                if isinstance(val, PhitsObject):
                    inp += f"{var} = {val.index}\n"
                else:
                    var2 = var.replace("_", "-")
                    inp += f"{var2} = {val}\n"
        return inp

        

class Gaussian(Source):
    def __init__(self, projectile, energy, spin=(0, 0, 0), mask=(None, 1000),
                 transform=None, weight=1.0, factor=1.0, charge_override=None, fissile=False,
                 center=(0.0, 0.0, 0.0), fwhms=(0.0, 0.0, 0.0), elevation=0.0,
                 azimuth=None, dispersion=0.0, **kwargs):
        super().__init__(3, projectile, spin=spin, mask=mask, transform=transform, charge_override=charge_override, fissile=fissile, **kwargs)
        self.x0 = center[0]
        self.y0 = center[1]
        self.z0 = center[2]
        self.x1 = fwhms[0]
        self.y1 = fwhms[1]
        self.z1 = fwhms[2]
        self.dir = elevation
        self.phi = azimuth
        self.dom = dispersion
        self.e0 = projectile_energy

        
    def __init__(self, projectile, energy, spin=(0, 0, 0), mask=(None, 1000),
                 transform=None, weight=1.0, factor=1.0, charge_override=None, fissile=False,
                 center=(0.0, 0.0), fwhm=0.0, zbound=(0.0, 0.0), elevation=0.0,
                 azimuth=None, dispersion=0.0, **kwargs):
        super().__init__(13, projectile, spin=spin, mask=mask, transform=transform, charge_override=charge_override, fissile=fissile, **kwargs)
        self.x0 = center[0]
        self.y0 = center[1]
        self.r1 = fwhm
        self.z0 = zbound[0]
        self.z1 = zbound[1]        
        self.dir = elevation
        self.phi = azimuth,
        self.dom = dispersion
        self.e0 = projectile_energy

    def definition(self):
        inp = super().definition()
        params = {k: v for k, v in self.__dict__.items() if k not in super().__dict__}
        for var, val in params:
            if val is not None:
                if isinstance(val, PhitsObject):
                    inp += f"{var} = {val.index}\n"
                else:
                    var2 = var.replace("_", "-")
                    inp += f"{var2} = {val}\n"
        return inp


class Parabolic(Source):
    # Thanks to lazy typing, xyz or x-y is determined by dimension of center
    def __init__(self, projectile, energy, spin=(0, 0, 0), mask=(None, 1000),
                 transform=None, weight=1.0, factor=1.0, charge_override=None, fissile=False,
                 center=(0.0, 0.0), width=(0.0, 0.0), zbound=(0.0, 0.0),
                 order=2, elevation=0.0, azimuth=None, dispersion=0.0, **kwargs):
        if len(width) == 2:
            super().__init__(7, projectile, spin=spin, mask=mask, transform=transform, charge_override=charge_override, fissile=fissile, **kwargs)
            self.x0 = center[0]
            self.y0 = center[1]
            self.x1 = width[0]
            self.y1 = width[1]
            self.z0 = zbound[0]
            self.z1 = zbound[1]
            self.rn = order
            self.dir = elevation
            self.phi = azimuth
            self.dom = dispersion
            self.e0 = projectile_energy

        else:
            super().__init__(15, projectile, spin=spin, mask=mask, transform=transform, charge_override=charge_override, fissile=fissile, **kwargs)
            self.x0 = center[0]
            self.y0 = center[1]
            self.r1 = width
            self.z0 = zbound[0]
            self.z1 = zbound[1]
            self.rn = order
            self.dir = elevation
            self.phi = azimuth
            self.dom = dispersion
            self.e0 = projectile_energy

    def definition(self):
        inp = super().definition()
        params = {k: v for k, v in self.__dict__.items() if k not in super().__dict__}
        for var, val in params:
            if val is not None:
                if isinstance(val, PhitsObject):
                    inp += f"{var} = {val.index}\n"
                else:
                    var2 = var.replace("_", "-")
                    inp += f"{var2} = {val}\n"
        return inp

        
class Spherical(Source):
    def __init__(self, projectile, energy, spin=(0, 0, 0), mask=(None, 1000),
                 transform=None, weight=1.0, factor=1.0, charge_override=None, fissile=False,
                 center=(0.0,0.0,0.0), r_out=0.0, r_in=0.0,
                 direction="outward", iso_options=None, **kwargs):
        super().__init__(9, projectile, spin=spin, mask=mask, transform=transform, charge_override=charge_override, fissile=fissile, **kwargs)
        self.x0 = center[0]
        self.y0 = center[1]
        self.z0 = center[2]
        self.r1 = r_in
        self.r2 = r_out
        if direction == "outward":
            self.dir = 1.0
        elif direction == "inward":
            self.dir = -1.0
        elif direction == "isotropic":
            self.dir = "all"
        # what tf is -all? "cosine distribution?"
        # what tf is iso?
        if isinstance(projectile_energy, float):
            self.e0 = projectile_energy
        else:
            self.e_type = projectile_energy

class Beam(Source): # I don't understand what this is trying to do
    def __init__(self, center=(0.0, 0.0), xbound=(0.0, 0.0), ybound=(0.0, 0.0), zbound=(0.0,0.0),
                 gradx=0.0, grady=0.0):
        pass
    
class Conical(Source):
    def __init__(self, projectile, energy, spin=(0, 0, 0), mask=(None, 1000),
                 transform=None, weight=1.0, factor=1.0, charge_override=None, fissile=False,
                 top=(0.0, 0.0, 0.0), altitude=(0.0, 0.0, 0.0),
                 trim_bottom=0.0, trim_top=0.0, slope=0.0, elevation=1.0, azimuth=None,
                 dispersion=0.0, **kwargs):
        super().__init__(18, projectile, spin=spin, mask=mask, transform=transform, charge_override=charge_override, fissile=fissile, **kwargs)
        self.x0 = top[0]
        self.y0 = top[1]
        self.z0 = top[2]
        self.x1 = altitude[0]
        self.y1 = altitude[1]
        self.z1 = altitude[2]
        self.r0 = trim_bottom
        self.r1 = trim_top
        self.dir = elevation #TODO: implement "all" and "data" support
        self.phi = azimuth
        self.dom = dispersion
        self.e0 = projectile_energy

    def definition(self):
        inp = super().definition()
        params = {k: v for k, v in self.__dict__.items() if k not in super().__dict__}
        for var, val in params:
            if val is not None:
                if isinstance(val, PhitsObject):
                    inp += f"{var} = {val.index}\n"
                else:
                    var2 = var.replace("_", "-")
                    inp += f"{var2} = {val}\n"
        return inp
            
                
class Prism(Source):
    def __init__(self, projectile, energy, spin=(0, 0, 0), mask=(None, 1000),
                 transform=None, weight=1.0, factor=1.0, charge_override=None, fissile=False,
                 origin=(0.0, 0.0, 0.0), side1=(0.0, 0.0, 0.0),
                 side2=(0.0, 0.0, 0.0), extrusion=(0.0, 0.0, 0.0), attenuation=0.0,
                 elevation=1.0, azimuth=None, dispersion=0.0, **kwargs):

        self.x0 = origin[0]
        self.y0 = origin[1]
        self.z0 = origin[2]
        self.x1 = side1[0]
        self.y1 = side1[1]
        self.z1 = side1[2]
        self.x2 = side2[0]
        self.y2 = side2[1]
        self.z2 = side2[2]
        self.x3 = extrusion[0]
        self.y3 = extrusion[1]
        self.z3 = extrusion[2]
        self.exa = attenuation
        self.dir = elevation
        self.phi = azimuth
        self.dom = dispersion
        self.e0 = projectile_energy

    def definition(self):
        inp = super().definition()
        params = {k: v for k, v in self.__dict__.items() if k not in super().__dict__}
        for var, val in params:
            if val is not None:
                if isinstance(val, PhitsObject):
                    inp += f"{var} = {val.index}\n"
                else:
                    var2 = var.replace("_", "-")
                    inp += f"{var2} = {val}\n"
        return inp

    
    
class Grid(Source): # I'm not sure how the intensity is supposed to work
    def __init__(self, mesh, projectile, energy, spin=(0, 0, 0), mask=(None, 1000),
                 transform=None, weight=1.0, factor=1.0, charge_override=None, fissile=False):
        pass

class Tetrahedral(Source): # TODO: tetrahedral geometry
    def __init__(self, projectile, energy, region, spin=(0, 0, 0), mask=(None, 1000),
                 transform=None, weight=1.0, factor=1.0, charge_override=None, fissile=False,
                 elevation=1.0, azimuth=None, dispersion=0.0, **kwargs):
        super().__init__(24, projectile, spin=spin, mask=mask, transform=transform, charge_override=charge_override, fissile=fissile, **kwargs)
        self.tetreg = region
        self.dir = elevation
        self.phi = azimuth
        self.dom = dispersion
        self.e0 = energy

    def definition(self):
        inp = super().definition()
        params = {k: v for k, v in self.__dict__.items() if k not in super().__dict__}
        for var, val in params:
            if val is not None:
                if isinstance(val, PhitsObject):
                    inp += f"{var} = {val.index}\n"
                else:
                    var2 = var.replace("_", "-")
                    inp += f"{var2} = {val}\n"
        return inp

# class SurfaceSource(Source): # TODO: think about how this can interact with surface numbers, and how
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
