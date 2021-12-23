
class Source:
    def __init__(self, s_type, projectile, spin=(0, 0, 0), mask=(None, 1000),
                 transform=None, different_charge=None):
        self.s_type = s_type
        self.proj = projectile
        self.sx = spin[0]
        self.sy = spin[1]
        self.sz = spin[2]
        self.reg = mask[0]
        self.ntmax = mask[1]
        self.trcl = transform
        self.izst = different_charge
        
        

class Cylindrical:
    def __init__(self, projectile, spin=(0, 0, 0), mask=(None, 1000), transform=idTransform,
                 different_charge=None, center=(0.0, 0.0), bounds=(0.0, 0.0), r_out=0.0, r_in=0.0,
                 elevation=0.0, azimuth=None, dispersion=0.0, projectile_energy):
        # elevation is elevation angle in degrees if numeric, set to "all" for isotropic,
        # and an requires an a-type subsection if a function.
        # Similarly, projectile_energy corresponds to e0 if numeric or e-type if a function or it'ble.
        super().__init__(1, projectile, spin, mask, transform, different_charge)
        self.x0 = center[0]
        self.y0 = center[1]
        self.z0 = bounds[0]
        self.z1 = bounds[1]
        self.r0 = r_out
        self.r1 = r_in
        self.dir = elevation
        self.phi = azimuth
        self.dom = dispersion


class Rectangular:
    def __init__(self, projectile, spin=(0, 0, 0), mask=(None, 1000), transform=idTransform,
                 different_charge=None, xbound=(0.0, 0.0), ybound=(0.0, 0.0), zbound=(0.0, 0,0)
                 elevation=0.0, azimuth=None, dispersion=0.0, projectile_energy):
        super().__init__(2, projectile, spin, mask, transform, different_charge)
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
            self.e_type = projectile_energy

        

class Gaussian:
    def __init__(self, projectile, spin=(0, 0, 0), mask=(None, 1000), transform=idTransform,
                 different_charge=None, center=(0.0, 0.0, 0.0), fwhms=(0.0, 0.0, 0.0), elevation=0.0,
                 azimuth=None, dispersion=0.0, projectile_energy):
        super().__init__(3, projectile, spin, mask, transform, different_charge)
        self.x0 = center[0]
        self.y0 = center[1]
        self.z0 = center[2]
        self.x1 = stdevs[0]
        self.y1 = stdevs[1]
        self.z1 = stdevs[2]
        self.dir = elevation
        self.phi = azimuth
        self.dom = dispersion
        if isinstance(projectile_energy, float):
            self.e0 = projectile_energy
        else:
            self.e_type = projectile_energy

        
    def __init__(self, projectile, spin=(0, 0, 0), mask=(None, 1000), transform=idTransform,
                 different_charge=None, center=(0.0, 0.0), fwhm=0.0, zbound=(0.0, 0.0), elevation=0.0,
                 azimuth=None, dispersion=0.0, projectile_energy):
        super().__init__(13, projectile, spin, mask, transform, different_charge)
        self.x0 = center[0]
        self.y0 = center[1]
        self.r1 = fwhm
        self.z0 = zbound[0]
        self.z1 = zbound[1]        
        self.dir = elevation
        self.phi = azimuth,
        self.dom = dispersion
        if isinstance(projectile_energy, float):
            self.e0 = projectile_energy
        else:
            self.e_type = projectile_energy


class Parabolic:
    # Thanks to lazy typing, xyz or x-y is determined by dimension of center
    def __init__(self, projectile, spin=(0, 0, 0), mask=(None, 1000), transform=idTransform,
                 different_charge=None, center=(0.0, 0.0), width=(0.0, 0.0), zbound=(0.0, 0.0),
                 order=2, elevation=0.0, azimuth=None, dispersion=0.0, projectile_energy):
        if len(width) == 2:
            super().__init__(7, projectile, spin, mask, transform, different_charge)
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
            if isinstance(projectile_energy, float):
                self.e0 = projectile_energy
            else:
                self.e_type = projectile_energy

        else:
            super().__init__(15, projectile, spin, mask, transform, different_charge)
            self.x0 = center[0]
            self.y0 = center[1]
            self.r1 = width
            self.z0 = zbound[0]
            self.z1 = zbound[1]
            self.rn = order
            self.dir = elevation
            self.phi = azimuth
            self.dom = dispersion
            if isinstance(projectile_energy, float):
                self.e0 = projectile_energy
            else:
                self.e_type = projectile_energy

        
class Spherical:
    def __init__(self, projectile, spin=(0, 0, 0), mask=(None, 1000), transform=idTransform,
                 different_charge=None, center=(0.0,0.0,0.0), r_out=0.0, r_in=0.0,
                 direction="outward", iso_options=None, projectile_energy):
        super().__init__(9, projectile, spin, mask, transform, different_charge)
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

class Beam:
    
class Conical:

class Prism:

class Grid:

class Tetrahedral:

class Energy:

class Angular:

class TimeDependent:

class Fissile:



