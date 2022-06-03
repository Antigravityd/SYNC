import numpy as np
import Transform
# TODO: get the PhitsBase initializer support
class Surface(PhitsBase): # all surfaces carry symbols, reflectiveness/whiteness, and coordinate transforms.
    def __init__(self, symb, reflective=False, white=False,
                 transform=idTransform):
        self.reflective = reflective
        self.white = white
        self.symb = symb
        self.transform = transform


class Plane(Surface): # Planar surface stored as Ax+By+Cz-D = 0.
    def __init__(self, A, B, C, D, reflective=False, white=False):
        super("P", reflective, white)
        self.A = A
        self.B = B
        self.C = C
        self.D = D

    def __init__(self, parallel, D, reflective=False, white=False):
        self.D = D
        if parallel == "x":
            super("PX", reflective, white)
            self.A = 1.0
            self.B = 0.0
            self.C = 0.0
        elif parallel == "y":
            super("PY", reflective, white)
            self.A = 0.0
            self.B = 1.0
            self.C = 0.0
        elif parallel == "z":
            super("PZ", reflective, white)
            self.A = 0.0
            self.B = 0.0
            self.C = 1.0
        else:
            raise ValueError("First argument to Plane(parallel, D) must be one of \"x\", \"y\", \"z\".")

    def __init__(self, p1, p2, p3, reflective=False, white=False):
        super("P", reflective, white)
        assert len(p1) == len(p2) == len(p3) == 3, \
            "All arguments to Point(p1,p2,p3) must be of length 3"
        r1 = np.array(p1)
        r2 = np.array(p2)
        r3 = np.array(p3)

        n = np.cross(r3-r2, r3-r1) # normal vector to plane

        self.A = n[0]
        self.B = n[1]
        self.C = n[2]
        self.D = np.dot(r3, n)


class Sphere(Surface): # Spherical surface stored as (x-x0)^2+(y-y0)^2+(z-z0)^2-R^2=0
    def __init__(self, R, reflective=False, white=False):
        super("SO", reflective, white)
        self.x0 = self.y0 = self.z0 = 0.0
        self.R = R

    def __init__(self, on, coordinate, R, reflective=False, white=False):
        self.R = R
        if on == "x":
            super("SX", reflective, white)
            self.symb = "SX"
            self.x0 = coordinate
            self.y0 = self.z0 = 0.0
        elif on == "y":
            super("SY", reflective, white)
            self.y0 = coordinate
            self.x0 = self.z0 = 0.0
        elif on == "z":
            super("SZ", reflective, white)
            self.z0 = coordinate
            self.x0 = self.y0 = 0.0
        else:
            raise ValueError("First argument to Sphere(on, coordinate, R) must be one of \"x\", \"y\", \"z\".")

    def __init__(self, x0, y0, z0, R, reflective=False, white=False):
        super("S", reflective, white)
        self.x0 = x0
        self.y0 = y0
        self.z0 = z0
        self.R = R


class Cylinder(Surface):
    def __init__(self, on, R, reflective=False, white=False):
        self.R = R
        if on == "x":
            super("CX", reflective, white)
        elif on == "y":
            super("CY", reflective, white)
        elif on == "z":
            super("CZ", reflective, white)
        else:
            raise ValueError("First argument to Cylinder(on, R) must be one of \"x\", \"y\", \"z\".")

    def __init__(self, parallel, other1, other2, R, reflective=False, white=False):

        self.R = R
        if parallel == "x":
            super("C/X", reflective, white)
            self.y0 = other1
            self.z0 = other2
        elif parallel == "y":
            super("C/Y", reflective, white)
            self.x0 = other1
            self.z0 = other2
        elif parallel == "z":
            super("C/Z", reflective, white)
            self.x0 = other1
            self.y0 = other2
        else:
            raise ValueError("First argument to Sphere(parallel, other1, other2, R) must be one of \"x\", \"y\", \"z\".")


class Cone(Surface):
    def __init__(self, on, coordinate, t, sheet, reflective=False, white=False):

        self.modsqt = abs(t) ** 2

        if sheet == "upper":
            self.k = 1.0
        elif sheet == "lower":
            self.k = -1.0
        else:
            raise ValueError("Last argument to Cone(on, coordinate, t, sheet) must be one of \"upper\", \"lower\"")

        if on == "x":
            super("KX", reflective, white)
            self.x0 = coordinate
        elif on == "y":
            super("KY", reflective, white)
            self.y0 = coordinate
        elif on == "z":
            super("KZ", reflective, white)
            self.z0 = coordinate
        else:
            raise ValueError("First argument to Cone(on, coordinate, t, sheet) must be one of \"x\", \"y\", \"z\".")

    def __init__(self, parallel, other1, other2, R, reflective=False, white=False):
        self.R = R
        if parallel == "x":
            super("C\X", reflective, white)
            self.y0 = other1
            self.z0 = other2
        elif parallel == "y":
            super("C\Y", reflective, white)
            self.x0 = other1
            self.z0 = other2
        elif parallel == "z":
            super("C\Z", reflective, white)
            self.x0 = other1
            self.y0 = other2
        else:
            raise ValueError("First argument to Sphere(parallel, other1, other2, R) must be one of \"x\", \"y\", \"z\".")


class SimpleConic(Surface): # ellipsoid, hyperboloid, or paraboloid parallel to an axis of the form
                   # A(x-x0)^2+B(y-y0)^2+C(z-z0)^2+2D(x-x0)+2E(y-y0)+2F(z-z0)+G = 0
    def __init__(self, A, B, C, D, E, F, G, x0, y0, z0, reflective=False, white=False):
        super("SQ", reflective, white)
        self.A = A
        self.B = B
        self.C = C
        self.D = D
        self.E = E
        self.F = F
        self.G = G
        self.x0 = x0
        self.y0 = y0
        self.z0 = z0


class GeneralConic(Surface): # ellipsoid, hyperboloid, or paraboloid of the form
                    # A(x-x0)^2+B(y-y0)^2+C(z-z0)^2+Dxy+Eyz+Fzx+Gx+Hy+Jz+K = 0
    def __init__(self, A, B, C, D, E, F, G, H, J, K, reflective=False, white=False):
        super("GQ", reflective, white)
        self.A = A
        self.B = B
        self.C = C
        self.E = E
        self.F = F
        self.G = G
        self.H = H
        self.J = J
        self.K = K


class Torus(Surface): # torus parallel to an axis of the form
             # (axisvar - axis0)^2/B^2 + (quadrature(<non-axis displacements>) - A)^2 - 1 = 0
    def __init__(self, parallel x0, y0, z0, A, B, C, reflective=False, white=False):
        if self.parallel == "x":
            super("TX", reflective, white)
        elif self.parallel == "y":
            super("TY", reflective, white)
        elif self.parallel == "z":
            super("TZ", reflective, white)
        else:
            raise ValueError("First argument to Torus(parallel, ...) must be one of \"x\", \"y\", \"z\".")

        self.x0 = x0
        self.y0 = y0
        self.z0 = z0
        self.A = A
        self.B = B
        self.C = C


class Box(Surface): # box formed by three vectors with tails at a given base point, or cross product of 3 intervals,
           # stored in the form x0 y0 z0 Ax Ay Az Bx By Bz Cx Cy Cz
    def __init__(self, base, s1, s2, s3, reflective=False, white=False):
        super("BOX", reflective, white)

        assert len(base) == len(s1) == len(s2) == len(s3) == 3, \
            "All arguments to Box(base,s1,s2,s3) must be of length 3"

        self.x0 = base[0]
        self.y0 = base[1]
        self.z0 = base[2]

        self.Ax = s1[0]
        self.Ay = s1[1]
        self.Az = s1[2]

        self.Bx = s2[0]
        self.By = s2[1]
        self.Bz = s2[2]

        self.Cx = s3[0]
        self.Cy = s3[1]
        self.Cz = s3[2]

    def __init__(self, xbounds, ybounds, zbounds, reflective=False, white=False):
        super("BOX", reflective, white)
        assert len(xbounds) == len(ybounds) == len(zbounds) == 2, \
            "All arguments to Box(xbounds,ybounds,zbounds) must be of length 3"

        self.x0 = xbounds[0]
        self.y0 = ybounds[0]
        self.z0 = ybounds[0]

        self.Ax = xbounds[1]
        self.Ay = ybounds[0]
        self.Az = zbounds[0]

        self.Bx = xbounds[0]
        self.By = ybounds[1]
        self.Bz = zbounds[0]

        self.Cx = xbounds[0]
        self.Cy = ybounds[0]
        self.Cz = zbounds[1]

# TODO: finish out the rest of Table 5.77 for surfaces
