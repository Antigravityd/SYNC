import numpy as np
from base import *
from transform import *

# TODO: Python, for some reason unknown to God or Man, doesn't have method overloading based on number of positional arguments supplied.

class Surface(PhitsObject): # all surfaces carry symbols, reflectiveness/whiteness, and coordinate transforms.
    def __init__(self, symb, reflective=False, white=False,
                 transform=idTransform, **kwargs):
        super().__init__("surface", **kwargs)
        self.reflective = reflective
        self.white = white
        self.symb = symb
        self.transform = transform

    def definition(self):
        boundary = "*" if self.reflective else ("+" if self.white else "")
        inp = f"{boundary}{self.index} {self.transform.index} {self.symbol} "
        return inp


class Plane(Surface): # Planar surface stored as Ax+By+Cz-D = 0.
    def __init__(self, A, B, C, D, reflective=False, white=False, **kwargs):
        super().__init__("P", reflective, white, **kwargs)
        self.A = A
        self.B = B
        self.C = C
        self.D = D

    def __init__(self, parallel, D, reflective=False, white=False, **kwargs):
        self.D = D
        if parallel == "x":
            super().__init__("P", reflective, white, **kwargs)
            self.A = 1.0
            self.B = 0.0
            self.C = 0.0
        elif parallel == "y":
            super().__init__("P", reflective, white, **kwargs)
            self.A = 0.0
            self.B = 1.0
            self.C = 0.0
        elif parallel == "z":
            super().__init__("P", reflective, white, **kwargs)
            self.A = 0.0
            self.B = 0.0
            self.C = 1.0
        else:
            raise ValueError("First argument to Plane(parallel, D) must be one of \"x\", \"y\", \"z\".")

    def __init__(self, p1, p2, p3, reflective=False, white=False, **kwargs):
        super().__init__("P", reflective, white, **kwargs)
        assert len(p1) == len(p2) == len(p3) == 3, \
            "All arguments to Plane(p1,p2,p3) must be of length 3"
        r1 = np.array(p1)
        r2 = np.array(p2)
        r3 = np.array(p3)

        n = np.cross(r3-r2, r3-r1) # normal vector to plane

        self.A = n[0]
        self.B = n[1]
        self.C = n[2]
        self.D = np.dot(r3, n)

    def definition(self):
        inp = super().definition()
        inp += f"{self.A} {self.B} {self.C} {self.D}\n"
        return inp


class Sphere(Surface): # Spherical surface stored as (x-x0)^2+(y-y0)^2+(z-z0)^2-R^2=0
    def __init__(self, R, reflective=False, white=False, **kwargs):
        super().__init__("S", reflective, white, **kwargs)
        self.x0 = self.y0 = self.z0 = 0.0
        self.R = R

# def __init__(self, on, coordinate, R, reflective=False, white=False, **kwargs):
#         self.R = R
#         if on == "x":
#             super().__init__("S", reflective, white, **kwargs)
#             self.x0 = coordinate
#             self.y0 = self.z0 = 0.0
#         elif on == "y":
#             super().__init__("S", reflective, white, **kwargs)
#             self.y0 = coordinate
#             self.x0 = self.z0 = 0.0
#         elif on == "z":
#             super().__init__("S", reflective, white, **kwargs)
#             self.z0 = coordinate
#             self.x0 = self.y0 = 0.0
#         else:
#             raise ValueError("First argument to Sphere(on, coordinate, R) must be one of \"x\", \"y\", \"z\".")

#     def __init__(self, x0, y0, z0, R, reflective=False, white=False, **kwargs):
#         super().__init__("S", reflective, white, **kwargs)
#         self.x0 = x0
#         self.y0 = y0
#         self.z0 = z0
#         self.R = R

    def definition(self):
        inp = super().definition()
        inp += f"{self.x0} {self.y0} {self.z0} {self.R}\n"
        return inp


class Cylinder(Surface):
    def __init__(self, on, R, reflective=False, white=False, **kwargs):
        self.R = R
        if on == "x":
            super().__init__("CX", reflective, white, **kwargs)
        elif on == "y":
            super().__init__("CY", reflective, white, **kwargs)
        elif on == "z":
            super().__init__("CZ", reflective, white, **kwargs)
        else:
            raise ValueError("First argument to Cylinder(on, R) must be one of \"x\", \"y\", \"z\".")

    def __init__(self, parallel, other1, other2, R, reflective=False, white=False, **kwargs):
        self.R = R
        if parallel == "x":
            super().__init__("C/X", reflective, white, **kwargs)
            self.y0 = other1
            self.z0 = other2
        elif parallel == "y":
            super().__init__("C/Y", reflective, white, **kwargs)
            self.x0 = other1
            self.z0 = other2
        elif parallel == "z":
            super().__init__("C/Z", reflective, white, **kwargs)
            self.x0 = other1
            self.y0 = other2
        else:
            raise ValueError("First argument to Sphere(parallel, other1, other2, R) must be one of \"x\", \"y\", \"z\".")

    def definition(self):
        inp = super().definition()
        if hasattr(self, "x0"):
            if hasattr(self, "y0"):
                inp += f"{self.x0} {self.y0}\n"
            else:
                inp += f"{self.x0} {self.z0}\n"
        elif hasattr(self, "y0"):
            inp += f"{self.y0} {self.z0}\n"
        else:
            inp += f"{self.R}\n"
        return inp


class Cone(Surface):
    def __init__(self, on, coordinate, t, sheet, reflective=False, white=False, **kwargs):
        self.modsqt = abs(t) ** 2

        if sheet == "upper":
            self.k = 1.0
        elif sheet == "lower":
            self.k = -1.0
        else:
            raise ValueError("Last argument to Cone(on, coordinate, t, sheet) must be one of \"upper\", \"lower\"")

        if on == "x":
            super().__init__("KX", reflective, white, **kwargs)
            self.x0 = coordinate
        elif on == "y":
            super().__init__("KY", reflective, white, **kwargs)
            self.y0 = coordinate
        elif on == "z":
            super().__init__("KZ", reflective, white, **kwargs)
            self.z0 = coordinate
        else:
            raise ValueError("First argument to Cone(on, coordinate, t, sheet) must be one of \"x\", \"y\", \"z\".")

    def __init__(self, parallel, other1, other2, R, reflective=False, white=False, **kwargs):
        self.R = R
        if parallel == "x":
            super().__init__("C\X", reflective, white, **kwargs)
            self.y0 = other1
            self.z0 = other2
        elif parallel == "y":
            super().__init__("C\Y", reflective, white, **kwargs)
            self.x0 = other1
            self.z0 = other2
        elif parallel == "z":
            super().__init__("C\Z", reflective, white, **kwargs)
            self.x0 = other1
            self.y0 = other2
        else:
            raise ValueError("First argument to Sphere(parallel, other1, other2, R) must be one of \"x\", \"y\", \"z\".")

    def definition(self):
        inp = super().definition()
        if hasattr(self, "x0") and hasattr(self, "y0") and hasattr(self, "z0"):
            inp += f"{self.x0} {self.y0} {self.z0} {self.modsqt} {self.k}\n"
        elif hasattr(self, "x0"):
            inp += f"{self.x0} {self.modsqt} {self.k}\n"
        elif hasattr(self, "y0"):
            inp += f"{self.y0} {self.modsqt} {self.k}\n"
        else:
            inp += f"{self.z0} {self.modsqt} {self.k}\n"
        return inp


class SimpleConic(Surface): # ellipsoid, hyperboloid, or paraboloid parallel to an axis of the form
                   # A(x-x0)^2+B(y-y0)^2+C(z-z0)^2+2D(x-x0)+2E(y-y0)+2F(z-z0)+G = 0
    def __init__(self, A, B, C, D, E, F, G, x0, y0, z0, reflective=False, white=False, **kwargs):
        super().__init__("SQ", reflective, white, **kwargs)
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

    def definition(self):
        inp = super().definition()
        inp += f"{self.A} {self.B} {self.C} {self.D} {self.E} {self.F} {self.G} {self.x0} {self.y0} {self.z0}\n"
        return inp



class GeneralConic(Surface): # ellipsoid, hyperboloid, or paraboloid of the form
                    # A(x-x0)^2+B(y-y0)^2+C(z-z0)^2+Dxy+Eyz+Fzx+Gx+Hy+Jz+K = 0
    def __init__(self, A, B, C, D, E, F, G, H, J, K, reflective=False, white=False, **kwargs):
        super().__init__("GQ", reflective, white, **kwargs)
        self.A = A
        self.B = B
        self.C = C
        self.D = D
        self.E = E
        self.F = F
        self.G = G
        self.H = H
        self.J = J
        self.K = K

    def definition(self):
        inp = super().definition()
        inp += f"{self.A} {self.B} {self.C} {self.D} {self.E} {self.F} {self.G} {self.H} {self.J} {self.K}\n"


class Torus(Surface): # torus parallel to an axis of the form
             # (axisvar - axis0)^2/B^2 + (quadrature(<non-axis displacements>) - A)^2 - 1 = 0
    def __init__(self, parallel, x0, y0, z0, A, B, C, reflective=False, white=False, **kwargs):
        if self.parallel == "x":
            super().__init__("TX", reflective, white, **kwargs)
        elif self.parallel == "y":
            super().__init__("TY", reflective, white, **kwargs)
        elif self.parallel == "z":
            super().__init__("TZ", reflective, white, **kwargs)
        else:
            raise ValueError("First argument to Torus(parallel, ...) must be one of \"x\", \"y\", \"z\".")

        self.x0 = x0
        self.y0 = y0
        self.z0 = z0
        self.A = A
        self.B = B
        self.C = C

    def definition(self):
        inp = super().definition()
        inp += f"{self.x0} {self.y0} {self.z0} {self.A} {self.B} {self.C}\n"
        return inp


class Box(Surface): # box formed by three vectors with tails at a given base point, or cross product of 3 intervals,
           # stored in the form x0 y0 z0 Ax Ay Az Bx By Bz Cx Cy Cz
    def __init__(self, base, s1, s2, s3, reflective=False, white=False, **kwargs):
        super().__init__("BOX", reflective, white, **kwargs)

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

    def __init__(self, xbounds, ybounds, zbounds, reflective=False, white=False, **kwargs):
        super().__init__("BOX", reflective, white, **kwargs)
        assert len(xbounds) == len(ybounds) == len(zbounds) == 2, \
            "All arguments to Box(xbounds,ybounds,zbounds) must be of length 2"

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

    def definition(self):
        inp = super().definition()
        inp += f"{self.x0} {self.y0} {self.z0} {self.Ax} {self.Ay} {self.Az} {self.Bx} {self.By} {self.Bz} {self.Cx} {self.Cy} {self.Cz}\n"
        return inp

class HexagonalPrism(Surface):
    def __init__(self, base, height, A, B, C, reflective=False, white=False, **kwargs):
        super().__init__("HEX", reflective, white, **kwargs)

        assert len(base) == len(height) == len(A) == len(B) == len(c) == 3, \
            "All arguments to HexagonalPrism must be of length 3"

        self.x0 = base[0]
        self.y0 = base[1]
        self.z0 = base[2]

        self.Hx = height[0]
        self.Hy = height[1]
        self.Hz = height[2]

        self.Ax = A[0]
        self.Ay = A[1]
        self.Az = A[2]

        self.Bx = B[0]
        self.By = B[1]
        self.Bz = B[2]

        self.Cx = C[0]
        self.Cy = C[1]
        self.Cz = C[2]

    def definition(self):
        inp = super().definition()
        inp += f"{self.x0} {self.y0} {self.z0} {self.Hx} {self.Hy} {self.Hz} {self.Ax} {self.Ay} {self.Az} {self.Bx} {self.By} {self.Bz} {self.Cx} {self.Cy} {self.Cz}\n"
        return inp

class ElipticalCylinder(Surface):
    def __init__(self, base, height, A, B, reflective=False, white=False, **kwargs):
        super().__init__("REC", reflective, white, **kwargs)

        assert len(base) == len(height) == len(A) == len(B) == len(c) == 3, \
            "All arguments to HexagonalPrism must be of length 3"

        self.x0 = base[0]
        self.y0 = base[1]
        self.z0 = base[2]

        self.Hx = height[0]
        self.Hy = height[1]
        self.Hz = height[2]

        self.Ax = A[0]
        self.Ay = A[1]
        self.Az = A[2]

        self.Bx = B[0]
        self.By = B[1]
        self.Bz = B[2]

    def definition(self):
        inp = super().definition()
        inp += f"{self.x0} {self.y0} {self.z0} {self.Hx} {self.Hy} {self.Hz} {self.Ax} {self.Ay} {self.Az} {self.Bx} {self.By} {self.Bz}\n"
        return inp

class TruncatedCone(Surface):
    def __init__(self, center, height, R_lower, R_upper, reflective=False, white=False, **kwargs):
        super().__init__("TRC", reflective, white, **kwargs)
        assert len(center) == len(height) == 3, "Center and height vectors in TruncatedCone must be of length 3"

        self.x0 = center[0]
        self.y0 = center[1]
        self.z0 = center[2]

        self.Hx = height[0]
        self.Hy = height[1]
        self.Hz = height[2]

        self.R1 = R_lower

        self.R2 = R_upper

    def definition(self):
        inp = super().definition()
        inp += f"{self.x0} {self.y0} {self.z0} {self.Hx} {self.Hy} {self.Hz} {self.R1} {self.R2}\n"
        return inp


class Spheroid(Surface):
    def __init__(self, f1, f2, R, reflective=False, white=False, **kwargs):
        super().__init__("ELL", reflective, white, **kwargs)
        assert len(f1) == len(f2) == 3, \
            "Foci of spheroid must be of length 3"

        self.x1 = f1[0]
        self.y1 = f1[1]
        self.z1 = f1[2]

        self.x2 = f2[0]
        self.y2 = f2[1]
        self.z2 = f2[2]

        self.R = R

    def __init__(self, center, axis, R, reflective=False, white=False, **kwargs):
        super().__init__("ELL", reflective, white, **kwargs)
        assert len(f1) == len(f2) == 3, \
            "Foci of spheroid must be of length 3"

        self.x0 = center[0]
        self.y0 = center[1]
        self.z0 = center[2]

        self.Ax = axis[0]
        self.Ay = axis[1]
        self.Az = axis[2]

        self.R = R

    def definition(self):
        if hasattr(self, "x1"):
            inp = super().definition()
            inp += f"{self.x1} {self.y1} {self.z1} {self.x2} {self.y2} {self.z2} {self.R}\n"
        else:
            inp = super().definition()
            inp += f"{self.x0} {self.y0} {self.z0} {self.Ax} {self.Ay} {self.Az} -{self.R}\n"


class Wedge(Surface):
    def __init__(self, point, side1, side2, height, reflective=False, white=False, **kwargs):
        super().__init__("WED", reflective, white, **kwargs)
        assert len(point) == len(side1) == len(side2) == len(height) == 3, "Arguments to Wedge() must be of length 3"

        self.x0 = point[0]
        self.y0 = point[1]
        self.z0 = point[2]

        self.Ax = side1[0]
        self.Ay = side1[1]
        self.Az = side1[2]

        self.Bx = side2[0]
        self.By = side2[1]
        self.Bz = side2[2]

        self.Hx = height[0]
        self.Hy = height[1]
        self.Hz = height[2]

    def definition(self):
        inp = super().definition()
        inp += f"{self.x0} {self.y0} {self.z0} {self.Ax} {self.Ay} {self.Az} {self.Bx} {self.By} {self.Bz} {self.Hx} {self.Hy} {self.Hz}\n"
