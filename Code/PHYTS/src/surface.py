import numpy as np
from base import *
from transform import *

# TODO: Python, for some reason unknown to God or Man, doesn't have method overloading based on number of positional arguments supplied.

class Surface(PhitsObject): # all surfaces carry symbols, reflectiveness/whiteness, and coordinate transforms.
    def __init__(self, symb, reflective=False, white=False,
                 transform=None, **kwargs):
        super().__init__("surface", required=["symbol"], positional=["symbol"],
                         optional=["reflective", "white", "transform"],
                         shape=((lambda: f"*{self.index}" if self.reflective else
                                 (f"+{self.index}" if self.white else f"{self.index}"), "transform", "symbol"),))
        super().__init__("surface", **kwargs)
        self.reflective = reflective
        self.white = white
        self.symb = symb
        self.transform = transform



class Plane(PhitsObject): # Planar surface stored as Ax+By+Cz-D = 0.
    def __init__(self, *args, **kwargs):
        if len(args) == 4:
            super().__init__("surface", required=["A", "B", "C", "D"], positional=["A", "B", "C", "D"],
                             optional=["reflective", "white", "transform"],
                             shape=((lambda: f"*{self.index}" if self.reflective else
                                     (f"+{self.index}" if self.white else f"{self.index}"),
                                     "transform", "P", "A", "B", "C", "D")), *args, **kwargs)
        elif len(args) == 3:
            super().__init__("surface", required=["p1", "p2", "p3"], positional=["p1", "p2", "p3"],
                             optional=["reflective", "white", "transform"],
                             shape=((lambda: f"*{self.index}" if self.reflective else
                                     (f"+{self.index}" if self.white else f"{self.index}"), "transform", "P", "p1", "p2", "p3"),),
                             *args, **kwargs)
        else:
            super().__init__("surface", required=["parallel", "D"], positional=["parallel", "D"],
                             optional=["reflective", "white", "transform"],
                             shape=((lambda: f"*{self.index}" if self.reflective else
                                     (f"+{self.index}" if self.white else f"{self.index}"),
                                     "transform",
                                     lambda: f"P{self.parallel}", "D")), *args, **kwargs)


class Sphere(PhitsObject): # Spherical surface stored as (x-x0)^2+(y-y0)^2+(z-z0)^2-R^2=0
    def __init__(self, *args, **kwargs):
        if len(args) == 1:
            super().__init__("surface", required=["R"], positional=["R"],
                             optional=["reflective", "white", "transform"],
                             shape=((lambda: f"*{self.index}" if self.reflective else
                                     (f"+{self.index}" if self.white else f"{self.index}"), "transform", "SO", "R"),),
                             *args, **kwargs)
        elif len(args) == 2:
            super().__init__("surface", required=["center", "R"], positional=["center", "R"],
                             optional=["reflective", "white", "transform"],
                             shape=((lambda: f"*{self.index}" if self.reflective else
                                     (f"+{self.index}" if self.white else f"{self.index}"), "transform", "S", "center", "R"),),
                             *args, **kwargs)

        elif len(args) == 3:
            super().__init__("surface", required=["coordinate", "on", "R"], positional=["coordinate", "on" "R"],
                             optional=["reflective", "white", "transform"],
                             shape=((lambda: f"*{self.index}" if self.reflective else
                                     (f"+{self.index}" if self.white else f"{self.index}"),
                                     "transform", lambda: f"S{self.on}", "coordinate" "R"),),
                             *args, **kwargs)


class Cylinder(PhitsObject):
    def __init__(self, *args, **kwargs):
        symb = args[0]
        if len(args) == 2:
                super().__init__("surface", required=["on", "R"], positional=["on", "R"],
                                 optional=["reflective", "white", "transform"],
                                 shape=((lambda: f"*{self.index}" if self.reflective else
                                         (f"+{self.index}" if self.white else f"{self.index}"),
                                         "transform", lambda: f"C{self.on}", "R"),), *args, **kwargs)
        elif len(args) == 3:
                super().__init__("surface", required=["parallel", "center", "R"], positional=["parallel", "center", "R"],
                                 optional=["reflective", "white", "transform"],
                                 shape=((lambda: f"*{self.index}" if self.reflective else
                                         (f"+{self.index}" if self.white else f"{self.index}"),
                                         "transform", lambda: f"C/{self.parallel}", "center", "R"),), *args, **kwargs)


class Cone(PhitsObject):
    def __init__(self, *args, **kwargs):
        if isinstance(args[1], list):
            super().__init__("surface", required=["on", "coordinate", "t_squared", "sheet"],
                             positional=["on","coordinate", "t_squared", "sheet"],
                             optional=["reflective", "white", "transform"],
                             shape=((lambda: f"*{self.index}" if self.reflective else
                                     (f"+{self.index}" if self.white else f"{self.index}"),
                                     "transform", lambda: f"K{self.on}", "coordinate", "t_squared", "sheet"),),
                             value_map={"upper": 1.0, "lower": -1.0}, *args, **kwargs)

        else:
            super().__init__("surface", required=["parallel", "coordinate", "t_squared", "sheet"],
                             positional=["parallel","coordinate", "t_squared", "sheet"],
                             optional=["reflective", "white", "transform"],
                             shape=((lambda: f"*{self.index}" if self.reflective else
                                     (f"+{self.index}" if self.white else f"{self.index}"),
                                     "transform", lambda: f"K/{self.parallel}", "coordinate", "t_squared", "sheet"),),
                             value_map={"upper": 1.0, "lower": -1.0}, *args, **kwargs)


class SimpleConic(PhitsObject): # ellipsoid, hyperboloid, or paraboloid parallel to an axis of the form
                   # A(x-x0)^2+B(y-y0)^2+C(z-z0)^2+2D(x-x0)+2E(y-y0)+2F(z-z0)+G = 0
    def __init__(self, *args, **kwargs):
        if len(args) == 8:
            super().__init__("surface", required=["A", "B", "C", "D", "E", "F", "G", "center"],
                             positional=["A", "B", "C", "D", "E", "F", "G", "center"],
                             optional=["reflective", "white", "transform"],
                             shape=((lambda: f"*{self.index}" if self.reflective else
                                     (f"+{self.index}" if self.white else f"{self.index}"),
                                     "transform", "SQ", "A", "B", "C", "D", "E", "F", "G", "center"),),
                             *args, **kwargs)



class GeneralConic(PhitsObject): # ellipsoid, hyperboloid, or paraboloid of the form
                    # A(x-x0)^2+B(y-y0)^2+C(z-z0)^2+Dxy+Eyz+Fzx+Gx+Hy+Jz+K = 0
    def __init__(self, *args, **kwargs):
            super().__init__("surface", required=["A", "B", "C", "D", "E", "F", "G", "H", "J", "K"],
                             positional=["A", "B", "C", "D", "E", "F", "G", "H", "J", "K"],
                             optional=["reflective", "white", "transform"],
                             shape=((lambda: f"*{self.index}" if self.reflective else
                                     (f"+{self.index}" if self.white else f"{self.index}"),
                                     "transform", "GQ", "A", "B", "C", "D", "E", "F", "G", "H", "J", "K"),),
                             *args, **kwargs)


class Torus(PhitsObject): # torus parallel to an axis of the form
             # (axisvar - axis0)^2/B^2 + (quadrature(<non-axis displacements>) - A)^2 - 1 = 0
    def __init__(self, *args, **kwargs):
        super().__init__("surface", required=["parallel", "center", "A", "B", "C"],
                             positional=["parallel", "center", "A", "B", "C"],
                             optional=["reflective", "white", "transform"],
                             shape=((lambda: f"*{self.index}" if self.reflective else
                                     (f"+{self.index}" if self.white else f"{self.index}"),
                                      "transform", lambda: f"T{self.parallel}", "center", "A", "B", "C"),),
                             *args, **kwargs)



class Box(PhitsObject): # box formed by three vectors with tails at a given base point, or cross product of 3 intervals,
           # stored in the form x0 y0 z0 Ax Ay Az Bx By Bz Cx Cy Cz
    def __init__(self, *args, **kwargs):
        super().__init__("surface", required=["base", "s1", "s2", "s3"],
                             positional=["base", "s1", "s2", "s3"],
                             optional=["reflective", "white", "transform"],
                             shape=((lambda: f"*{self.index}" if self.reflective else
                                     (f"+{self.index}" if self.white else f"{self.index}"),
                                     "transform", "BOX", "base", "s1", "s2", "s3"),),
                             *args, **kwargs)


class HexagonalPrism(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("surface", required=["base", "height", "s1", "s2", "s3"],
                         positional=["base", "height", "s1", "s2", "s3"],
                         optional=["reflective", "white", "transform"],
                         shape=((lambda: f"*{self.index}" if self.reflective else
                                 (f"+{self.index}" if self.white else f"{self.index}"),
                                 "transform", "HEX", "base", "height", "s1", "s2", "s3"),),
                         *args, **kwargs)


class ElipticalCylinder(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("surface", required=["center", "height", "major_axis", "minor_axis"],
                         positional=["center", "height", "major_axis", "minor_axis"],
                         optional=["reflective", "white", "transform"],
                         shape=((lambda: f"*{self.index}" if self.reflective else
                                 (f"+{self.index}" if self.white else f"{self.index}"),
                                 "transform", "REC", "center", "height", "major_axis", "minor_axis"),),
                         *args, **kwargs)



class TruncatedCone(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("surface", required=["center", "height", "r_top", "r_bottom"],
                         positional=["center", "height", "r_top", "r_bottom"],
                         optional=["reflective", "white", "transform"],
                         shape=((lambda: f"*{self.index}" if self.reflective else
                                 (f"+{self.index}" if self.white else f"{self.index}"),
                                 "transform", "TRC", "center", "height", "r_top", "r_bottom"),),
                         *args, **kwargs)



class Spheroid(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("surface", required=["focus_1", "focus_2" "major_axis"],
                         positional=["focus_1", "focus_2" "major_axis"],
                         optional=["reflective", "white", "transform"],
                         shape=((lambda: f"*{self.index}" if self.reflective else
                                 (f"+{self.index}" if self.white else f"{self.index}"),
                                 "transform", "ELL", "focus_1", "focus_2", "major_axis"),),
                         *args, **kwargs)


class Wedge(PhitsObject):
    def __init__(self, point, side1, side2, height, reflective=False, white=False, **kwargs):
        super().__init__("surface", required=["tip", "s1", "s2", "height"],
                         positional=["tip", "s1", "s2", "height"],
                         optional=["reflective", "white", "transform"],
                         shape=((lambda: f"*{self.index}" if self.reflective else
                                 (f"+{self.index}" if self.white else f"{self.index}"),
                                 "transform", "WED", "tip", "s1", "s2", "height"),),
                         *args, **kwargs)
