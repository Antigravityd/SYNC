import numpy as np
from base import *
from transform import *


common = {"reflective": (None, Choice10(), None),
          "white": (None, Choice10(), None),
          "transform": (None, IsA(Transform, index=True), None),
          "inside": (None, Choice10(), None)}


class Plane(PhitsObject):
    """A plane of the form Ax + By + Cz - D = 0."""
    name = "surface"
    syntax = common | {"A": (None, Real(), 0),
                       "B": (None, Real(), 1),
                       "C": (None, Real(), 2),
                       "D": (None, Real(), 3)}

    shape = lambda self: ((f"*{self.index}" if self.reflective else
                           (f"+{self.index}" if self.white else f"{self.index}"),
                           "transform", "P", "A", "B", "C", "D"))


# TODO: consider obliterating the next 2
class PointPlane(PhitsObject):
    """A plane specified by three points."""
    name = "surface"
    syntax = common | {"p1": (None, Tuple(Real(), Real(), Real()), 0),
                       "p2": (None, Tuple(Real(), Real(), Real()), 1),
                       "p3": (None, Tuple(Real(), Real(), Real()), 2)
                       }

    shape = lambda self: ((f"*{self.index}" if self.reflective else
                           (f"+{self.index}" if self.white else f"{self.index}"),
                           "transform", "P",
                           f"{self.p1[0]}", f"{self.p1[1]}", f"{self.p1[2]}",
                           f"{self.p2[0]}", f"{self.p2[1]}", f"{self.p2[2]}",
                           f"{self.p3[0]}", f"{self.p3[1]}", f"{self.p3[2]}"))

class ParallelPlane(PhitsObject):
    """A plane of the form x_i = D."""
    name = "surface"
    syntax = common | {"parallel": (None, FinBij({"x": "X", "y": "Y", "z":"Z"}), 0),
                       "D": (None, Real(), 1)}

    shape = lambda self: ((f"*{self.index}" if self.reflective else
                           (f"+{self.index}" if self.white else f"{self.index}"),
                           "transform", f"P{self.parallel}", "D"))


class Sphere(PhitsObject):
    "A sphere of radius R centered on (x0, y0, z0)."
    name = "surface"
    syntax = common | {"radius": (None, PosReal(), 0),
                       "center": (None, Tuple(Real(), Real(), Real()), 1)}

    shape = lambda self: ((f"*{self.index}" if self.reflective else
                           (f"+{self.index}" if self.white else f"{self.index}"),
                           "transform",
                           "SPH", f"{self.center[0]}", f"{self.center[1]}", f"{self.center[2]}" "R"))


class Cylinder(PhitsObject):
    """A right-circular cylinder with center of the bottom face (x_0, y_0, z_0), height vector from the bottom to top face (H_x, H_y, H_z),
    and radius R."""
    name = "surface"
    syntax = common | {"center": (None, Tuple(Real(), Real(), Real()), 0),
                       "height": (None, Tuple(Real(), Real(), Real()), 1),
                       "radius": (None, PosReal(), 2)}
    shape = lambda self: ((f"*{self.index}" if self.reflective else
                           (f"+{self.index}" if self.white else f"{self.index}"),
                           "transform",
                           "RCC", " ".join(self.center), " ".join(self.height), "radius"))

class Cone(PhitsObject):
    """A truncated right-angle cone with bottom-face center (x_0, y_0, z_0), height vector (H_x, H_y, H_z), and bottom and top radii
    R_1 and R_2 respectively."""
    name = "surface"
    syntax = common | {"center": (None, Tuple(Real(), Real(), Real()), 0),
                       "height": (None, Tuple(Real(), Real(), Real()), 1),
                       "bottom_r": (None, PosReal(), 2),
                       "top_r": (None, PosReal(), 3)}
    shape = lambda self: ((f"*{self.index}" if self.reflective else
                           (f"+{self.index}" if self.white else f"{self.index}"),
                           "transform",
                           "TRC", " ".join(self.center), " ".join(self.height), "bottom_r", "top_r"))


class SimpleConic(PhitsObject): # ellipsoid, hyperboloid, or paraboloid parallel to an axis of the form
                   # A(x-x0)^2+B(y-y0)^2+C(z-z0)^2+2D(x-x0)+2E(y-y0)+2F(z-z0)+G = 0
    name = "surface"
    required = ["A", "B", "C", "D", "E", "F", "G", "center"]
    positional = ["A", "B", "C", "D", "E", "F", "G", "center"]
    optional = ["reflective", "white", "transform", "inside"]
    shape = ((lambda self: f"*{self.index}" if self.reflective else
              (f"+{self.index}" if self.white else f"{self.index}"),
              "transform", "SQ", "A", "B", "C", "D", "E", "F", "G", "center"),)
    def __init__(self, *args, **kwargs):
        super().no_hash.add("inside")



class GeneralConic(PhitsObject): # ellipsoid, hyperboloid, or paraboloid of the form
                    # A(x-x0)^2+B(y-y0)^2+C(z-z0)^2+Dxy+Eyz+Fzx+Gx+Hy+Jz+K = 0
    name = "surface"
    required = ["A", "B", "C", "D", "E", "F", "G", "H", "J", "K"]
    positional = ["A", "B", "C", "D", "E", "F", "G", "H", "J", "K"]
    optional = ["reflective", "white", "transform", "inside"]
    shape = ((lambda self: f"*{self.index}" if self.reflective else
              (f"+{self.index}" if self.white else f"{self.index}"),
              "transform", "GQ", "A", "B", "C", "D", "E", "F", "G", "H", "J", "K"),)
    def __init__(self, *args, **kwargs):
        super().no_hash.add("inside")



class Torus(PhitsObject): # torus parallel to an axis of the form
             # (axisvar - axis0)^2/B^2 + (quadrature(<non-axis displacements>) - A)^2 - 1 = 0
    name = "surface"
    required = ["parallel", "center", "A", "B", "C"]
    positional = ["parallel", "center", "A", "B", "C"]
    optional = ["reflective", "white", "transform", "inside"]
    shape = ((lambda self: f"*{self.index}" if self.reflective else
              (f"+{self.index}" if self.white else f"{self.index}"),
              "transform", lambda self: f"T{self.parallel}", "center", "A", "B", "C"),)
    def __init__(self, *args, **kwargs):
        super().no_hash.add("inside")



class Box(PhitsObject): # box formed by three vectors with tails at a given base point, or cross product of 3 intervals,
           # stored in the form x0 y0 z0 Ax Ay Az Bx By Bz Cx Cy Cz
    name = "surface"
    required = ["base", "s1", "s2", "s3"]
    positional = ["base", "s1", "s2", "s3"]
    optional = ["reflective", "white", "transform", "inside"]
    shape = ((lambda self: f"*{self.index}" if self.reflective else
              (f"+{self.index}" if self.white else f"{self.index}"),
              "transform", "BOX", "base", "s1", "s2", "s3"),)
    def __init__(self, *args, **kwargs):
        super().no_hash.add("inside")


class HexagonalPrism(PhitsObject):
    name = "surface"
    required = ["base", "height", "s1", "s2", "s3"]
    positional = ["base", "height", "s1", "s2", "s3"]
    optional = ["reflective", "white", "transform", "inside"]
    shape = ((lambda self: f"*{self.index}" if self.reflective else
              (f"+{self.index}" if self.white else f"{self.index}"),
              "transform", "HEX", "base", "height", "s1", "s2", "s3"))
    def __init__(self, *args, **kwargs):
        super().no_hash.add("inside")


class ElipticalCylinder(PhitsObject):
    name = "surface"
    required = ["center", "height", "major_axis", "minor_axis"]
    positional = ["center", "height", "major_axis", "minor_axis"]
    optional = ["reflective", "white", "transform", "inside"]
    shape = ((lambda self: f"*{self.index}" if self.reflective else
              (f"+{self.index}" if self.white else f"{self.index}"),
              "transform", "REC", "center", "height", "major_axis", "minor_axis"),)
    def __init__(self, *args, **kwargs):
        super().no_hash.add("inside")



class TruncatedCone(PhitsObject):
    name = "surface"
    required = ["center", "height", "r_top", "r_bottom"]
    positional = ["center", "height", "r_top", "r_bottom"]
    optional = ["reflective", "white", "transform", "inside"]
    shape = ((lambda self: f"*{self.index}" if self.reflective else
              (f"+{self.index}" if self.white else f"{self.index}"),
              "transform", "TRC", "center", "height", "r_top", "r_bottom"))
    def __init__(self, *args, **kwargs):
        super().no_hash.add("inside")



class Spheroid(PhitsObject):
    name = "surface"
    required = ["focus_1", "focus_2" "major_axis"]
    positional = ["focus_1", "focus_2" "major_axis"]
    optional = ["reflective", "white", "transform", "inside"]
    shape = ((lambda self: f"*{self.index}" if self.reflective else
              (f"+{self.index}" if self.white else f"{self.index}"),
              "transform", "ELL", "focus_1", "focus_2", "major_axis"),)
    def __init__(self, *args, **kwargs):
        super().no_hash.add("inside")


class Wedge(PhitsObject):
    name = "surface"
    required = ["tip", "s1", "s2", "height"]
    positional = ["tip", "s1", "s2", "height"]
    optional = ["reflective", "white", "transform", "inside"]
    shape = ((lambda self: f"*{self.index}" if self.reflective else
              (f"+{self.index}" if self.white else f"{self.index}"),
              "transform", "WED", "tip", "s1", "s2", "height"),)
    def __init__(self, *args, **kwargs):
        super().no_hash.add("inside")
